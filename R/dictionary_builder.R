# =============================================================================
# dictionary_builder.R
# =============================================================================
# This file defines the rules for variable naming and provenance.
# Any change here affects all downstream datasets and analyses.
# Edit only with explicit intent.
#
# Goal:
# - Build a dictionary (old -> new + measurement_label) from an SPSS .sav
# - Enforce a strict naming ontology: study_construct[_facet][_target][_item][_wave][_metric]
# - Apply the dictionary to rename columns while preserving provenance (labels/attrs)
#
# Design notes:
# - "suggestions" are scaffolding only; rely on overrides + human curation for truth.
# - Fail-fast checks prevent silent drift and broken merges.
# =============================================================================

# =============================================================================
# 1) Ontology: allowed metrics + validator
# =============================================================================

liss_allowed_metrics <- c(
  "raw", "mean", "sum", "index", "z", "binary", "count", "percent", "percentile"
)

liss_forbidden_tokens <- c(
  "dv", "iv", "mediator", "moderator", "control", "treatment",
  "efa", "cfa", "alpha", "question", "item", "scale", "score", "wave"
)

liss_forbidden_token_patterns <- c(
  "^q\\d+$", "^block\\d+$", "^page\\d+$", "^factor\\d+$", "^component\\d+$"
)

liss_is_valid_name_token <- function(token) {
  stringr::str_detect(token, "^[a-z][a-z0-9]*$")
}

liss_is_valid_new_name <- function(name) {
  if (!stringr::str_detect(name, "^[a-z][a-z0-9_]*$")) return(FALSE)
  if (stringr::str_detect(name, "__")) return(FALSE)
  if (stringr::str_detect(name, "^_|_$")) return(FALSE)
  if (nchar(name) > 50) return(FALSE)

  parts <- strsplit(name, "_", fixed = TRUE)[[1]]
  if (length(parts) < 2 || length(parts) > 7) return(FALSE)

  if (!liss_is_valid_name_token(parts[1])) return(FALSE)

  metric <- parts[length(parts)]
  has_metric <- metric %in% liss_allowed_metrics

  item_idx <- which(stringr::str_detect(parts, "^i\\d{2,3}$"))
  wave_idx <- which(stringr::str_detect(parts, "^w\\d{2}$"))

  if (length(item_idx) > 1 || length(wave_idx) > 1) return(FALSE)
  if (has_metric && length(parts) >= 2 && metric != parts[length(parts)]) return(FALSE)

  if (has_metric && any(parts[-length(parts)] %in% liss_allowed_metrics)) return(FALSE)

  if (length(wave_idx) == 1) {
    if (has_metric && wave_idx != (length(parts) - 1)) return(FALSE)
    if (!has_metric && wave_idx != length(parts)) return(FALSE)
  }

  if (length(item_idx) == 1) {
    if (length(wave_idx) == 1 && item_idx > wave_idx) return(FALSE)
    if (length(wave_idx) == 0 && has_metric && item_idx != (length(parts) - 1)) return(FALSE)
    if (length(wave_idx) == 0 && !has_metric && item_idx != length(parts)) return(FALSE)
  }

  if (!liss_is_valid_name_token(parts[2])) return(FALSE)
  if (parts[2] %in% liss_allowed_metrics) return(FALSE)
  if (stringr::str_detect(parts[2], "^i\\d{2,3}$|^w\\d{2}$")) return(FALSE)

  for (part in parts) {
    if (part %in% liss_allowed_metrics) next
    if (stringr::str_detect(part, "^i\\d{2,3}$")) next
    if (stringr::str_detect(part, "^w\\d{2}$")) next
    if (!liss_is_valid_name_token(part)) return(FALSE)
  }

  TRUE
}

#' Validate new names against the LISS ontology
#'
#' @param new_names Character vector of candidate variable names.
#'
#' @return TRUE when all names match the ontology; otherwise errors.
#' @export
#'
#' @examples
#' liss_validate_new_names("yf24a_wellbeing_mean")
#' \dontrun{
#' liss_validate_new_names("wellbeing_mean")
#' }
liss_validate_new_names <- function(new_names) {
  new_names <- as.character(new_names)

  bad <- new_names[!purrr::map_lgl(new_names, liss_is_valid_new_name)]
  if (length(bad) > 0) {
    stop(
      "Invalid new variable names (must match study_construct[_facet][_target][_item][_wave][_metric]).\n",
      "Bad names:\n", paste(unique(bad), collapse = "\n")
    )
  }
  TRUE
}

#' Validate that names contain no forbidden tokens
#'
#' @param new_names Character vector of candidate variable names.
#'
#' @return TRUE when all names pass; otherwise errors.
#' @export
#'
#' @examples
#' \dontrun{
#' liss_validate_no_forbidden_tokens("yf24a_treatment_binary")
#' }
liss_validate_no_forbidden_tokens <- function(new_names) {
  new_names <- as.character(new_names)

  hits <- purrr::map_lgl(new_names, function(name) {
    tokens <- strsplit(name, "_", fixed = TRUE)[[1]]
    if (any(tokens %in% liss_forbidden_tokens)) return(TRUE)
    if (any(purrr::map_lgl(tokens, ~ any(stringr::str_detect(.x, liss_forbidden_token_patterns))))) {
      return(TRUE)
    }
    FALSE
  })

  if (any(hits)) {
    stop(
      "New names contain forbidden tokens (analysis roles, wave codes, methods, etc.).\n",
      "Bad names:\n", paste(unique(new_names[hits]), collapse = "\n")
    )
  }
  TRUE
}

#' Validate a dictionary for ontology and uniqueness
#'
#' @param dict Dictionary tibble with at least `old` and `new` columns.
#' @param strict Logical; if TRUE, disallow NA values in `new`.
#'
#' @return TRUE when dictionary passes all checks; otherwise errors.
liss_validate_dictionary <- function(dict, strict = FALSE) {
  req <- c("old", "new")
  if (!all(req %in% names(dict))) {
    stop("Dictionary must contain columns: ", paste(req, collapse = ", "))
  }

  if (anyDuplicated(dict$old) > 0) stop("Dictionary has duplicated 'old' names.")
  if (anyDuplicated(dict$new) > 0) stop("Dictionary has duplicated 'new' names.")

  if (strict && any(is.na(dict$new))) {
    stop("Dictionary has missing 'new' names while strict = TRUE.")
  }

  non_missing_new <- dict$new[!is.na(dict$new)]
  liss_validate_new_names(non_missing_new)
  liss_validate_no_forbidden_tokens(non_missing_new)

  TRUE
}

# =============================================================================
# 2) Extract SPSS metadata from .sav
# =============================================================================

liss_extract_sav_meta <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)

  df <- haven::read_sav(path)

  meta <- tibble::tibble(old = names(df)) |>
    dplyr::mutate(
      label_raw = purrr::map_chr(.data$old, ~{
        lbl <- haven::var_label(df[[.x]])
        if (is.null(lbl) || identical(lbl, "")) NA_character_ else as.character(lbl)
      }),
      class = purrr::map_chr(.data$old, ~ paste(class(df[[.x]]), collapse = ","))
    )

  list(df = df, meta = meta)
}

# =============================================================================
# 3) Conservative name suggestion (SCAFFOLDING ONLY)
# =============================================================================

liss_suggest_metric_from_label <- function(lbl) {
  if (is.na(lbl)) return(NA_character_)
  l <- stringr::str_to_lower(lbl)

  dplyr::case_when(
    stringr::str_detect(l, "\\bpercentile\\b") ~ "percentile",
    stringr::str_detect(l, "\\b0\\s*[-–]\\s*100\\b|\\b0\\s*to\\s*100\\b|\\bpercent\\b|\\bpercentage\\b") ~ "percent",
    stringr::str_detect(l, "\\byes\\b\\s*/\\s*\\bno\\b|\\btrue\\b\\s*/\\s*\\bfalse\\b|\\bbinary\\b") ~ "binary",
    stringr::str_detect(l, "\\bhow many\\b|\\bnumber of\\b|\\bcount\\b|\\btimes\\b") ~ "count",
    TRUE ~ "raw"
  )
}

liss_slugify_constructish <- function(x, max_words = 4) {
  if (is.na(x) || identical(x, "")) return(NA_character_)

  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\([^\\)]*\\)", " ") |>
    stringr::str_replace_all("\\[[^\\]]*\\]", " ") |>
    stringr::str_replace_all("[^a-z0-9\\s]", " ") |>
    stringr::str_squish() |>
    stringr::str_remove_all("\\b(the|a|an|of|to|for|and|or|in|on|at|with|about)\\b") |>
    stringr::str_squish() |>
    stringr::word(1, max_words) |>
    stringr::str_replace_all("\\s+", "_") |>
    janitor::make_clean_names()
}

liss_suggest_new_name <- function(old, label_raw, module_id) {
  # Intentionally conservative and "dumb": avoids inventing construct semantics.
  metric <- liss_suggest_metric_from_label(label_raw)
  base   <- liss_slugify_constructish(label_raw)

  if (is.na(base) || identical(base, "")) return(NA_character_)
  paste(module_id, base, metric, sep = "_")
}

# =============================================================================
# 4) Build dictionary (module-aware, override-aware, fail-fast)
# =============================================================================

#' Build a dictionary from a .sav file
#'
#' @param sav_path Path to the SPSS .sav file.
#' @param module_id Module identifier (e.g., \"yf24a\").
#' @param required_old Optional character vector of required raw variables.
#' @param overrides Optional tibble with columns `old`, `new`, and optionally
#'   `measurement_label` to override suggestions.
#' @param fail_on_na_new Logical; if TRUE, error when any `new` is NA.
#' @param enforce_forbidden_tokens Logical; if TRUE, enforce forbidden-token rules.
#'
#' @return A list with the raw data frame and the dictionary tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' built <- liss_build_dictionary(\"path/to/file.sav\", module_id = \"yf24a\")
#' }
liss_build_dictionary <- function(
    sav_path,
    module_id,
    required_old = NULL,
    overrides = NULL,          # tibble(old, new, measurement_label) overrides suggested values
    fail_on_na_new = FALSE,    # set TRUE once your overrides cover everything you care about
    enforce_forbidden_tokens = TRUE
) {
  if (missing(module_id) || is.null(module_id) || identical(module_id, "")) {
    stop("module_id is required (e.g., 'yf24a').")
  }

  out <- liss_extract_sav_meta(sav_path)
  meta <- out$meta

  dict <- meta |>
    dplyr::mutate(
      module = as.character(module_id),
      new_suggested = purrr::pmap_chr(
        list(.data$old, .data$label_raw, .data$module),
        liss_suggest_new_name
      ),
      measurement_label = dplyr::coalesce(.data$label_raw, .data$old)
    ) |>
    dplyr::select(
      .data$module, .data$old, .data$new_suggested, .data$measurement_label,
      .data$label_raw, .data$class
    )

  if (!is.null(overrides)) {
    if (!all(c("old", "new") %in% names(overrides))) {
      stop("overrides must include columns: old, new (and optionally measurement_label).")
    }

    overrides2 <- overrides |>
      dplyr::transmute(
        old = as.character(.data$old),
        new_override = as.character(.data$new),
        ml_override = dplyr::if_else(
          "measurement_label" %in% names(overrides),
          as.character(overrides$measurement_label),
          NA_character_
        )
      )

    dict <- dict |>
      dplyr::left_join(overrides2, by = "old") |>
      dplyr::mutate(
        new = dplyr::coalesce(.data$new_override, .data$new_suggested),
        measurement_label = dplyr::coalesce(.data$ml_override, .data$measurement_label)
      ) |>
      dplyr::select(-dplyr::any_of(c("new_override", "ml_override")))
  } else {
    dict <- dict |>
      dplyr::mutate(new = .data$new_suggested)
  }

  # ---- Integrity checks ----
  if (!is.null(required_old)) {
    missing_old <- setdiff(required_old, dict$old)
    if (length(missing_old) > 0) {
      stop("Missing required old variables in .sav: ", paste(missing_old, collapse = ", "))
    }
  }

  if (anyDuplicated(dict$old) > 0) stop("Dictionary has duplicated 'old' names.")

  if (fail_on_na_new && any(is.na(dict$new))) {
    stop(
      "New names are NA for these variables (add overrides or improve labels):\n",
      paste(dict$old[is.na(dict$new)], collapse = "\n")
    )
  }

  dup_new <- dict$new[!is.na(dict$new) & duplicated(dict$new)]
  if (length(dup_new) > 0) {
    stop(
      "Duplicate new names detected (must be unique):\n",
      paste(unique(dup_new), collapse = "\n")
    )
  }

  # Enforce ontology only on non-missing names (during skeleton phase, NAs are allowed)
  non_missing_new <- dict$new[!is.na(dict$new)]
  liss_validate_new_names(non_missing_new)
  if (enforce_forbidden_tokens) liss_validate_no_forbidden_tokens(non_missing_new)

  list(df = out$df, dict = dict)
}

# =============================================================================
# 5) Apply dictionary: rename + preserve provenance (labels, attrs, or both)
# =============================================================================

#' Rename variables with a dictionary and preserve provenance
#'
#' @param df Data frame to rename.
#' @param dict Dictionary with columns `old`, `new`, and `measurement_label`.
#' @param keep_provenance Logical; if TRUE, stores dictionary attributes and
#'   variable labels derived from the dictionary.
#' @param keep_unmapped Logical; if FALSE, drops columns not present in `dict$old`.
#'
#' @return Renamed data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' built <- liss_build_dictionary("path/to/file.sav", module_id = "yf24a")
#' df_named <- liss_rename_with_dictionary(built$df, built$dict)
#' }
liss_rename_with_dictionary <- function(
    df,
    dict,
    keep_provenance = TRUE,
    keep_unmapped = TRUE        # if FALSE, drops columns not present in dict$old
) {

  req <- c("old", "new", "measurement_label")
  if (!all(req %in% names(dict))) stop("Dictionary must contain: ", paste(req, collapse = ", "))

  dict2 <- dict |>
    dplyr::transmute(
      old = as.character(.data$old),
      new = as.character(.data$new),
      measurement_label = as.character(.data$measurement_label),
      module = dplyr::if_else("module" %in% names(dict), as.character(dict$module), NA_character_)
    )

  # Only apply rows with non-missing new names
  dict_apply <- dict2 |>
    dplyr::filter(!is.na(.data$new))

  missing_old <- setdiff(dict_apply$old, names(df))
  if (length(missing_old) > 0) {
    stop("Dictionary 'old' not in df: ", paste(missing_old, collapse = ", "))
  }

  if (anyDuplicated(dict_apply$old) > 0) stop("Dictionary has duplicated 'old' (after filtering NA new).")
  if (anyDuplicated(dict_apply$new) > 0) stop("Dictionary has duplicated 'new' (after filtering NA new).")

  liss_validate_new_names(dict_apply$new)
  liss_validate_no_forbidden_tokens(dict_apply$new)

  if (!keep_unmapped) {
    df <- df[, intersect(names(df), dict_apply$old), drop = FALSE]
  }

  rename_quos <- rlang::set_names(rlang::syms(dict_apply$old), dict_apply$new)
  df_new <- dplyr::rename(df, !!!rename_quos)

  # Preserve provenance as attributes
  if (isTRUE(keep_provenance)) {
    attr(df_new, "name_dictionary") <- dict2
    attr(df_new, "name_dictionary_timestamp") <- as.character(Sys.time())
  }

  # Preserve provenance as variable labels (best for human inspection)
  if (isTRUE(keep_provenance)) {
    for (i in seq_len(nrow(dict_apply))) {
      new_nm <- dict_apply$new[i]
      old_nm <- dict_apply$old[i]
      lbl    <- dict_apply$measurement_label[i]
      haven::var_label(df_new[[new_nm]]) <- paste0(lbl, " [orig: ", old_nm, "]")
    }
  }

  df_new
}

# =============================================================================
# 6) Drift report: compare a .sav against an existing dictionary
# =============================================================================

#' Report dictionary drift against a new .sav file
#'
#' @param sav_path Path to a .sav file.
#' @param dict_existing Existing dictionary with `old` and `new` columns.
#'
#' @return A tibble listing variables in the sav but not the dictionary, and
#'   vice versa.
#' @export
#'
#' @examples
#' \dontrun{
#' drift <- liss_dictionary_drift_report("path/to/file.sav", dict_existing)
#' }
liss_dictionary_drift_report <- function(sav_path, dict_existing) {
  out <- liss_extract_sav_meta(sav_path)
  old_in_sav <- out$meta$old

  req <- c("old", "new")
  if (!all(req %in% names(dict_existing))) stop("dict_existing must contain: old, new")

  old_in_dict <- as.character(dict_existing$old)

  tibble::tibble(
    in_sav_not_in_dict = list(setdiff(old_in_sav, old_in_dict)),
    in_dict_not_in_sav = list(setdiff(old_in_dict, old_in_sav))
  )
}

# =============================================================================
# Example (remove from package code; keep as vignette or tests)
# =============================================================================
# built <- liss_build_dictionary(
#   sav_path  = "/mnt/data/yf24a_EN_1.0p.sav",
#   module_id = "yf24a",
#   overrides = tibble::tribble(
#     ~old,         ~new,                                   ~measurement_label,
#     "nomem_encr2", "yf24a_person_id_encrypted_raw",             "Encrypted household member identifier (merge key)",
#     "yf24a_m",     "yf24a_fieldwork_year_month_raw",            "Fieldwork period (YYYYMM)",
#     "yf24a010",    "yf24a_hire_likelihood_self_percent",        "Likelihood of being hired for target job (0–100)",
#     "yf24a011",    "yf24a_performance_expectation_self_percentile","Expected performance vs others in same job (0–100)"
#   ),
#   fail_on_na_new = FALSE
# )
#
# df_named <- liss_rename_with_dictionary(built$df, built$dict, keep_provenance = TRUE)
# drift <- liss_dictionary_drift_report("/mnt/data/yf24a_EN_1.0p.sav", built$dict)
