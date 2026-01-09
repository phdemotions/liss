# =============================================================================
# dictionary_builder.R
# =============================================================================
# This file defines the rules for variable naming and provenance.
# Any change here affects all downstream datasets and analyses.
# Edit only with explicit intent.
#
# Goal:
# - Build a dictionary (old -> new + measurement_label) from an SPSS .sav
# - Enforce a strict naming ontology: construct[_facet][_target]__metric
# - Apply the dictionary to rename columns while preserving provenance (labels/attrs)
#
# Design notes:
# - "suggestions" are scaffolding only; rely on overrides + human curation for truth.
# - Fail-fast checks prevent silent drift and broken merges.
# =============================================================================

# ---- Dependencies (package-friendly) ----
# In a package, put these in DESCRIPTION/Imports and remove the checks.
.require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required packages: ", paste(missing, collapse = ", "),
      "\nInstall them and try again."
    )
  }
  invisible(TRUE)
}

.require_pkgs(c("haven", "dplyr", "tibble", "purrr", "stringr", "rlang", "janitor"))

# =============================================================================
# 1) Ontology: allowed metrics + validator
# =============================================================================

liss_allowed_metrics <- c(
  "binary", "count", "mean", "sum", "index", "percent", "percentile", "likert", "z", "raw"
)

liss_ontology_regex <- paste0(
  "^",
  "[a-z]+",            # construct start
  "(_[a-z]+)*",        # optional snake_case extensions (facet/target etc.)
  "__",
  "(",
  paste(liss_allowed_metrics, collapse = "|"),
  ")",
  "$"
)

liss_validate_new_names <- function(new_names) {
  new_names <- as.character(new_names)

  bad <- new_names[!stringr::str_detect(new_names, liss_ontology_regex)]
  if (length(bad) > 0) {
    stop(
      "Invalid new variable names (must match construct[_facet][_target]__metric).\n",
      "Bad names:\n", paste(unique(bad), collapse = "\n")
    )
  }
  TRUE
}

# Optional: enforce "no forbidden substrings" policy (analysis-role leakage, wave codes)
liss_forbidden_patterns <- c(
  "\\biv\\b", "\\bdv\\b", "mediator", "moderator", "treatment", "control",
  "\\bwave\\b", "\\bw[0-9]+\\b", "\\byf[0-9]+\\b", "\\bq[0-9]+\\b",
  "efa", "cfa", "alpha"
)

liss_validate_no_forbidden_tokens <- function(new_names) {
  new_names <- as.character(new_names)

  hits <- purrr::map_lgl(
    new_names,
    ~ any(stringr::str_detect(.x, liss_forbidden_patterns))
  )

  if (any(hits)) {
    stop(
      "New names contain forbidden tokens (analysis roles, wave codes, methods, etc.).\n",
      "Bad names:\n", paste(unique(new_names[hits]), collapse = "\n")
    )
  }
  TRUE
}

# =============================================================================
# 2) Extract SPSS metadata from .sav
# =============================================================================

liss_extract_sav_meta <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)

  df <- haven::read_sav(path)

  meta <- tibble::tibble(old = names(df)) %>%
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
    stringr::str_detect(l, "\\blikert\\b|\\bstrongly\\s+agree\\b|\\bstrongly\\s+disagree\\b") ~ "likert",
    TRUE ~ "raw"
  )
}

liss_slugify_constructish <- function(x, max_words = 4) {
  if (is.na(x) || identical(x, "")) return(NA_character_)

  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\([^\\)]*\\)", " ") %>%   # remove (...) text
    stringr::str_replace_all("\\[[^\\]]*\\]", " ") %>%   # remove [...] text
    stringr::str_replace_all("[^a-z0-9\\s]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_remove_all("\\b(the|a|an|of|to|for|and|or|in|on|at|with|about)\\b") %>%
    stringr::str_squish() %>%
    stringr::word(1, max_words) %>%
    stringr::str_replace_all("\\s+", "_") %>%
    janitor::make_clean_names()
}

liss_suggest_new_name <- function(old, label_raw) {
  # Intentionally conservative and "dumb": avoids inventing construct semantics.
  metric <- liss_suggest_metric_from_label(label_raw)
  base   <- liss_slugify_constructish(label_raw)

  if (is.na(base) || identical(base, "")) return(NA_character_)
  paste0(base, "__", metric)
}

# =============================================================================
# 4) Build dictionary (module-aware, override-aware, fail-fast)
# =============================================================================

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

  dict <- meta %>%
    dplyr::mutate(
      module = as.character(module_id),
      new_suggested = purrr::map2_chr(.data$old, .data$label_raw, liss_suggest_new_name),
      measurement_label = dplyr::coalesce(.data$label_raw, .data$old)
    ) %>%
    dplyr::select(
      .data$module, .data$old, .data$new_suggested, .data$measurement_label,
      .data$label_raw, .data$class
    )

  if (!is.null(overrides)) {
    if (!all(c("old", "new") %in% names(overrides))) {
      stop("overrides must include columns: old, new (and optionally measurement_label).")
    }

    overrides2 <- overrides %>%
      dplyr::transmute(
        old = as.character(.data$old),
        new_override = as.character(.data$new),
        ml_override = dplyr::if_else(
          "measurement_label" %in% names(overrides),
          as.character(overrides$measurement_label),
          NA_character_
        )
      )

    dict <- dict %>%
      dplyr::left_join(overrides2, by = "old") %>%
      dplyr::mutate(
        new = dplyr::coalesce(.data$new_override, .data$new_suggested),
        measurement_label = dplyr::coalesce(.data$ml_override, .data$measurement_label)
      ) %>%
      dplyr::select(-dplyr::any_of(c("new_override", "ml_override")))
  } else {
    dict <- dict %>% dplyr::mutate(new = .data$new_suggested)
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

liss_rename_with_dictionary <- function(
    df,
    dict,
    keep_old = c("labels", "attribute", "both", "none"),
    keep_unmapped = TRUE        # if FALSE, drops columns not present in dict$old
) {
  keep_old <- match.arg(keep_old)

  req <- c("old", "new", "measurement_label")
  if (!all(req %in% names(dict))) stop("Dictionary must contain: ", paste(req, collapse = ", "))

  dict2 <- dict %>%
    dplyr::transmute(
      old = as.character(.data$old),
      new = as.character(.data$new),
      measurement_label = as.character(.data$measurement_label),
      module = dplyr::if_else("module" %in% names(dict), as.character(dict$module), NA_character_)
    )

  # Only apply rows with non-missing new names
  dict_apply <- dict2 %>% dplyr::filter(!is.na(.data$new))

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
  if (keep_old %in% c("attribute", "both")) {
    attr(df_new, "name_dictionary") <- dict2
    attr(df_new, "name_dictionary_timestamp") <- as.character(Sys.time())
  }

  # Preserve provenance as variable labels (best for human inspection)
  if (keep_old %in% c("labels", "both")) {
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
#     "nomem_encr2", "person_id_encrypted__raw",             "Encrypted household member identifier (merge key)",
#     "yf24a_m",     "fieldwork_year_month__raw",            "Fieldwork period (YYYYMM)",
#     "yf24a010",    "hire_likelihood_self__percent",        "Likelihood of being hired for target job (0–100)",
#     "yf24a011",    "performance_expectation_self__percentile","Expected performance vs others in same job (0–100)"
#   ),
#   fail_on_na_new = FALSE
# )
#
# df_named <- liss_rename_with_dictionary(built$df, built$dict, keep_old = "both")
# drift <- liss_dictionary_drift_report("/mnt/data/yf24a_EN_1.0p.sav", built$dict)
