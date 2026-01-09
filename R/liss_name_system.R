# =============================================================================
# liss_name_system.R
# =============================================================================
# Canonical naming ontology + dictionary builder + renaming helpers
# =============================================================================

#' Return the default registry path
#'
#' @return A file path to the packaged name registry CSV.
#' @export
liss_registry_path <- function() {
  system.file("extdata", "liss_name_registry.csv", package = "liss")
}

#' Read the LISS name registry
#'
#' @param registry_path Optional path to a registry CSV.
#'
#' @return A tibble with registry rows.
#' @export
liss_read_registry <- function(registry_path = liss_registry_path()) {
  if (is.null(registry_path) || identical(registry_path, "") || !file.exists(registry_path)) {
    return(tibble::tibble(
      study_id = character(),
      old_name = character(),
      new_name = character(),
      construct = character(),
      facet = character(),
      item_number = integer(),
      wave = integer(),
      decision_status = character(),
      decided_by = character(),
      decided_on = character(),
      rationale = character()
    ))
  }

  raw <- utils::read.csv(registry_path, stringsAsFactors = FALSE, check.names = FALSE)
  tibble::as_tibble(raw)
}

#' Write a LISS name registry
#'
#' @param registry A tibble of registry entries.
#' @param registry_path Output path for the CSV.
#'
#' @return The registry path invisibly.
#' @export
liss_write_registry <- function(registry, registry_path = liss_registry_path()) {
  if (is.null(registry_path) || identical(registry_path, "")) {
    stop("registry_path must be a non-empty file path.")
  }
  utils::write.csv(registry, registry_path, row.names = FALSE, na = "")
  invisible(registry_path)
}

#' Validate canonical LISS names
#'
#' @param names Character vector of candidate names.
#'
#' @return TRUE when names are valid; otherwise errors.
#' @export
liss_validate_canonical_names <- function(names) {
  names <- as.character(names)
  if (any(is.na(names))) stop("Canonical names cannot be NA.")

  bad_ascii <- names[stringr::str_detect(names, "[^\\x00-\\x7F]")]
  if (length(bad_ascii) > 0) {
    stop("Canonical names must be ASCII. Bad names:\n", paste(unique(bad_ascii), collapse = "\n"))
  }

  bad_len <- names[nchar(names) > 50]
  if (length(bad_len) > 0) {
    stop("Canonical names must be <= 50 characters. Bad names:\n", paste(unique(bad_len), collapse = "\n"))
  }

  bad_snake <- names[!stringr::str_detect(names, "^[a-z0-9]+(_[a-z0-9]+)*$")]
  if (length(bad_snake) > 0) {
    stop("Canonical names must be snake_case. Bad names:\n", paste(unique(bad_snake), collapse = "\n"))
  }

  too_short <- names[stringr::str_count(names, "_") < 1]
  if (length(too_short) > 0) {
    stop("Canonical names must include at least study + construct. Bad names:\n",
         paste(unique(too_short), collapse = "\n"))
  }

  bad_item <- names[stringr::str_detect(names, "_i\\d{2}") &
                      !stringr::str_detect(names, "_i\\d{2}(_w\\d{2})?(_v[a-z0-9]+)?$")]
  if (length(bad_item) > 0) {
    stop("Item segments must be formatted as _iXX (optionally followed by wave/version). Bad names:\n",
         paste(unique(bad_item), collapse = "\n"))
  }

  bad_wave <- names[stringr::str_detect(names, "_w\\d{2}") &
                      !stringr::str_detect(names, "_w\\d{2}(_v[a-z0-9]+)?$")]
  if (length(bad_wave) > 0) {
    stop("Wave segments must be formatted as _wYY at the end (optionally followed by version). Bad names:\n",
         paste(unique(bad_wave), collapse = "\n"))
  }

TRUE
}

#' Clean a segment into snake_case ASCII
#'
#' @param x Character input.
#'
#' @return Cleaned segment or NA.
#' @keywords internal
liss_clean_segment <- function(x) {
  if (is.null(x) || is.na(x) || identical(x, "")) return(NA_character_)

  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  if (identical(x, "")) NA_character_ else x
}

#' Build a canonical LISS variable name
#'
#' @param study_id Study identifier (required).
#' @param construct Construct name (required).
#' @param module Optional module segment.
#' @param facet Optional facet segment.
#' @param item_number Optional item number (integer).
#' @param wave Optional wave number (integer).
#' @param version Optional version string.
#'
#' @return A canonical variable name.
#' @export
liss_build_canonical_name <- function(
    study_id,
    construct,
    module = NULL,
    facet = NULL,
    item_number = NULL,
    wave = NULL,
    version = NULL
) {
  study_id <- liss_clean_segment(study_id)
  construct <- liss_clean_segment(construct)
  module <- liss_clean_segment(module)
  facet <- liss_clean_segment(facet)

  if (is.na(study_id) || is.na(construct)) {
    stop("study_id and construct are required for canonical names.")
  }

  parts <- c(study_id, module, construct, facet)
  parts <- parts[!is.na(parts)]

  name <- paste(parts, collapse = "_")

  if (!is.null(item_number) && !is.na(item_number)) {
    name <- paste0(name, "_i", sprintf("%02d", as.integer(item_number)))
  }

  if (!is.null(wave) && !is.na(wave)) {
    name <- paste0(name, "_w", sprintf("%02d", as.integer(wave)))
  }

  if (!is.null(version) && !is.na(version)) {
    version_clean <- liss_clean_segment(version)
    if (!is.na(version_clean)) {
      name <- paste0(name, "_v", version_clean)
    }
  }

  liss_validate_canonical_names(name)
  name
}

#' Extract an item number from labels or names
#'
#' @param old_name Original variable name.
#' @param label Variable label.
#'
#' @return Integer item number, or NA.
#' @keywords internal
liss_extract_item_number <- function(old_name, label) {
  candidates <- c(old_name, label)
  for (x in candidates) {
    if (is.na(x) || identical(x, "")) next
    m <- stringr::str_match(x, "(\\d{1,2})\\s*$")
    if (!is.na(m[, 2])) {
      return(as.integer(m[, 2]))
    }
  }
  NA_integer_
}

#' Extract a wave number from labels or names
#'
#' @param old_name Original variable name.
#' @param label Variable label.
#'
#' @return Integer wave number, or NA.
#' @keywords internal
liss_extract_wave <- function(old_name, label) {
  candidates <- c(old_name, label)
  for (x in candidates) {
    if (is.na(x) || identical(x, "")) next
    m <- stringr::str_match(x, "\\bw(\\d{1,2})\\b")
    if (!is.na(m[, 2])) {
      return(as.integer(m[, 2]))
    }
  }
  NA_integer_
}

#' Guess a construct slug from label or name
#'
#' @param label Variable label.
#' @param old_name Original variable name.
#'
#' @return Character construct slug.
#' @keywords internal
liss_guess_construct <- function(label, old_name) {
  base <- if (!is.na(label) && !identical(label, "")) label else old_name
  if (is.na(base) || identical(base, "")) return(NA_character_)

  base <- iconv(base, to = "ASCII//TRANSLIT")
  base <- stringr::str_to_lower(base)
  base <- stringr::str_replace_all(base, "[^a-z0-9\\s]", " ")
  base <- stringr::str_squish(base)
  base <- stringr::str_remove_all(base, "\\b(the|a|an|of|to|for|and|or|in|on|at|with|about)\\b")
  base <- stringr::str_squish(base)
  words <- stringr::str_split(base, "\\s+", simplify = TRUE)
  words <- words[words != ""]
  if (length(words) == 0) return(NA_character_)

  construct <- paste(words[seq_len(min(2, length(words)))], collapse = "_")
  construct <- liss_clean_segment(construct)
  if (is.na(construct)) "unknown" else construct
}

#' Guess measure type based on metadata
#'
#' @param old_name Original variable name.
#' @param label Variable label.
#' @param class Data class string.
#' @param has_value_labels Logical flag for value labels.
#' @param item_number Integer item number.
#'
#' @return Measure type string.
#' @keywords internal
liss_guess_measure_type <- function(old_name, label, class, has_value_labels, item_number) {
  label <- stringr::str_to_lower(ifelse(is.na(label), "", label))
  old_name <- stringr::str_to_lower(ifelse(is.na(old_name), "", old_name))

  if (stringr::str_detect(old_name, "\\b(id|identifier|panelid|nomem)\\b") ||
      stringr::str_detect(label, "\\b(id|identifier|panel)\\b")) {
    return("id")
  }

  if (stringr::str_detect(label, "\\b(fieldwork|interview|timestamp|date|duration|weight)\\b")) {
    return("admin")
  }

  if (stringr::str_detect(label, "\\b(age|gender|sex|education|income|household|marital|ethnic|nationality|occupation)\\b")) {
    return("demographic")
  }

  if (!is.na(item_number)) {
    return("scale_item")
  }

  if (stringr::str_detect(label, "\\b(other|specify|open)\\b")) {
    return("open_text")
  }

  if (any(stringr::str_detect(class, "character"))) {
    return("open_text")
  }

  if (isTRUE(has_value_labels)) {
    return("single_item")
  }

  "single_item"
}

#' Convert value labels to JSON
#'
#' @param x A vector with value labels.
#'
#' @return JSON string or NA.
#' @keywords internal
liss_value_labels_json <- function(x) {
  labels <- haven::val_labels(x)
  if (is.null(labels) || length(labels) == 0) return(NA_character_)

  values <- unname(labels)
  labs <- names(labels)
  ord <- order(values, na.last = TRUE)
  mapping <- stats::setNames(as.character(labs[ord]), as.character(values[ord]))
  jsonlite::toJSON(mapping, auto_unbox = TRUE, null = "null")
}

#' Parse a codebook PDF for variable metadata
#'
#' @param codebook_path Path to a PDF codebook.
#'
#' @return A tibble with extracted metadata, or NULL.
#' @keywords internal
liss_parse_codebook_pdf <- function(codebook_path) {
  if (is.null(codebook_path) || identical(codebook_path, "")) return(NULL)
  if (!file.exists(codebook_path)) return(NULL)
  if (!requireNamespace("pdftools", quietly = TRUE)) return(NULL)

  text <- pdftools::pdf_text(codebook_path)
  lines <- unlist(strsplit(text, "\n"))
  lines <- stringr::str_squish(lines)
  lines <- lines[lines != ""]

  if (length(lines) == 0) return(NULL)

  var_rows <- stringr::str_detect(lines, "^[A-Za-z][A-Za-z0-9_]{2,}\\b")
  indices <- which(var_rows)
  if (length(indices) == 0) return(NULL)

  out <- vector("list", length(indices))
  for (i in seq_along(indices)) {
    idx <- indices[i]
    var_line <- lines[[idx]]
    var_name <- stringr::str_extract(var_line, "^[A-Za-z][A-Za-z0-9_]{2,}")
    label <- stringr::str_remove(var_line, "^[A-Za-z][A-Za-z0-9_]{2,}\\s*[-:]*\\s*")

    next_idx <- if (i < length(indices)) indices[[i + 1]] - 1 else length(lines)
    block <- lines[(idx + 1):next_idx]
    cats <- block[stringr::str_detect(block, "^\\d+\\s*[=:].+")]
    categories <- if (length(cats) > 0) paste(cats, collapse = " | ") else NA_character_

    out[[i]] <- tibble::tibble(
      old_name = var_name,
      pdf_label = if (identical(label, "")) NA_character_ else label,
      pdf_categories = categories
    )
  }

  dplyr::bind_rows(out)
}

#' Generate a deterministic provisional name
#'
#' @param study_id Study identifier.
#' @param old_name Original variable name.
#' @param label Variable label.
#'
#' @return A list with provisional naming components.
#' @export
liss_generate_provisional_name <- function(study_id, old_name, label) {
  construct <- liss_guess_construct(label, old_name)
  item_number <- liss_extract_item_number(old_name, label)
  wave <- liss_extract_wave(old_name, label)

  list(
    new_name = liss_build_canonical_name(
      study_id = study_id,
      construct = construct,
      item_number = item_number,
      wave = wave
    ),
    construct = construct,
    facet = NA_character_,
    item_number = item_number,
    wave = wave
  )
}

#' Build a tidy dictionary from an SPSS .sav file
#'
#' @param path_sav Path to the .sav file.
#' @param study_id Study identifier (required).
#' @param codebook_path Optional path to a codebook PDF.
#' @param registry_path Optional path to a mapping registry CSV.
#'
#' @return A tibble with dictionary metadata.
#' @export
build_dictionary_sav <- function(path_sav, study_id, codebook_path = NULL, registry_path = liss_registry_path()) {
  if (missing(study_id) || is.null(study_id) || identical(study_id, "")) {
    stop("study_id is required.")
  }
  if (!file.exists(path_sav)) stop("File not found: ", path_sav)

  df <- haven::read_sav(path_sav)
  registry <- liss_read_registry(registry_path)
  registry <- registry |>
    dplyr::mutate(
      study_id = as.character(.data$study_id),
      old_name = as.character(.data$old_name),
      new_name = as.character(.data$new_name),
      construct = as.character(.data$construct),
      facet = as.character(.data$facet),
      item_number = suppressWarnings(as.integer(.data$item_number)),
      wave = suppressWarnings(as.integer(.data$wave)),
      decision_status = as.character(.data$decision_status)
    )

  pdf_info <- liss_parse_codebook_pdf(codebook_path)

  dict <- tibble::tibble(old_name = names(df)) |>
    dplyr::mutate(
      study_id = as.character(study_id),
      source_file = basename(path_sav),
      var_label = purrr::map_chr(.data$old_name, ~{
        lbl <- haven::var_label(df[[.x]])
        if (is.null(lbl) || identical(lbl, "")) NA_character_ else as.character(lbl)
      }),
      value_labels_json = purrr::map_chr(.data$old_name, ~ liss_value_labels_json(df[[.x]])),
      class = purrr::map_chr(.data$old_name, ~ paste(class(df[[.x]]), collapse = ","))
    ) |>
    dplyr::mutate(
      item_number_sav = purrr::map2_int(.data$old_name, .data$var_label, liss_extract_item_number),
      wave_sav = purrr::map2_int(.data$old_name, .data$var_label, liss_extract_wave)
    )

  dict <- dict |>
    dplyr::left_join(
      registry |>
        dplyr::filter(.data$study_id == study_id) |>
        dplyr::select(
          .data$study_id, .data$old_name, .data$new_name, .data$construct,
          .data$facet, item_number_registry = .data$item_number,
          wave_registry = .data$wave, .data$decision_status
        ),
      by = c("study_id", "old_name")
    )

  if (!is.null(pdf_info)) {
    dict <- dict |>
      dplyr::left_join(pdf_info, by = "old_name") |>
      dplyr::mutate(
        notes = dplyr::if_else(
          !is.na(.data$pdf_categories),
          paste0("Codebook categories: ", .data$pdf_categories),
          NA_character_
        ),
        provenance = dplyr::if_else(!is.na(.data$pdf_label), "pdf_assisted", "sav_only"),
        var_label = dplyr::coalesce(.data$var_label, .data$pdf_label)
      ) |>
      dplyr::select(-dplyr::any_of(c("pdf_label", "pdf_categories")))
  } else {
    dict <- dict |>
      dplyr::mutate(
        notes = NA_character_,
        provenance = "sav_only"
      )
  }

  provisional <- purrr::pmap(
    dict[, c("study_id", "old_name", "var_label")],
    liss_generate_provisional_name
  )
  provisional_tbl <- tibble::tibble(
    new_name_provisional = purrr::map_chr(provisional, "new_name"),
    construct_provisional = purrr::map_chr(provisional, "construct"),
    facet_provisional = purrr::map_chr(provisional, "facet"),
    item_number_provisional = purrr::map_int(provisional, "item_number"),
    wave_provisional = purrr::map_int(provisional, "wave")
  )

  dict <- dplyr::bind_cols(dict, provisional_tbl) |>
    dplyr::mutate(
      new_name = dplyr::coalesce(.data$new_name, .data$new_name_provisional),
      construct = dplyr::coalesce(.data$construct, .data$construct_provisional),
      facet = dplyr::coalesce(.data$facet, .data$facet_provisional),
      item_number = dplyr::coalesce(.data$item_number_registry, .data$item_number_sav, .data$item_number_provisional),
      wave = dplyr::coalesce(.data$wave_registry, .data$wave_sav, .data$wave_provisional),
      decision_status = dplyr::coalesce(.data$decision_status, "auto_provisional"),
      provenance = dplyr::if_else(
        .data$decision_status == "auto_provisional",
        "auto_provisional",
        .data$provenance
      ),
      measure_type = purrr::pmap_chr(
        list(.data$old_name, .data$var_label, .data$class, .data$value_labels_json, .data$item_number),
        function(old_name, var_label, class, value_labels_json, item_number) {
          has_value_labels <- !is.na(value_labels_json)
          liss_guess_measure_type(old_name, var_label, class, has_value_labels, item_number)
        }
      )
    ) |>
    dplyr::select(
      .data$study_id, .data$source_file, .data$old_name, .data$new_name, .data$var_label,
      .data$value_labels_json, .data$class, .data$measure_type, .data$construct,
      .data$facet, .data$item_number, .data$wave, .data$notes, .data$provenance,
      .data$decision_status
    )

  if (anyDuplicated(dict$new_name) > 0) {
    dupes <- dict$new_name[duplicated(dict$new_name)]
    stop("Duplicate new_name detected in dictionary:\n", paste(unique(dupes), collapse = "\n"))
  }

  liss_validate_canonical_names(dict$new_name)
  dict
}

#' Rename a data frame using a dictionary
#'
#' @param df Data frame to rename.
#' @param dict Dictionary with old_name/new_name mappings.
#' @param keep_old Logical; if TRUE, attach mapping attributes.
#'
#' @return A renamed data frame with metadata attributes.
#' @export
rename_with_dictionary <- function(df, dict, keep_old = TRUE) {
  req <- c("old_name", "new_name")
  if (!all(req %in% names(dict))) {
    stop("Dictionary must include columns: ", paste(req, collapse = ", "))
  }

  dict <- dict |>
    dplyr::mutate(
      old_name = as.character(.data$old_name),
      new_name = as.character(.data$new_name),
      decision_status = dplyr::coalesce(.data$decision_status, "auto_provisional")
    )

  if (anyDuplicated(dict$new_name) > 0) {
    dupes <- dict$new_name[duplicated(dict$new_name)]
    stop("Duplicate new_name detected in dictionary:\n", paste(unique(dupes), collapse = "\n"))
  }

  liss_validate_canonical_names(dict$new_name)

  locked_missing <- dict |>
    dplyr::filter(.data$decision_status == "locked") |>
    dplyr::filter(!.data$old_name %in% names(df))

  if (nrow(locked_missing) > 0) {
    stop("Locked mappings missing from df:\n", paste(locked_missing$old_name, collapse = "\n"))
  }

  warn_rows <- dict |>
    dplyr::filter(.data$decision_status %in% c("auto_provisional", "review"))
  if (nrow(warn_rows) > 0) {
    warning("Dictionary contains provisional/review mappings.")
  }

  rename_quos <- rlang::set_names(rlang::syms(dict$old_name), dict$new_name)
  df_out <- dplyr::rename(df, !!!rename_quos)

  if (isTRUE(keep_old)) {
    attr(df_out, "liss_old_names") <- stats::setNames(dict$old_name, dict$new_name)
    attr(df_out, "liss_dictionary") <- dict
  }

  df_out
}

#' Return old names attached to a data frame
#'
#' @param df Data frame with liss_old_names attribute.
#'
#' @return Named character vector of new_name -> old_name.
#' @export
old_names <- function(df) {
  attr(df, "liss_old_names")
}

#' Return dictionary attached to a data frame
#'
#' @param df Data frame with liss_dictionary attribute.
#'
#' @return Dictionary tibble.
#' @export
dictionary <- function(df) {
  attr(df, "liss_dictionary")
}

#' Revert a renamed data frame back to original SPSS names
#'
#' @param df Data frame with liss_old_names attribute.
#'
#' @return Data frame with original names restored.
#' @export
revert_names <- function(df) {
  mapping <- old_names(df)
  if (is.null(mapping)) {
    stop("No liss_old_names attribute found.")
  }
  rename_quos <- rlang::set_names(rlang::syms(names(mapping)), mapping)
  dplyr::rename(df, !!!rename_quos)
}

#' Select variables by construct
#'
#' @param df Data frame with liss_dictionary attribute.
#' @param construct Construct to select.
#'
#' @return A data frame with selected columns.
#' @export
select_construct <- function(df, construct) {
  dict <- dictionary(df)
  if (is.null(dict)) stop("No liss_dictionary attribute found.")
  if (!"construct" %in% names(dict)) stop("Dictionary must include construct column.")

  vars <- dict |>
    dplyr::filter(.data$construct == construct) |>
    dplyr::pull(.data$new_name)

  dplyr::select(df, dplyr::all_of(vars))
}
