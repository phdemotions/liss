# =============================================================================
# discovery.R
# =============================================================================

#' List available modules and versions
#'
#' @return A tibble of modules and versions.
#' @export
#'
#' @examples
#' modules()
modules <- function() {
  manifest <- liss_read_manifest()
  manifest |>
    dplyr::select(.data$module, .data$version)
}

#' Return a module crosswalk
#'
#' @param module Module identifier.
#' @param version Version string; if NULL, resolves using manifest rules.
#'
#' @return A tibble with module, old, new, measurement_label, and label_raw.
#' @export
#'
#' @examples
#' crosswalk("yf24a")
crosswalk <- function(module, version = NULL) {
  dict <- liss_load_dictionary(module, version)
  if (!"label_raw" %in% names(dict)) {
    dict$label_raw <- NA_character_
  }
  dict |>
    dplyr::select(.data$module, .data$old, .data$new, .data$measurement_label, .data$label_raw)
}

#' List variables for a module
#'
#' @param module Module identifier.
#' @param version Version string; if NULL, resolves using manifest rules.
#' @param naming Either "analysis" (dictionary names) or "raw".
#'
#' @return A tibble of variable names and labels.
#' @export
#'
#' @examples
#' vars("yf24a")
vars <- function(module, version = NULL, naming = c("analysis", "raw")) {
  naming <- match.arg(naming)
  dict <- liss_load_dictionary(module, version)

  if (naming == "analysis") {
    dict |>
      dplyr::transmute(
        new = .data$new,
        measurement_label = .data$measurement_label,
        orig_old = .data$old
      )
  } else {
    if (!"label_raw" %in% names(dict)) {
      dict$label_raw <- NA_character_
    }
    dict |>
      dplyr::transmute(
        old = .data$old,
        label_raw = .data$label_raw
      )
  }
}

#' Search variable names and labels
#'
#' @param pattern Regular expression pattern to search.
#' @param module Optional module filter.
#' @param version Optional version filter.
#' @param fields Fields to search in.
#'
#' @return A tibble of matching variables with module/version context.
#' @export
#'
#' @examples
#' search_vars("wellbeing")
search_vars <- function(
    pattern,
    module = NULL,
    version = NULL,
    fields = c("new", "old", "measurement_label", "label_raw")
) {
  manifest <- liss_read_manifest()

  if (!is.null(module)) {
    manifest <- manifest |>
      dplyr::filter(.data$module == module)
  }

  if (!is.null(version)) {
    manifest <- manifest |>
      dplyr::filter(.data$version == version)
  }

  if (nrow(manifest) == 0) {
    return(tibble::tibble())
  }

  fields <- as.character(fields)
  if (!all(fields %in% c("new", "old", "measurement_label", "label_raw"))) {
    stop("Invalid fields requested.")
  }

  results <- purrr::map_dfr(seq_len(nrow(manifest)), function(i) {
    dict <- liss_load_dictionary(manifest$module[[i]], manifest$version[[i]])
    if (!"label_raw" %in% names(dict)) {
      dict$label_raw <- NA_character_
    }

    haystack <- dict[, fields, drop = FALSE]
    match_rows <- apply(haystack, 1, function(row) {
      any(stringr::str_detect(as.character(row), pattern))
    })

    if (!any(match_rows)) {
      return(tibble::tibble())
    }

    dict[match_rows, , drop = FALSE] |>
      dplyr::transmute(
        module = manifest$module[[i]],
        version = manifest$version[[i]],
        old = .data$old,
        new = .data$new,
        measurement_label = .data$measurement_label,
        label_raw = .data$label_raw
      )
  })

  results
}
