# =============================================================================
# manifest.R
# =============================================================================

liss_read_manifest <- function() {
  manifest_path <- liss_extdata_path("manifest.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  manifest <- tibble::as_tibble(manifest)

  required <- c(
    "module",
    "version",
    "sav_file",
    "dict_file",
    "codebook_file",
    "id_key_new",
    "id_key_old",
    "notes"
  )

  missing_cols <- setdiff(required, names(manifest))
  if (length(missing_cols) > 0) {
    stop("Manifest is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  dupes <- manifest |>
    dplyr::count(.data$module, .data$version) |>
    dplyr::filter(.data$n > 1)

  if (nrow(dupes) > 0) {
    stop("Manifest has duplicate module/version rows.")
  }

  dict_paths <- purrr::map_chr(manifest$dict_file, ~ liss_extdata_path(.x))
  if (!all(file.exists(dict_paths))) {
    stop("Manifest references missing dictionary files.")
  }

  manifest
}

liss_available_versions <- function(module) {
  manifest <- liss_read_manifest()
  manifest |>
    dplyr::filter(.data$module == module) |>
    dplyr::pull(.data$version)
}

liss_resolve_version <- function(module, version) {
  available <- liss_available_versions(module)

  if (length(available) == 0) {
    stop("Unknown module: ", module)
  }

  if (!is.null(version)) {
    if (!version %in% available) {
      stop(
        "Version '", version, "' not available for module '", module, "'. ",
        "Available versions: ", paste(available, collapse = ", ")
      )
    }
    return(version)
  }

  if (length(available) == 1) {
    return(available)
  }

  parsed <- purrr::map(available, ~ tryCatch(utils::package_version(.x), error = function(e) NULL))
  if (any(purrr::map_lgl(parsed, is.null))) {
    stop(
      "Multiple versions available for module '", module, "'. ",
      "Specify one of: ", paste(available, collapse = ", ")
    )
  }

  parsed_vec <- vapply(parsed, identity, utils::package_version("0"))
  available[which.max(parsed_vec)]
}

liss_load_dictionary <- function(module, version = NULL) {
  manifest <- liss_read_manifest()
  resolved <- liss_resolve_version(module, version)

  manifest_row <- manifest |>
    dplyr::filter(.data$module == module, .data$version == resolved)

  if (nrow(manifest_row) == 0) {
    stop("No manifest entry for module '", module, "' version '", resolved, "'.")
  }

  dict_path <- liss_extdata_path(manifest_row$dict_file[[1]])
  dict <- tryCatch(
    readRDS(dict_path),
    error = function(e) {
      stop("Failed to read dictionary RDS: ", dict_path, call. = FALSE)
    }
  )
  dict
}
