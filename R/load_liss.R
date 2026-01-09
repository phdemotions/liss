# =============================================================================
# load_liss.R
# =============================================================================

#' Load a LISS module
#'
#' @param module Module identifier (e.g., "yf24a").
#' @param version Version string; if NULL, resolves using manifest rules.
#' @param path Optional path to a user-provided .sav file.
#' @param naming Either "analysis" (apply dictionary) or "raw".
#' @param keep_provenance Logical; if TRUE, attach provenance attributes and labels.
#' @param keep_unmapped Logical; if FALSE, drop unmapped raw columns when renaming.
#'
#' @return A data frame with attributes describing module metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' modules()
#' df <- load_liss("yf24a", naming = "analysis")
#' }
load_liss <- function(
    module,
    version = NULL,
    path = NULL,
    naming = c("analysis", "raw"),
    keep_provenance = TRUE,
    keep_unmapped = TRUE
) {
  naming <- match.arg(naming)

  manifest <- liss_read_manifest()
  resolved <- liss_resolve_version(module, version)

  manifest_row <- manifest |>
    dplyr::filter(.data$module == module, .data$version == resolved)

  if (nrow(manifest_row) == 0) {
    stop("No manifest entry for module '", module, "' version '", resolved, "'.")
  }

  dict <- liss_load_dictionary(module, resolved)
  liss_validate_dictionary(dict, strict = TRUE)

  id_old <- manifest_row$id_key_old[[1]]
  id_new <- manifest_row$id_key_new[[1]]

  if (!id_old %in% dict$old) {
    stop("Dictionary missing id_key_old: ", id_old)
  }

  if (!id_new %in% dict$new) {
    stop("Dictionary missing id_key_new: ", id_new)
  }

  if (!is.null(path)) {
    sav_path <- path
  } else {
    sav_file <- manifest_row$sav_file[[1]]
    if (is.na(sav_file) || identical(sav_file, "")) {
      stop("No packaged sav for this module; supply path=.")
    }
    sav_path <- liss_extdata_path(sav_file)
  }

  df_raw <- haven::read_sav(sav_path)

  if (naming == "analysis") {
    df_out <- liss_rename_with_dictionary(
      df_raw,
      dict,
      keep_provenance = keep_provenance,
      keep_unmapped = keep_unmapped
    )
  } else {
    df_out <- df_raw
    if (isTRUE(keep_provenance)) {
      attr(df_out, "name_dictionary") <- dict
      attr(df_out, "name_dictionary_timestamp") <- as.character(Sys.time())
    }
  }

  attr(df_out, "liss_module") <- module
  attr(df_out, "liss_version") <- resolved

  codebook_rel <- manifest_row$codebook_file[[1]]
  attr(df_out, "liss_codebook") <- liss_extdata_path(codebook_rel)

  df_out
}
