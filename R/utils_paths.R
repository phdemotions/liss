# =============================================================================
# utils_paths.R
# =============================================================================

liss_extdata_path <- function(...) {
  rel_path <- file.path(...)
  override_root <- Sys.getenv("LISS_EXTDATA_PATH", unset = "")
  if (!identical(override_root, "")) {
    pkg_path <- file.path(override_root, rel_path)
  } else {
    pkg_path <- system.file("extdata", rel_path, package = "liss")
    if (pkg_path == "") {
      pkg_path <- file.path("inst", "extdata", rel_path)
    }
  }

  if (!file.exists(pkg_path)) {
    stop("Extdata path not found: ", rel_path)
  }

  pkg_path
}
