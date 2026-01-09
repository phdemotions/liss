create_test_dictionary <- function(module, include_ids = TRUE) {
  dict <- tibble::tibble(
    module = module,
    old = c("nomem_encr2", "yf24a010"),
    new = c("person_id_encrypted__raw", "hire_likelihood_self__percent"),
    measurement_label = c(
      "Encrypted household member identifier (merge key)",
      "Likelihood of being hired for target job (0-100)"
    ),
    label_raw = c(
      "Encrypted household member identifier (merge key)",
      "Likelihood of being hired for target job (0-100)"
    )
  )

  if (!include_ids) {
    dict$new <- c("respondent_id__raw", "hire_likelihood_self__percent")
  }

  dict
}

create_test_manifest <- function(path) {
  manifest <- tibble::tibble(
    module = c("yf24a", "yf24a", "ambig", "ambig", "badid"),
    version = c("1.0", "2.0", "alpha", "beta", "1.0"),
    sav_file = c("", "", "", "", ""),
    dict_file = c(
      "dictionaries/dict_yf24a_1.0.rds",
      "dictionaries/dict_yf24a_2.0.rds",
      "dictionaries/dict_ambig_alpha.rds",
      "dictionaries/dict_ambig_beta.rds",
      "dictionaries/dict_badid_1.0.rds"
    ),
    codebook_file = c(
      "codebooks/yf24a_EN_1.0.pdf",
      "codebooks/yf24a_EN_2.0.pdf",
      "codebooks/ambig_alpha.pdf",
      "codebooks/ambig_beta.pdf",
      "codebooks/badid_1.0.pdf"
    ),
    id_key_new = c(
      "person_id_encrypted__raw",
      "person_id_encrypted__raw",
      "person_id_encrypted__raw",
      "person_id_encrypted__raw",
      "person_id_encrypted__raw"
    ),
    id_key_old = c(
      "nomem_encr2",
      "nomem_encr2",
      "nomem_encr2",
      "nomem_encr2",
      "nomem_encr2"
    ),
    notes = c("test", "test", "test", "test", "test")
  )

  utils::write.csv(manifest, file.path(path, "manifest.csv"), row.names = FALSE)
}

create_test_extdata <- function() {
  root <- tempfile("liss-extdata-")
  dir.create(root)
  dir.create(file.path(root, "dictionaries"), recursive = TRUE)
  dir.create(file.path(root, "codebooks"), recursive = TRUE)
  dir.create(file.path(root, "data"), recursive = TRUE)

  saveRDS(create_test_dictionary("yf24a"), file.path(root, "dictionaries", "dict_yf24a_1.0.rds"))
  saveRDS(create_test_dictionary("yf24a"), file.path(root, "dictionaries", "dict_yf24a_2.0.rds"))
  saveRDS(create_test_dictionary("ambig"), file.path(root, "dictionaries", "dict_ambig_alpha.rds"))
  saveRDS(create_test_dictionary("ambig"), file.path(root, "dictionaries", "dict_ambig_beta.rds"))
  saveRDS(create_test_dictionary("badid", include_ids = FALSE), file.path(root, "dictionaries", "dict_badid_1.0.rds"))

  file.create(file.path(root, "codebooks", "yf24a_EN_1.0.pdf"))
  file.create(file.path(root, "codebooks", "yf24a_EN_2.0.pdf"))
  file.create(file.path(root, "codebooks", "ambig_alpha.pdf"))
  file.create(file.path(root, "codebooks", "ambig_beta.pdf"))
  file.create(file.path(root, "codebooks", "badid_1.0.pdf"))

  create_test_manifest(root)

  root
}

with_test_extdata <- function(code) {
  root <- create_test_extdata()
  old <- Sys.getenv("LISS_EXTDATA_PATH", unset = NA)
  Sys.setenv(LISS_EXTDATA_PATH = root)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("LISS_EXTDATA_PATH")
    } else {
      Sys.setenv(LISS_EXTDATA_PATH = old)
    }
  }, add = TRUE)

  force(code)
}
