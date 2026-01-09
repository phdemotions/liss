# Example workflow for the LISS naming system

sav_path <- "path/to/yf24a_EN_1.0p.sav"
codebook_path <- "path/to/codebook_yf24a_EN_1.0.pdf"

# Build dictionary (sav-only if codebook is unavailable)
dict <- build_dictionary_sav(
  path_sav = sav_path,
  study_id = "yf24a",
  codebook_path = codebook_path
)

# Save dictionary to disk
utils::write.csv(dict, file = "yf24a_dictionary.csv", row.names = FALSE)

# Apply renaming and keep provenance
raw_df <- haven::read_sav(sav_path)
renamed <- rename_with_dictionary(raw_df, dict, keep_old = TRUE)

# Revert back to original names
reverted <- revert_names(renamed)

# Select by construct
agreeable <- select_construct(renamed, "personality")
