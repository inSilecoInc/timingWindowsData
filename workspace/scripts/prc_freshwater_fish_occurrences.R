prc_freshwater_fish_occurrences <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/freshwater_fish_occurrences-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/freshwater_fish_occurrences-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   "freshwater_fish_occurrences.zip"
  # )
  input_files <- unlist(input_files)

  # Unzip data
  tmp <- file.path(output_path, "tmp/")
  archive::archive_extract(input_files, tmp)

  # Bassins
  sf::st_read(file.path(tmp, "Basin042017_3119.shp"), quiet = TRUE) |>
    janitor::clean_names() |>
    dplyr::rename_with(~ sub("^x\\d+_", "", .x)) |>
    sf::st_write(dsn = file.path(output_path, "bassins.gpkg"), quiet = TRUE, delete_dsn = TRUE)

  # Occurrence table
  suppressWarnings({
    vroom::vroom(file.path(tmp, "Occurrence_Table.csv"), progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::rename_with(~ sub("^x\\d+_", "", .x)) |>
      dplyr::mutate(
        species_name_in_source = stringr::str_replace(species_name_in_source, "\\.", " "),
        fishbase_valid_species_name = stringr::str_replace(fishbase_valid_species_name, "\\.", " ")
      ) |>
      vroom::vroom_write(file.path(output_path, "occurrences.csv"), delim = ",")
  })

  # Clean up temporary files
  fs::dir_delete(tmp)
}
