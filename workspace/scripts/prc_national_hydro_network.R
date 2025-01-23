prc_national_hydro_network <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/national_hydro_network-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/national_hydro_network-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   "rhn_nhn_decoupage.gpkg.zip"
  # )
  input_files <- unlist(input_files)

  # Unzip data
  tmp <- file.path(output_path, "tmp/")
  archive::archive_extract(input_files, tmp)

  watersheds <- file.path(tmp, "rhn_nhn_decoupage.gpkg") |>
    sf::st_read(quiet = TRUE) |>
    sf::st_cast("GEOMETRYCOLLECTION") |>
    sf::st_collection_extract("POLYGON") |>
    sf::st_simplify(dTolerance = 100) |>
    dplyr::mutate(
      watershed_id = sprintf("ws_%06d", seq_len(dplyr::n())),
    ) |>
    dplyr::select(watershed_id)

  # Export
  sf::st_write(watersheds, file.path(output_path, "watersheds.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
