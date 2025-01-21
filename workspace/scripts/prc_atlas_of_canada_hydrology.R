prc_atlas_of_canada_hydrology <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "AC_1M_metadata_EN_201705.pdf",
  #     "AC_1M_Rivers.gdb.zip",
  #     "AC_1M_Schema_Dissemination_2017.pdf",
  #     "AC_1M_Waterbodies.gdb.zip"
  #    )
  # )
  input_files <- unlist(input_files)

  # Unzip data
  tmp <- file.path(output_path, "tmp/")
  archive::archive_extract(input_files[grepl("AC_1M_Waterbodies.gdb.zip", input_files)], tmp)
  archive::archive_extract(input_files[grepl("AC_1M_Rivers.gdb.zip", input_files)], tmp)

  # Waterbodies
  waterbodies <- sf::st_read(file.path(tmp, "AC_1M_Waterbodies.gdb"), layer = "AC_1M_Waterbodies", quiet = TRUE)
  waterbodies$area <- units::set_units(sf::st_area(waterbodies), km2)
  waterbodies$perimeter <- units::set_units(lwgeom::st_perimeter_lwgeom(waterbodies), km)
  waterbodies <- waterbodies |>
    # dplyr::filter(area > units::set_units(5, km2)) |>
    dplyr::mutate(waterbody_id = sprintf("wb_%06d", seq_len(dplyr::n()))) |>
    dplyr::select(waterbody_id, name = NAME, name_fr = NOM, area, perimeter)
  sf::st_write(waterbodies, file.path(output_path, "waterbodies.gpkg"), delete_dsn = TRUE)

  # rivers
  rivers <- sf::st_read(file.path(tmp, "AC_1M_Rivers_dense.gdb"), layer = "AC_1M_Rivers_dense", quiet = TRUE)
  rivers$length <- units::set_units(sf::st_length(rivers), km)
  rivers <- rivers |>
    # dplyr::filter(area > units::set_units(5, km2)) |>
    dplyr::mutate(river_id = sprintf("wb_%06d", seq_len(dplyr::n()))) |>
    dplyr::select(river_id, name = NAME, name_fr = NOM, length)
  sf::st_write(rivers, file.path(output_path, "rivers.gpkg"), delete_dsn = TRUE)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
