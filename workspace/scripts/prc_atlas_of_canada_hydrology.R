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

  ## ------------- Lakes data
  lakes <- sf::st_read(file.path(tmp, "AC_1M_Waterbodies.gdb"), layer = "AC_1M_Waterbodies", quiet = TRUE)
  lakes$area <- units::set_units(sf::st_area(lakes), km2)
  lakes$perimeter <- units::set_units(lwgeom::st_perimeter_lwgeom(lakes), km)
  lakes <- lakes |>
    # dplyr::filter(area > units::set_units(5, km2)) |>
    dplyr::mutate(
      waterbody_id = sprintf("wb_lak_%06d", seq_len(dplyr::n())),
      wb_type = "lake"
    ) |>
    dplyr::select(waterbody_id, wb_type, name = NAME, name_fr = NOM, area, perimeter) |>
    sf::st_transform(crs = 4326)
  sf::st_write(lakes, file.path(output_path, "lakes_polygons.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Points
  lakes |>
    sf::st_centroid() |>
    dplyr::select(waterbody_id) |>
    sf::st_write(file.path(output_path, "lakes_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  ## ------------ Rivers
  rivers <- sf::st_read(file.path(tmp, "AC_1M_Rivers_dense.gdb"), layer = "AC_1M_Rivers_dense", quiet = TRUE)
  rivers$length <- units::set_units(sf::st_length(rivers), km)
  rivers <- rivers |>
    # dplyr::filter(area > units::set_units(5, km2)) |>
    dplyr::mutate(
      waterbody_id = sprintf("wb_riv_%06d", seq_len(dplyr::n())),
      wb_type = "river"
    ) |>
    dplyr::select(waterbody_id, wb_type, name = NAME, name_fr = NOM, length) |>
    sf::st_transform(crs = 4326)

  sf::st_write(rivers, file.path(output_path, "rivers_lines.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Points
  rivers_pt <- rivers |>
    sf::st_cast("LINESTRING") |>
    sf::st_transform(crs = 3857) |>
    sf::st_line_sample(sample = 0.5, 1) |>
    sf::st_cast("POINT") |>
    sf::st_transform(crs = 4326)

  sf::st_sf(
    data.frame(waterbody_id = rivers$waterbody_id),
    geom = rivers_pt
  ) |>
    sf::st_write(file.path(output_path, "rivers_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
