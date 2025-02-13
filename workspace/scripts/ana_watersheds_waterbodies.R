ana_watersheds_waterbodies <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/watersheds_waterbodies-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_polygons.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_lines.gpkg",
  #   "workspace/data/harvested/national_hydro_network-1.0.0/processed/watersheds.gpkg"
  # )
  input_files <- unlist(input_files)

  # -----------------------------------------------------------------
  # Lakes & rivers
  waterbodies <- dplyr::bind_rows(
    input_files[grepl("lakes_polygons.gpkg", input_files)] |>
      sf::st_read(quiet = TRUE) |>
      dplyr::arrange(waterbody_id) |>
      sf::st_transform(crs = 3857),
    rivers <- input_files[grepl("rivers_lines.gpkg", input_files)] |>
      sf::st_read(quiet = TRUE) |>
      dplyr::arrange(waterbody_id) |>
      sf::st_transform(crs = 3857)
  ) |>
    dplyr::select(waterbody_id)

  # -----------------------------------------------------------------
  # Watersheds
  watersheds <- input_files[grepl("watersheds.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::arrange(watershed_id) |>
    sf::st_transform(crs = 3857)

  # -----------------------------------------------------------------
  # Perform spatial join
  # Lakes
  watersheds_waterbodies <- sf::st_intersects(watersheds, waterbodies)
  for (i in seq_len(length(watersheds_waterbodies))) {
    if (length(watersheds_waterbodies[[i]] > 0)) {
      watersheds_waterbodies[[i]] <- data.frame(
        watershed_id = watersheds$watershed_id[i],
        waterbody_id = waterbodies$waterbody_id[watersheds_waterbodies[[i]]]
      )
    }
  }
  watersheds_waterbodies <- purrr::compact(watersheds_waterbodies) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    na.omit()

  # Export
  watersheds_waterbodies |>
    vroom::vroom_write(file.path(output_path, "watersheds_waterbodies.csv"), delim = ",")
}
