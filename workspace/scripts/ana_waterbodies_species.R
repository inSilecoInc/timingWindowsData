ana_waterbodies_species <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/waterbodies_species-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_polygons.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_lines.gpkg",
  #   "workspace/data/harvested/gbif-1.0.0/raw/species_occurrences_gbif.gpkg"
  # )
  input_files <- unlist(input_files)

  # -----------------------------------------------------------------
  # Lakes & rivers
  lakes <- input_files[grepl("lakes_polygons.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::arrange(waterbody_id) |>
    sf::st_buffer(2000) |>
    sf::st_transform(crs = 3857)

  rivers <- input_files[grepl("rivers_lines.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::arrange(waterbody_id) |>
    sf::st_buffer(2000) |>
    sf::st_transform(crs = 3857)

  # -----------------------------------------------------------------
  # Species list
  spList <- input_files[grepl("freshwater_fish_species_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(species_id, species)

  # -----------------------------------------------------------------
  # Species opccurrences
  species <- input_files[grepl("species_occurrences_gbif.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_transform(crs = 3857) |>
    dplyr::left_join(spList, by = "species") |>
    dplyr::select(species_id) |>
    na.omit()

  # -----------------------------------------------------------------
  # Perform spatial join
  # Lakes
  lakes_species <- sf::st_intersects(lakes, species)
  for (i in 1:length(lakes_species)) {
    if (length(lakes_species[[i]] > 0)) {
      lakes_species[[i]] <- data.frame(
        waterbody_id = lakes$waterbody_id[i],
        species_id = species$species_id[lakes_species[[i]]]
      )
    }
  }
  lakes_species <- purrr::compact(lakes_species) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    na.omit()

  # Rivers
  rivers_species <- sf::st_intersects(rivers, species)
  for (i in 1:length(rivers_species)) {
    if (length(rivers_species[[i]] > 0)) {
      rivers_species[[i]] <- data.frame(
        waterbody_id = rivers$waterbody_id[i],
        species_id = species$species_id[rivers_species[[i]]]
      )
    }
  }
  rivers_species <- purrr::compact(rivers_species) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    na.omit()

  # Bind together and export
  dplyr::bind_rows(lakes_species, rivers_species) |>
    vroom::vroom_write(file.path(output_path, "waterbodies_species.csv"), delim = ",")
}
