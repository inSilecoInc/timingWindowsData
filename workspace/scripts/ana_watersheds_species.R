ana_watersheds_species <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/watersheds_species-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv",
  #   "workspace/data/harvested/national_hydro_network-1.0.0/processed/watersheds.gpkg",
  #   "workspace/data/harvested/gbif-1.0.0/raw/species_occurrences_gbif.gpkg"
  # )
  input_files <- unlist(input_files)

  # -----------------------------------------------------------------
  # Watersheds
  watersheds <- input_files[grepl("watersheds.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    dplyr::arrange(watershed_id) |>
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
  watersheds_species <- sf::st_intersects(watersheds, species)
  for (i in 1:length(watersheds_species)) {
    if (length(watersheds_species[[i]] > 0)) {
      watersheds_species[[i]] <- data.frame(
        watershed_id = watersheds$watershed_id[i],
        species = species$species_id[watersheds_species[[i]]]
      )
    }
  }
  watersheds_species <- purrr::compact(watersheds_species) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    na.omit()

  # -----------------------------------------------------------------
  # Check watersheds without species
  # sum(!watersheds_species$watershed_id %in% watersheds$watershed_id)
  # None found, but many with single species
  vroom::vroom_write(watersheds_species, file.path(output_path, "watersheds_species.csv"), delim = ",")
}
