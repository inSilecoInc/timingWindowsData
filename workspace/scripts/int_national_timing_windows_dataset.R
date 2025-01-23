int_national_timing_windows_dataset <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/national_timing_windows_dataset-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_polygons.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_points.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_lines.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_points.gpkg",
  #   "workspace/data/harvested/national_hydro_network-1.0.0/processed/watersheds.gpkg"
  # )
  input_files <- unlist(input_files)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sqlite DB
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(output_path, "national_timing_windows_database.sqlite"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tables for DB schema

  # -----------------------------------------------------------------
  # Watersheds
  watersheds <- input_files[grepl("watersheds.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE) |>
    sf::st_drop_geometry()

  # -----------------------------------------------------------------
  # Lakes & rivers
  waterbodies <- list(
    input_files[grepl("lakes_polygons.gpkg", input_files)] |>
      sf::st_read(quiet = TRUE) |>
      sf::st_drop_geometry(),
    input_files[grepl("rivers_lines.gpkg", input_files)] |>
      sf::st_read(quiet = TRUE) |>
      sf::st_drop_geometry()
  ) |>
    dplyr::bind_rows()

  # -----------------------------------------------------------------
  # Species
  species <- input_files[grepl("freshwater_fish_species_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  # # -----------------------------------------------------------------
  # # Life processes
  # life_processes <- data.frame(
  #   life_process_id = character(0),
  #   name = character(0),
  #   description = character(0)
  # )

  # # -----------------------------------------------------------------
  # # Traits
  # traits <- data.frame(
  #   trait_id = character(0),
  #   name = character(0),
  #   description = character(0)
  # )

  # # -----------------------------------------------------------------
  # # WUA
  # wua <- data.frame(
  #   wua_id = character(0),
  #   name = character(0),
  #   description = character(0)
  # )

  # # -----------------------------------------------------------------
  # # Stressors
  # stressors <- data.frame(
  #   stressor_id = character(0),
  #   name = character(0),
  #   description = character(0)
  # )

  # # -----------------------------------------------------------------
  # # Waterbodies - species
  # waterbodies_species <- data.frame(
  #   waterbody_id = character(0),
  #   species_id = character(0),
  #   waterbody_species_id = character(0)
  # )

  # # -----------------------------------------------------------------
  # # Waterbody - species - life processes
  # waterbodies_species_life_processes <- data.frame(
  #   waterbody_species_id = character(0),
  #   life_process_id = character(0),
  #   start = numeric(0),
  #   end = numeric(0)
  # )

  # # -----------------------------------------------------------------
  # # WUA - stressors
  # wua_stressors <- data.frame(
  #   wua_id = character(0),
  #   stressor_id = character(0)
  # )

  # # -----------------------------------------------------------------
  # # Species - traits
  # species_traits <- data.frame(
  #   species_id = character(0),
  #   trait_id = character(0),
  #   value = numeric(0)
  # )

  # # -----------------------------------------------------------------
  # # Stressor - traits
  # stressors_traits <- data.frame(
  #   stressor_id = character(0),
  #   trait_id = character(0),
  #   value = numeric(0)
  # )

  # # -----------------------------------------------------------------
  # # Species stressor sensitivity
  # species_stressor_sensitivity <- data.frame(
  #   species_id = character(0),
  #   stressor_id = character(0),
  #   value = numeric(0)
  # )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Base de données
  DBI::dbWriteTable(con, "watersheds", watersheds, overwrite = TRUE)
  DBI::dbWriteTable(con, "waterbodies", waterbodies, overwrite = TRUE)
  DBI::dbWriteTable(con, "species", species, overwrite = TRUE)
  # DBI::dbWriteTable(con, "life_processes", life_processes, overwrite = TRUE)
  # DBI::dbWriteTable(con, "traits", traits, overwrite = TRUE)
  # DBI::dbWriteTable(con, "wua", wua, overwrite = TRUE)
  # DBI::dbWriteTable(con, "stressors", stressors, overwrite = TRUE)
  # DBI::dbWriteTable(con, "waterbodies_species", waterbodies_species, overwrite = TRUE)
  # DBI::dbWriteTable(con, "waterbodies_species_life_processes", waterbodies_species_life_processes, overwrite = TRUE)
  # DBI::dbWriteTable(con, "wua_stressors", wua_stressors, overwrite = TRUE)
  # DBI::dbWriteTable(con, "species_traits", species_traits, overwrite = TRUE)
  # DBI::dbWriteTable(con, "stressors_traits", stressors_traits, overwrite = TRUE)
  # DBI::dbWriteTable(con, "species_stressor_sensitivity", species_stressor_sensitivity, overwrite = TRUE)

  # Figure de la structure de la base de données
  dm::dm_from_con(con) |>
    dm::dm_add_pk(table = "watersheds", "watershed_id") |>
    dm::dm_add_pk(table = "waterbodies", "waterbody_id") |>
    dm::dm_add_pk(table = "species", "species_id") |>
    # dm::dm_add_pk(table = "life_processes", "life_process_id") |>
    # dm::dm_add_pk(table = "traits", "trait_id") |>
    # dm::dm_add_pk(table = "wua", "wua_id") |>
    # dm::dm_add_pk(table = "stressors", "stressor_id") |>
    # dm::dm_add_pk(table = "waterbodies_species", "waterbody_species_id") |>
    # dm::dm_add_fk(table = "waterbodies_species", "waterbody_id", "waterbodies") |>
    # dm::dm_add_fk(table = "waterbodies_species", "species_id", "species") |>
    # dm::dm_add_fk(table = "waterbodies_species_life_processes", "waterbody_species_id", "waterbodies_species") |>
    # dm::dm_add_fk(table = "waterbodies_species_life_processes", "life_process_id", "life_processes") |>
    # dm::dm_add_fk(table = "wua_stressors", "wua_id", "wua") |>
    # dm::dm_add_fk(table = "wua_stressors", "stressor_id", "stressors") |>
    # dm::dm_add_fk(table = "species_traits", "species_id", "species") |>
    # dm::dm_add_fk(table = "species_traits", "trait_id", "traits") |>
    # dm::dm_add_fk(table = "stressors_traits", "stressor_id", "stressors") |>
    # dm::dm_add_fk(table = "stressors_traits", "trait_id", "traits") |>
    # dm::dm_add_fk(table = "species_stressor_sensitivity", "species_id", "species") |>
    # dm::dm_add_fk(table = "species_stressor_sensitivity", "stressor_id", "stressors") |>
    dm::dm_draw(view_type = "all", rankdir = "BT", column_types = TRUE) |>
    DiagrammeRsvg::export_svg() |>
    write(file.path(output_path, "national_timing_windows_database.svg"))

  DBI::dbDisconnect(con)
}
