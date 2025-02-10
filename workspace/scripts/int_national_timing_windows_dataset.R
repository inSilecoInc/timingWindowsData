int_national_timing_windows_dataset <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/national_timing_windows_dataset-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_polygons.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/lakes_points.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_lines.gpkg",
  #   "workspace/data/harvested/atlas_of_canada_hydrology-1.0.0/processed/rivers_points.gpkg",
  #   "workspace/data/harvested/national_hydro_network-1.0.0/processed/watersheds.gpkg",
  #   "workspace/data/analyzed/waterbodies_species-1.0.0/waterbodies_species.csv",
  #   "workspace/data/analyzed/watersheds_species-1.0.0/watersheds_species.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/habitat_adult.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/habitat_juvenile.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/habitat_spawning.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/habitat_yoy.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/food_items.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/spawning.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/eggs.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/larvae.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/migration.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/morphology.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/tolerance_adult.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/tolerance_embryo.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/tolerance_larvae.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/tolerance_spawner.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/taxonomy.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/picture.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/swimming.csv",
  #   "workspace/data/analyzed/species_phenology-1.0.0/migration.csv",
  #   "workspace/data/analyzed/species_phenology-1.0.0/spawning.csv",
  #   "workspace/data/analyzed/species_phenology-1.0.0/larvae.csv"
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
  migration_phenology <- input_files[grepl("species_phenology-1.0.0/migration.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  spawning_phenology <- input_files[grepl("species_phenology-1.0.0/spawning.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  larvae_phenology <- input_files[grepl("species_phenology-1.0.0/larvae.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  # -----------------------------------------------------------------
  # Traits
  habitat_adult <- input_files[grepl("habitat_adult.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(-life_stage) |>
    tidyr::pivot_wider(names_from = habitat, values_from = value, values_fill = list(value = 0)) |>
    dplyr::arrange(species_id)

  habitat_juvenile <- input_files[grepl("habitat_juvenile.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(-life_stage) |>
    tidyr::pivot_wider(names_from = habitat, values_from = value, values_fill = list(value = 0)) |>
    dplyr::arrange(species_id)

  habitat_spawning <- input_files[grepl("species_traits-1.0.0/habitat_spawning.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(-life_stage) |>
    tidyr::pivot_wider(names_from = habitat, values_from = value, values_fill = list(value = 0)) |>
    dplyr::arrange(species_id)

  habitat_yoy <- input_files[grepl("species_traits-1.0.0/habitat_yoy.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(-life_stage) |>
    tidyr::pivot_wider(names_from = habitat, values_from = value, values_fill = list(value = 0)) |>
    dplyr::arrange(species_id)

  food_items <- input_files[grepl("species_traits-1.0.0/food_items.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  # Spawning
  reproductive_guilds <- c(
    "ariadnophils", "batch_spawner", "bearers", "brood_hiders", "clutch_tenders",
    "external_brooders", "guarders", "internal_live_bearers", "litho-pelagophils",
    "lithophils", "nesters", "nonguarders", "open_substratum_spawners", "pelagophils",
    "phyto-lithophils", "phytophils", "polyphils", "psammophils", "speleophils",
    "substratum_choosers"
  )

  spawning <- input_files[grepl("species_traits-1.0.0/spawning.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::group_by(spawning) |>
    dplyr::group_split() |>
    lapply(function(x) {
      if (x$spawning[1] %in% c("fecundity_min", "fecundity_max", "temperature_max", "temperature_min")) {
        x <- x |>
          dplyr::group_by(species_id, spawning) |>
          dplyr::summarize(
            value = as.numeric(value) |>
              mean(na.rm = TRUE) |>
              as.character(),
            .groups = "keep"
          ) |>
          dplyr::ungroup()
      }
      x
    }) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = spawning, values_from = value) |>
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(c(
          "species_id", "fertilization", "mating_system", "spawning",
          "parental_care", "repro_mode", "spawning_frequency"
        )),
        as.numeric
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(reproductive_guilds),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)
      )
    ) |>
    dplyr::arrange(species_id)
  iid <- rowSums(spawning[, reproductive_guilds]) == 0
  spawning[iid, reproductive_guilds] <- NA


  # Eggs
  eggs <- input_files[grepl("species_traits-1.0.0/eggs.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  larvae <- input_files[grepl("species_traits-1.0.0/larvae.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    tidyr::pivot_wider(names_from = larvae, values_from = value) |>
    dplyr::mutate(
      duration = as.numeric(duration),
      duration_min = as.numeric(duration_min),
      duration_max = as.numeric(duration_max),
      temperature = as.numeric(temperature)
    ) |>
    dplyr::arrange(species_id)

  # Migration
  migration <- input_files[grepl("species_traits-1.0.0/migration.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = migration, values_from = value, values_fill = list(value = 0)) |>
    dplyr::arrange(species_id)

  # Morphology
  morphology <- input_files[grepl("species_traits-1.0.0/morphology.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::group_by(morphology) |>
    dplyr::group_split() |>
    lapply(function(x) {
      if (x$morphology[1] %in% c("maximum_length_cm")) {
        x <- x |>
          dplyr::group_by(species_id, morphology) |>
          dplyr::summarize(
            value = as.numeric(value) |>
              mean(na.rm = TRUE) |>
              as.character(),
            .groups = "keep"
          ) |>
          dplyr::ungroup()
      }
      x
    }) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = morphology, values_from = value) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "body_depth_percent_tl",
          "caudal_peduncle_throttling",
          "common_length_cm",
          "eye_size_percent_hl",
          "maximum_length_cm",
          "maximum_weight_kg",
          "pectoral_fin_size",
          "pectoral_fin_vertical_position",
          "vertical_eye_position"
        )),
        as.numeric
      )
    )


  tolerance_adult <- input_files[grepl("species_traits-1.0.0/tolerance_adult.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  tolerance_embryo <- input_files[grepl("species_traits-1.0.0/tolerance_embryo.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  tolerance_larvae <- input_files[grepl("species_traits-1.0.0/tolerance_larvae.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  tolerance_spawner <- input_files[grepl("species_traits-1.0.0/tolerance_spawner.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  taxonomy <- input_files[grepl("species_traits-1.0.0/taxonomy.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  picture <- input_files[grepl("species_traits-1.0.0/picture.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  swimming <- input_files[grepl("species_traits-1.0.0/swimming.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)


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

  # -----------------------------------------------------------------
  # Watersheds - species
  watersheds_species <- input_files[grepl("watersheds_species.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  # -----------------------------------------------------------------
  # Waterbodies - species
  waterbodies_species <- input_files[grepl("waterbodies_species.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)


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
  DBI::dbWriteTable(con, "migration_phenology", migration_phenology, overwrite = TRUE)
  DBI::dbWriteTable(con, "spawning_phenology", spawning_phenology, overwrite = TRUE)
  DBI::dbWriteTable(con, "larvae_phenology", larvae_phenology, overwrite = TRUE)
  DBI::dbWriteTable(con, "habitat_adult", habitat_adult, overwrite = TRUE)
  DBI::dbWriteTable(con, "habitat_juvenile", habitat_juvenile, overwrite = TRUE)
  DBI::dbWriteTable(con, "habitat_spawning", habitat_spawning, overwrite = TRUE)
  DBI::dbWriteTable(con, "habitat_yoy", habitat_yoy, overwrite = TRUE)
  DBI::dbWriteTable(con, "food_items", food_items, overwrite = TRUE)
  DBI::dbWriteTable(con, "spawning", spawning, overwrite = TRUE)
  DBI::dbWriteTable(con, "eggs", eggs, overwrite = TRUE)
  DBI::dbWriteTable(con, "larvae", larvae, overwrite = TRUE)
  DBI::dbWriteTable(con, "migration", migration, overwrite = TRUE)
  DBI::dbWriteTable(con, "morphology", morphology, overwrite = TRUE)
  DBI::dbWriteTable(con, "tolerance_adult", tolerance_adult, overwrite = TRUE)
  DBI::dbWriteTable(con, "tolerance_embryo", tolerance_embryo, overwrite = TRUE)
  DBI::dbWriteTable(con, "tolerance_larvae", tolerance_larvae, overwrite = TRUE)
  DBI::dbWriteTable(con, "tolerance_spawner", tolerance_spawner, overwrite = TRUE)
  DBI::dbWriteTable(con, "taxonomy", taxonomy, overwrite = TRUE)
  DBI::dbWriteTable(con, "picture", picture, overwrite = TRUE)
  DBI::dbWriteTable(con, "swimming", swimming, overwrite = TRUE)
  # DBI::dbWriteTable(con, "wua", wua, overwrite = TRUE)
  # DBI::dbWriteTable(con, "stressors", stressors, overwrite = TRUE)
  DBI::dbWriteTable(con, "waterbodies_species", waterbodies_species, overwrite = TRUE)
  DBI::dbWriteTable(con, "watersheds_species", watersheds_species, overwrite = TRUE)
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
    dm::dm_add_pk(table = "migration_phenology", "species_id") |>
    dm::dm_add_pk(table = "spawning_phenology", "species_id") |>
    dm::dm_add_pk(table = "larvae_phenology", "species_id") |>
    dm::dm_add_pk(table = "habitat_adult", "species_id") |>
    dm::dm_add_pk(table = "habitat_juvenile", "species_id") |>
    dm::dm_add_pk(table = "habitat_spawning", "species_id") |>
    dm::dm_add_pk(table = "habitat_yoy", "species_id") |>
    dm::dm_add_pk(table = "food_items", "species_id") |>
    dm::dm_add_pk(table = "spawning", "species_id") |>
    dm::dm_add_pk(table = "eggs", "species_id") |>
    dm::dm_add_pk(table = "larvae", "species_id") |>
    dm::dm_add_pk(table = "migration", "species_id") |>
    dm::dm_add_pk(table = "morphology", "species_id") |>
    dm::dm_add_pk(table = "tolerance_adult", "species_id") |>
    dm::dm_add_pk(table = "tolerance_embryo", "species_id") |>
    dm::dm_add_pk(table = "tolerance_larvae", "species_id") |>
    dm::dm_add_pk(table = "tolerance_spawner", "species_id") |>
    dm::dm_add_pk(table = "taxonomy", "species_id") |>
    dm::dm_add_pk(table = "picture", "species_id") |>
    dm::dm_add_pk(table = "swimming", "species_id") |>
    # dm::dm_add_pk(table = "wua", "wua_id") |>
    # dm::dm_add_pk(table = "stressors", "stressor_id") |>
    # dm::dm_add_pk(table = "waterbodies_species", "waterbody_species_id") |>
    dm::dm_add_fk(table = "waterbodies_species", "waterbody_id", "waterbodies") |>
    dm::dm_add_fk(table = "waterbodies_species", "species_id", "species") |>
    dm::dm_add_fk(table = "watersheds_species", "watershed_id", "watersheds") |>
    dm::dm_add_fk(table = "watersheds_species", "species_id", "species") |>
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
