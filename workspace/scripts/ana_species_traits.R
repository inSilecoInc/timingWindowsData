ana_species_traits <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/species_traits-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/ecology.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/eggdev.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/fecundity.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/fooditems.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/larvae_phenology.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/larvae_traits.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/larvaepresence_phenology.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/larvdyn.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/reproduc.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/spawning_phenology.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/spawning_traits.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/species.csv",
  #   "workspace/data/analyzed/fishbase-1.0.0/swimming.csv",
  #   "workspace/data/harvested/dahlke_2020-1.0.0/processed/experimental_imputed_tolerance.csv",
  #   "workspace/data/harvested/dahlke_2020-1.0.0/processed/thermal_responsiveness.csv",
  #   "workspace/data/harvested/dahlke_2020-1.0.0/processed/thermal_safety_margins.csv",
  #   "workspace/data/harvested/dahlke_2020-1.0.0/processed/thermal_tolerance.csv",
  #   "workspace/data/harvested/fishpass-1.0.0/processed/fishpass_behaviour.csv",
  #   "workspace/data/harvested/fishpass-1.0.0/processed/fishpass_morphology.csv",
  #   "workspace/data/harvested/fishpass-1.0.0/processed/fishpass_phenology.csv",
  #   "workspace/data/harvested/fishpass-1.0.0/processed/fishpass_physiology.csv",
  #   "workspace/data/harvested/ontario_freshwater_fishes_life_history-1.0.0/processed/ontario_fishes_characteristics.csv",
  #   "workspace/data/harvested/ontario_freshwater_fishes_life_history-1.0.0/processed/ontario_fishes_references.csv",
  #   "workspace/data/harvested/roberge_2002-1.0.0/processed/roberge.csv",
  #   "workspace/data/harvested/north_american_freshwater_migratory_fish_database-1.0.0/processed/north_american_freshwater_migratory_fish_database.csv"
  # )
  input_files <- unlist(input_files)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data sources

  # -------------------------------------------------------
  # Species list
  spList <- input_files[grepl("freshwater_fish_species_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    dplyr::select(species_id, species)

  # -------------------------------------------------------
  # Data tables
  dat <- list()

  # Iterate over input files and populate the dat list
  for (file in input_files) {
    # Extract database name
    db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
    db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version

    # Extract table name
    table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension

    # Load data using vroom
    suppressWarnings({
      df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
    })

    # Store in nested list
    if (!db_name %in% names(dat)) {
      dat[[db_name]] <- list()
    }
    dat[[db_name]][[table_name]] <- df
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Species id from species list for databases lacking that information
  species_join <- function(target_df, target_col, species_list = spList) {
    target_df |>
      dplyr::rename(species = eval(target_col)) |>
      dplyr::left_join(species_list, by = "species") |>
      dplyr::relocate(species_id) |>
      dplyr::filter(!is.na(species_id)) |>
      dplyr::select(-species)
  }

  # ------------------------------------------------
  # Dahlke 2020
  # experimental_imputed_tolerance
  dat$dahlke_2020$experimental_imputed_tolerance <-
    dat$dahlke_2020$experimental_imputed_tolerance |>
    species_join("species") |>
    dplyr::select(-reference)

  # thermal_responsiveness
  dat$dahlke_2020$thermal_responsiveness <-
    dat$dahlke_2020$thermal_responsiveness |>
    species_join("species") |>
    dplyr::select(-reference)

  # thermal_safety_margins
  dat$dahlke_2020$thermal_safety_margins <-
    dat$dahlke_2020$thermal_safety_margins |>
    species_join("species_fish_base") |>
    dplyr::select(-tree_of_life_name, -genus, -family, -order, -weight, -thermal_tolerance, -parameter) |>
    dplyr::rename(tmin = tmin_c, tmax = tmax_c, trange = trange_c) |>
    dplyr::mutate(lifestage = stringr::str_remove(lifestage, "^[^_]+_")) |>
    dplyr::group_by(species_id, realm, lifestage) |>
    dplyr::summarize(
      latitude = mean(latitude),
      tmin = sum(tmin, na.rm = TRUE),
      tmax = sum(tmax, na.rm = TRUE),
      trange = sum(trange, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # thermal_tolerance
  season_map <- list(
    "MJJ" = c("may", "jun", "jul"),
    "ASO" = c("aug", "sep", "oct"),
    "NDJ" = c("nov", "dec", "jan"),
    "FMA" = c("feb", "mar", "apr"),
    "all" = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  )
  expand_seasons <- function(season) {
    months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    selected_months <- season_map[[season]] # Get mapped months
    setNames(as.integer(months %in% selected_months), months) # Set column names before unnesting
  }

  dat$dahlke_2020$thermal_tolerance <-
    dat$dahlke_2020$thermal_tolerance |>
    dplyr::mutate(
      species = stringr::str_replace(species, "_", " ") |>
        stringr::str_to_sentence()
    ) |>
    species_join("species") |>
    dplyr::mutate(spawning_season = purrr::map(spawning_season, expand_seasons)) |>
    tidyr::unnest_wider(spawning_season)


  # ------------------------------------------------
  # Fishpass
  # fishpass_behaviour
  dat$fishpass$fishpass_behaviour <-
    dat$fishpass$fishpass_behaviour |>
    dplyr::select(scientific_name, vertical_station, schooling_behaviour) |>
    species_join("scientific_name")

  # fishpass_morphology
  dat$fishpass$fishpass_morphology <-
    dat$fishpass$fishpass_morphology |>
    dplyr::select(
      scientific_name, maximum_total_length_cm, body_depth_percent_tl, body_shape,
      aspect_ratio, caudal_peduncle_throttling, pectoral_fin_vertical_position,
      pectoral_fin_size, vertical_eye_position, eye_size_percent_hl
    ) |>
    species_join("scientific_name")

  # fishpass_phenology
  # Define mapping of seasons to months
  season_to_months <- list(
    "spring_spawner" = c("mar", "apr", "may"),
    "summer_spawner" = c("jun", "jul", "aug"),
    "fall_spawner"   = c("sep", "oct", "nov"),
    "winter_spawner" = c("dec", "jan", "feb")
  )

  # Function to expand seasonal spawners into monthly binary columns
  expand_seasonal_spawning <- function(row) {
    # Initialize all months as 0
    month_values <- stats::setNames(
      rep(0, 12),
      c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    )

    # Check each seasonal spawner column and fill in months accordingly
    purrr::walk(names(season_to_months), function(season) {
      if (row[[season]] == 1) {
        month_values[season_to_months[[season]]] <<- 1
      }
    })

    return(as.list(month_values))
  }

  df <- dat$fishpass$fishpass_phenology |>
    dplyr::select(
      scientific_name, migratory_status, spatial_scale_of_movement,
      spawning_frequency, spring_spawner, summer_spawner, fall_spawner,
      winter_spawner, x1_8_c, x9_16_c, x17_24_c, x25_32_c
    ) |>
    dplyr::rename(
      spawn_stimulus_1_8_degrees_celcius = x1_8_c,
      spawn_stimulus_9_16_degrees_celcius = x9_16_c,
      spawn_stimulus_17_24_degrees_celcius = x17_24_c,
      spawn_stimulus_25_32_degrees_celcius = x25_32_c
    ) |>
    species_join("scientific_name")

  # Identify non-seasonal columns
  non_seasonal_cols <- df |>
    dplyr::select(-spring_spawner, -summer_spawner, -fall_spawner, -winter_spawner)

  # Apply transformation while keeping non-seasonal columns
  seasonal_cols <- df |>
    dplyr::select(spring_spawner, summer_spawner, fall_spawner, winter_spawner) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(everything(), as.integer)) |>
    dplyr::mutate(monthly_data = list(expand_seasonal_spawning(dplyr::cur_data()))) |>
    tidyr::unnest_wider(monthly_data) |>
    dplyr::select(-spring_spawner, -summer_spawner, -fall_spawner, -winter_spawner)

  dat$fishpass$fishpass_phenology <- cbind(non_seasonal_cols, seasonal_cols)

  # fishpass_physiology
  dat$fishpass$fishpass_physiology <-
    dat$fishpass$fishpass_physiology |>
    dplyr::select(
      scientific_name, climbing_ability, hearing_specialization,
      lateral_line_trunk_placement, trophic_level,
      presence_of_ampullary_electroreceptors,
    ) |>
    species_join("scientific_name")


  # ------------------------------------------------
  # ontario_freshwater_fishes_life_history
  # Function to expand month spans into binary columns
  expand_spawning_months <- function(month_string) {
    # Initialize all months as 0 using built-in month abbreviations
    month_values <- stats::setNames(rep(0, 12), month.abb)

    # Extract start and end months
    months_present <- stringr::str_split(month_string, "-")[[1]] |>
      stringr::str_trim() # Remove leading/trailing spaces

    # Convert to Date format using lubridate (assign base year 2000)
    start_month <- lubridate::dmy(paste0("01-", months_present[1], "-2000"))
    end_month <- lubridate::dmy(paste0("01-", months_present[length(months_present)], "-2000"))

    # Adjust end year if the range spans into the next year
    if (lubridate::month(start_month) > lubridate::month(end_month)) {
      end_month <- lubridate::dmy(paste0("01-", months_present[length(months_present)], "-2001"))
    }

    # Generate full month sequence (handling year wrap)
    month_seq <- seq(start_month, end_month, by = "1 month") |>
      lubridate::month(label = TRUE, abbr = TRUE) |>
      as.character()

    # Assign 1 to identified months
    month_values[month_seq] <- 1

    return(as.list(month_values))
  }

  dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics <-
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
    dplyr::select(
      -family, -species, -taxonomic_authority, -common_name_s, -french_name,
      -species_code, -family_tsn, -parent_tsn, -species_tsn
    ) |>
    species_join("species_name") |>
    dplyr::mutate(monthly_data = purrr::map(spawning_month_s, expand_spawning_months)) |>
    tidyr::unnest_wider(monthly_data) |>
    janitor::clean_names()

  # ------------------------------------------------
  # roberge_2002
  dat$roberge_2002$roberge <-
    dat$roberge_2002$roberge |>
    dplyr::select(-name, -migration_strategy) |>
    species_join("scientificname")

  dat$roberge_2002$roberge_phenology <- dat$roberge_2002$roberge |>
    dplyr::filter(
      characteristics == "Duration",
      life_stage == "Spawning",
      value != "-"
    ) |>
    dplyr::select(-characteristics) |>
    dplyr::mutate(value = stringr::str_replace_all(
      value,
      c(
        "\\bfall-January\\b" = "October-January",
        "\\blate summer- early fall\\b" = "August-October",
        "\\bMay-July, peak June\\b" = "May-July",
        "\\bspring-early summer\\b" = "April-July",
        "\\bspring-August\\b" = "April-August",
        "\\bspring-summer\\b" = "April-September",
        "\\bsummer-fall\\b" = "July-December",
        "\\bfall\\b" = "October-December"
      )
    )) |>
    dplyr::mutate(monthly_data = purrr::map(value, expand_spawning_months)) |>
    tidyr::unnest_wider(monthly_data) |>
    janitor::clean_names() |>
    dplyr::select(-life_stage, -value)

  # ------------------------------------------------
  # north_american_freshwater_migratory_fish_database
  dat$north_american_freshwater_migratory_fish_database$north_american_freshwater_migratory_fish_database <-
    dat$north_american_freshwater_migratory_fish_database$north_american_freshwater_migratory_fish_database |>
    dplyr::select(
      -itis_family, -itis_genus, -itis_species, -family_name, -genus_name,
      -common_name, -undetermined, -reference, -reference_assignment
    ) |>
    species_join("scientific_name") |>
    dplyr::distinct()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Traits tables
  # -------------------------------------------------------
  # habitat table
  # Fishbase
  fb <- dplyr::bind_rows(
    dat$fishbase$ecology |>
      dplyr::select(-diet_troph, -food_troph, -fb_table) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "habitat", values_to = "value") |>
      dplyr::filter(value == 1),
    dat$fishbase$species |>
      dplyr::select(species_id, freshwater = fresh, brackish = brack, saltwater) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "habitat", values_to = "value") |>
      dplyr::filter(value == 1)
  ) |>
    dplyr::mutate(life_stage = "adult")

  # Roberge
  rb <- dplyr::bind_rows(
    dat$roberge_2002$roberge |>
      dplyr::filter(characteristics == "Stream/Ocean/Lake") |>
      dplyr::mutate(
        life_stage = tolower(life_stage),
        oceanic = stringr::str_detect(value, "O") |> as.numeric(),
        stream = stringr::str_detect(value, "S") |> as.numeric(),
        lakes = stringr::str_detect(value, "L") |> as.numeric()
      ) |>
      dplyr::select(species_id, life_stage, oceanic, stream, lakes) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(cols = -c("species_id", "life_stage"), names_to = "habitat", values_to = "value") |>
      dplyr::filter(value == 1),
    dat$roberge_2002$roberge |>
      dplyr::filter(
        characteristics %in% c("Dominant Substrate", "Dominant Habitat"),
        value != "-"
      ) |>
      dplyr::select(-characteristics) |>
      tidyr::separate_rows(value, sep = "\\s*(,|\\sand\\s|-)\\s*") |>
      dplyr::rename(habitat = value) |>
      dplyr::mutate(value = 1) |>
      dplyr::distinct() |>
      dplyr::mutate(habitat = stringr::str_replace_all(
        habitat,
        c(
          "\\bmainchannel\\b" = "channel",
          "\\bmain channel\\b" = "channel",
          "\\bback channel\\b" = "channel",
          "\\bside channel\\b" = "channel",
          "\\bchannels\\b" = "channel",
          "\\blarge rock\\b" = "rocky",
          "\\blarge rocks\\b" = "rocky",
          "\\blarger than gravel\\b" = "gravel",
          "\\blarge gravel\\b" = "gravel",
          "\\briver margin\\b" = "margins",
          "\\briver margins\\b" = "margins",
          "\\brocks\\b" = "rocky",
          "\\bclean uniform rock\\b" = "rocky",
          "\\bgroundwater upwelling\\b" = "upwelling",
          "\\bcourse gravel to sand\\b" = "gravel",
          "\\bcourse sand\\b" = "sand",
          "\\bhead of riffles\\b" = "riffle",
          "\\bmarshy areas\\b" = "marshes_swamps",
          "\\borganic debris\\b" = "organic",
          "\\bmargins\\b" = "margin",
          "\\bsmooth stone\\b" = "rocky",
          "\\brock\\b" = "rocky",
          "\\bback eddies\\b" = "eddy"
        )
      )) |>
      dplyr::filter(!habitat %in% c("none", "fast", "off", "rapid", "various sizes", "large substrate"))
  )

  # Fishpass
  fp <- dat$fishpass$fishpass_behaviour |>
    dplyr::select(species_id, habitat = vertical_station) |>
    dplyr::mutate(value = 1) |>
    dplyr::mutate(life_stage = "adult")

  # Ontario freshwater fishes
  off <- dplyr::bind_rows(
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(species_id, general_habitat_s, environment) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "var", values_to = "habitat") |>
      tidyr::separate_rows(habitat, sep = "\\s*(;|\\s|,)\\s*") |>
      dplyr::mutate(life_stage = "adult"),
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(species_id, spawning_habitat_s) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "var", values_to = "habitat") |>
      tidyr::separate_rows(habitat, sep = "\\s*(;|\\s|,)\\s*") |>
      dplyr::mutate(life_stage = "spawning"),
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(species_id, nursery_habitat_s) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "var", values_to = "habitat") |>
      tidyr::separate_rows(habitat, sep = "\\s*(;|\\s|,)\\s*") |>
      dplyr::mutate(life_stage = "yoy")
  ) |>
    dplyr::mutate(
      habitat = stringr::str_replace(habitat, "1", ""),
      value = 1
    ) |>
    dplyr::select(-var) |>
    dplyr::distinct()

  # Bind and distinct
  habitat <- dplyr::bind_rows(fb, rb, fp, off) |>
    dplyr::mutate(life_stage = tolower(life_stage)) |>
    dplyr::filter(life_stage != "ammocoete") |>
    dplyr::distinct() |>
    dplyr::group_by(life_stage) |>
    dplyr::group_split()
  names(habitat) <- lapply(habitat, function(x) x$life_stage[1]) |> unlist()


  # -------------------------------------------------------
  # food_items table
  food_items <- dat$fishbase$fooditems |>
    dplyr::select(-country, -longitude, -latitude, -fb_table) |>
    dplyr::distinct()

  # -------------------------------------------------------
  # spawning table
  split_range_column <- function(df, col_name) {
    df |>
      tidyr::separate_wider_delim(
        col = col_name, # Column to split
        delim = "-",
        names = c(paste0(col_name, "_min"), paste0(col_name, "_max")),
        cols_remove = TRUE,
        too_few = "align_start"
      )
  }

  spawning <- dplyr::bind_rows(
    dat$fishbase$fecundity |>
      dplyr::select(-fb_table) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "spawning", values_to = "value") |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      na.omit(),
    dat$fishbase$reproduc |>
      dplyr::select(-fb_table) |>
      dplyr::mutate(
        batch_spawner = abs(batch_spawner),
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "spawning", values_to = "value") |>
      na.omit(),
    dat$fishbase$spawning_traits |>
      dplyr::select(
        species_id, fecundity_min, fecundity_max, spawning_cycles,
        coastal, lacustrine, riverine, estuarine
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "spawning", values_to = "value") |>
      na.omit(),
    dat$fishpass$fishpass_phenology |>
      dplyr::select(
        species_id, spawning_frequency, spawn_stimulus_1_8_degrees_celcius,
        spawn_stimulus_9_16_degrees_celcius, spawn_stimulus_17_24_degrees_celcius,
        spawn_stimulus_25_32_degrees_celcius
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "spawning", values_to = "value") |>
      na.omit(),
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(
        species_id, reproductive_guild, spawning_habitat_s,
        spawning_temperature_c, fecundity
      ) |>
      dplyr::rename(
        habitat = spawning_habitat_s,
        temperature = spawning_temperature_c
      ) |>
      dplyr::mutate(
        temperature = dplyr::if_else(temperature == "no data", NA, temperature),
        temperature = stringr::str_replace_all(temperature, "\\+", ""),
        fecundity = stringr::str_replace_all(fecundity, ",", "")
      ) |>
      split_range_column("temperature") |>
      split_range_column("fecundity") |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "spawning", values_to = "value") |>
      tidyr::separate_rows(value, sep = ": ") |>
      dplyr::distinct() |>
      na.omit()
  ) |>
    dplyr::distinct()

  # -------------------------------------------------------
  # eggs table
  eggs <- dplyr::left_join(
    # Species list
    spList |>
      dplyr::select(species_id),

    # Egg data
    dat$fishbase$eggdev |>
      dplyr::select(-fb_table),

    # Join by
    by = "species_id"
  )

  # -------------------------------------------------------
  # larvae table
  larvae <- dplyr::bind_rows(
    dat$fishbase$larvae_traits |>
      dplyr::select(-fb_table) |>
      dplyr::rename(
        duration_min = larval_duration_min,
        duration_max = larval_duration_max,
        duration = larval_duration_mod
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "larvae", values_to = "value") |>
      na.omit(),
    dat$fishbase$larvdyn |>
      dplyr::select(-ecosystem, -fb_table) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "larvae", values_to = "value") |>
      na.omit()
  )

  # -------------------------------------------------------
  # migration table
  migration <- dplyr::bind_rows(
    dat$fishbase$species |>
      dplyr::select(species_id, migration = ana_cat) |>
      na.omit() |>
      dplyr::mutate(value = 1),
    dat$fishpass$fishpass_phenology |>
      dplyr::select(species_id, migratory_status, spatial_scale_of_movement) |>
      dplyr::mutate(
        migratory_status = stringr::str_replace_all(
          migratory_status,
          c("both" = "migratory_and_non_migratory", "nonmigratory" = "non_migratory")
        ),
        spatial_scale_of_movement = stringr::str_replace_all(
          spatial_scale_of_movement,
          c("both" = "diadromous_and_potamodromous", "nonmigratory" = "non_migratory")
        )
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "val", values_to = "migration") |>
      na.omit() |>
      dplyr::filter(migration != "undetermined") |>
      dplyr::select(-val) |>
      dplyr::mutate(value = 1),
    dat$north_american_freshwater_migratory_fish_database$north_american_freshwater_migratory_fish_database |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "migration", values_to = "value") |>
      na.omit() |>
      dplyr::filter(value == 1)
  ) |>
    dplyr::distinct()

  # -------------------------------------------------------
  # morphology table
  morphology <- dplyr::bind_rows(
    dat$fishbase$species |>
      dplyr::select(
        species_id,
        body_shape = body_shape_i, air_breathing, maximum_length_cm = length,
        common_length_cm = common_length, maximum_weight_kg = weight
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "morphology", values_to = "value") |>
      na.omit(),
    dat$fishpass$fishpass_morphology |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "morphology", values_to = "value") |>
      na.omit(),
    dat$fishpass$fishpass_physiology |>
      dplyr::select(-trophic_level) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "morphology", values_to = "value") |>
      na.omit(),
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(
        species_id, adult_length_cm, adult_weight_kg, maximum_length_cm,
        maximum_weight_kg, record_length_cm, record_weight_kg
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), as.character)
      ) |>
      tidyr::pivot_longer(cols = -c("species_id"), names_to = "morphology", values_to = "value") |>
      na.omit()
  ) |>
    dplyr::distinct()


  # -------------------------------------------------------
  # tolerance table
  tolerance <- dat$dahlke_2020$thermal_safety_margins |>
    dplyr::select(-realm) |>
    dplyr::mutate(lifestage = tolower(lifestage)) |>
    dplyr::group_by(lifestage) |>
    dplyr::group_split()
  names(tolerance) <- lapply(tolerance, function(x) x$lifestage[1]) |> unlist()


  # -------------------------------------------------------
  # taxonomy table
  taxonomy <- dat$freshwater_fish_canada$freshwater_fish_species_canada |>
    dplyr::select(species_id, family, order, species) |>
    tidyr::separate_wider_delim(
      col = species,
      delim = " ",
      names = c("genus", "species"),
      cols_remove = TRUE
    )

  # -------------------------------------------------------
  # picture table
  picture <- dplyr::left_join(
    # Species list
    spList |>
      dplyr::select(species_id),

    # Egg data
    dat$fishbase$species |>
      dplyr::select(species_id, picture_url = pic_preferred_name),

    # Join by
    by = "species_id"
  )

  # -------------------------------------------------------
  # swimming_behavior table
  swimming <- dplyr::left_join(
    # Species list
    spList |>
      dplyr::select(species_id),

    # Egg data
    dat$fishbase$swimming |>
      dplyr::select(-fb_table),

    # Join by
    by = "species_id"
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export
  vroom::vroom_write(habitat$adult, file.path(output_path, "habitat_adult.csv"), delim = ",")
  vroom::vroom_write(habitat$juvenile, file.path(output_path, "habitat_juvenile.csv"), delim = ",")
  vroom::vroom_write(habitat$spawning, file.path(output_path, "habitat_spawning.csv"), delim = ",")
  vroom::vroom_write(habitat$yoy, file.path(output_path, "habitat_yoy.csv"), delim = ",")
  vroom::vroom_write(food_items, file.path(output_path, "food_items.csv"), delim = ",")
  vroom::vroom_write(spawning, file.path(output_path, "spawning.csv"), delim = ",")
  vroom::vroom_write(eggs, file.path(output_path, "eggs.csv"), delim = ",")
  vroom::vroom_write(larvae, file.path(output_path, "larvae.csv"), delim = ",")
  vroom::vroom_write(migration, file.path(output_path, "migration.csv"), delim = ",")
  vroom::vroom_write(morphology, file.path(output_path, "morphology.csv"), delim = ",")
  vroom::vroom_write(tolerance$adult, file.path(output_path, "tolerance_adult.csv"), delim = ",")
  vroom::vroom_write(tolerance$embryo, file.path(output_path, "tolerance_embryo.csv"), delim = ",")
  vroom::vroom_write(tolerance$larvae, file.path(output_path, "tolerance_larvae.csv"), delim = ",")
  vroom::vroom_write(tolerance$spawner, file.path(output_path, "tolerance_spawner.csv"), delim = ",")
  vroom::vroom_write(taxonomy, file.path(output_path, "taxonomy.csv"), delim = ",")
  vroom::vroom_write(picture, file.path(output_path, "picture.csv"), delim = ",")
  vroom::vroom_write(swimming, file.path(output_path, "swimming.csv"), delim = ",")
}
