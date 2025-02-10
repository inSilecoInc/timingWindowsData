ana_species_phenology <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/species_phenology-1.0.0/"
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
  #   "workspace/data/harvested/north_american_freshwater_migratory_fish_database-1.0.0/processed/north_american_freshwater_migratory_fish_database.csv",
  #   "workspace/data/analyzed/species_traits-1.0.0/migration.csv"
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
  suppressWarnings({
    seasonal_cols <- df |>
      dplyr::select(spring_spawner, summer_spawner, fall_spawner, winter_spawner) |>
      dplyr::rowwise() |>
      dplyr::mutate(dplyr::across(everything(), as.integer)) |>
      dplyr::mutate(monthly_data = list(expand_seasonal_spawning(dplyr::cur_data()))) |>
      tidyr::unnest_wider(monthly_data) |>
      dplyr::select(-spring_spawner, -summer_spawner, -fall_spawner, -winter_spawner)
  })

  dat$fishpass$fishpass_phenology <- cbind(non_seasonal_cols, seasonal_cols)


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
  # Phenology tables

  # -------------------------------------------------------
  # Function to summarize spawning data
  summarize_spawning <- function(df) {
    north_america <- c("USA", "Canada", "Mexico", "Alaska")

    df |>
      dplyr::group_by(species_id) |> # Group by species
      dplyr::group_modify(~ {
        if (nrow(.x) == 1) {
          return(.x) # Case 1: Use the single row as-is
        }

        # Case 2: Filter for rows in North America
        na_rows <- .x |> dplyr::filter(country %in% north_america)

        if (nrow(na_rows) > 0) {
          .x <- na_rows # Case 2a: Keep only North America rows
        }

        # Case 2b: Aggregate the remaining rows into a single row
        summarized_row <- .x |>
          dplyr::select(-country, -longitude, -latitude) |> # Remove location columns
          dplyr::summarize(dplyr::across(everything(), \(x) sum(x, na.rm = TRUE)))

        return(summarized_row)
      }) |>
      dplyr::ungroup()
  }

  # spawning table
  spawning <- dplyr::bind_rows(
    dat$fishbase$spawning_phenology |>
      dplyr::mutate(db = "fishbase"),
    dat$dahlke_2020$thermal_tolerance |>
      dplyr::select(-realm, -depth_spawners, -depths_embryos) |>
      dplyr::mutate(db = "dahlke"),
    dat$fishpass$fishpass_phenology |>
      dplyr::select(species_id, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) |>
      dplyr::mutate(db = "fishpass"),
    dat$ontario_freshwater_fishes_life_history$ontario_fishes_characteristics |>
      dplyr::select(species_id, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) |>
      dplyr::mutate(db = "offlh"),
    dat$roberge_2002$roberge_phenology |>
      dplyr::mutate(db = "roberge")
  ) |>
    dplyr::select(-fb_table, -db) |>
    summarize_spawning() |>
    dplyr::select(-country, -longitude, -latitude) |>
    dplyr::distinct()

  # -------------------------------------------------------
  # Define migratory species (excluding non-migratory)
  migratory_terms <- c(
    "anadromous", "potamodromous", "amphidromous",
    "catadromous", "oceanodromous", "migratory", "diadromous",
    "diadromous_and_potamodromous", "semi_anadromous",
    "suspected_migrant", "multiple_diadromous",
    "anadromous_semianadromous", "migratory_and_non_migratory"
  )

  # Filter only migratory species
  migratory_species <- dat$species_traits$migration |>
    dplyr::filter(migration %in% migratory_terms) |>
    dplyr::select(species_id) |>
    dplyr::distinct()

  # Merge with spawning table to get only migratory species' spawning timing
  spawning_migratory <- spawning |>
    dplyr::filter(species_id %in% migratory_species$species_id)


  # Function to create migration timing
  define_migration_period <- function(df) {
    df |>
      dplyr::group_by(species_id) |>
      dplyr::mutate(
        # Pre-spawning migration: Shift months earlier
        jan = dplyr::lead(jan, default = feb),
        feb = dplyr::lead(feb, default = mar),
        mar = dplyr::lead(mar, default = apr),
        apr = dplyr::lead(apr, default = may),
        may = dplyr::lead(may, default = jun),
        jun = dplyr::lead(jun, default = jul),
        jul = dplyr::lead(jul, default = aug),
        aug = dplyr::lead(aug, default = sep),
        sep = dplyr::lead(sep, default = oct),
        oct = dplyr::lead(oct, default = nov),
        nov = dplyr::lead(nov, default = dec),
        dec = dplyr::lead(dec, default = jan) # ,

        # # Post-spawning migration: Shift months later
        # jan_post = dplyr::lag(jan, default = dec),
        # feb_post = dplyr::lag(feb, default = jan),
        # mar_post = dplyr::lag(mar, default = feb),
        # apr_post = dplyr::lag(apr, default = mar),
        # may_post = dplyr::lag(may, default = apr),
        # jun_post = dplyr::lag(jun, default = may),
        # jul_post = dplyr::lag(jul, default = jun),
        # aug_post = dplyr::lag(aug, default = jul),
        # sep_post = dplyr::lag(sep, default = aug),
        # oct_post = dplyr::lag(oct, default = sep),
        # nov_post = dplyr::lag(nov, default = oct),
        # dec_post = dplyr::lag(dec, default = nov),
      ) |>
      dplyr::ungroup() #|>
    # dplyr::select(species_id, jan_pre:dec_pre) # , jan_post:dec_post) # Keep migration timing
  }

  # Apply migration timing definition
  migration <- spawning_migratory |> define_migration_period()

  # Convert to final format by summing pre and post-migration periods
  # migration_table_final <- migration_table |>
  #   dplyr::mutate(
  #     jan = pmax(jan_pre, jan_post),
  #     feb = pmax(feb_pre, feb_post),
  #     mar = pmax(mar_pre, mar_post),
  #     apr = pmax(apr_pre, apr_post),
  #     may = pmax(may_pre, may_post),
  #     jun = pmax(jun_pre, jun_post),
  #     jul = pmax(jul_pre, jul_post),
  #     aug = pmax(aug_pre, aug_post),
  #     sep = pmax(sep_pre, sep_post),
  #     oct = pmax(oct_pre, oct_post),
  #     nov = pmax(nov_pre, nov_post),
  #     dec = pmax(dec_pre, dec_post)
  #   ) |>
  #   dplyr::select(species_id, jan:dec) # Keep only final migration columns



  # -------------------------------------------------------
  # larvae table
  # larvae <- dplyr::bind_rows(
  #   dat$fishbase$larvae_phenology,
  #   dat$fishbase$larvaepresence_phenology |>
  #     dplyr::rename(locality = country)
  # )

  # Function to create larvae timing based on spawning data
  define_larvae_period <- function(df) {
    df |>
      dplyr::mutate(
        # Identify last spawning month (column with the highest nonzero value)
        last_spawning_month = apply(df[, 2:13], 1, function(x) max(which(x > 0), na.rm = TRUE)),

        # Compute next month index (wrapping around December to January)
        next_larvae_month = ifelse(last_spawning_month == 12, 1, last_spawning_month + 1)
      ) |>
      dplyr::rowwise() |> # Row-wise mutation to assign binary values
      dplyr::mutate(
        # Create a larvae timing table, marking last spawning and next month
        jan = as.integer(last_spawning_month == 1 | next_larvae_month == 1),
        feb = as.integer(last_spawning_month == 2 | next_larvae_month == 2),
        mar = as.integer(last_spawning_month == 3 | next_larvae_month == 3),
        apr = as.integer(last_spawning_month == 4 | next_larvae_month == 4),
        may = as.integer(last_spawning_month == 5 | next_larvae_month == 5),
        jun = as.integer(last_spawning_month == 6 | next_larvae_month == 6),
        jul = as.integer(last_spawning_month == 7 | next_larvae_month == 7),
        aug = as.integer(last_spawning_month == 8 | next_larvae_month == 8),
        sep = as.integer(last_spawning_month == 9 | next_larvae_month == 9),
        oct = as.integer(last_spawning_month == 10 | next_larvae_month == 10),
        nov = as.integer(last_spawning_month == 11 | next_larvae_month == 11),
        dec = as.integer(last_spawning_month == 12 | next_larvae_month == 12)
      ) |>
      dplyr::select(species_id, jan:dec) # Keep only final larvae columns
  }

  # Apply the function to generate larvae season table
  larvae <- spawning |> define_larvae_period()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export
  vroom::vroom_write(spawning, file.path(output_path, "spawning.csv"), delim = ",")
  vroom::vroom_write(migration, file.path(output_path, "migration.csv"), delim = ",")
  vroom::vroom_write(larvae, file.path(output_path, "larvae.csv"), delim = ",")
}
