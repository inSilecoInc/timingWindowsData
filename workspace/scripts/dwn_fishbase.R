download_fishbase <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/fishbase-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv"
  # )
  input_files <- unlist(input_files)


  #  --------------------------------------------------
  # Species list
  species <- input_files[grepl("freshwater_fish_species_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)

  # tbl <- rfishbase::fb_tables(server = "fishbase")
  # prefix for pictures: https://www.fishbase.se/images/thumbnails/jpg/tn_

  #  --------------------------------------------------
  fetch_fishbase_data <- function(tables) {
    fishbase_data <- setNames(lapply(tables, function(tbl) {
      df <- tryCatch(
        rfishbase::fb_tbl(tbl), # Fetch table data from FishBase
        error = function(e) NULL # Handle errors (e.g., if table is unavailable)
      )

      return(df)
    }), tables)

    return(fishbase_data)
  }

  # Relevant FishBase tables
  tables <- c(
    "species", "fecundity", "spawning", "reproduc",
    "eggdev", "larvae", "larvaepresence", "larvdyn",
    "fooditems", "ecology", "swimming"
  )


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch data and begin formatting
  fishbase <- fetch_fishbase_data(tables) # Run only once to explore the data
  # temporary_fishbase <- fishbase
  # fishbase <- temporary_fishbase
  spList <- species$species

  # Filter species table as a reference for all others
  fishbase$species <- fishbase$species |>
    janitor::clean_names() |>
    dplyr::mutate(scientific = glue::glue("{genus} {species}")) |>
    dplyr::filter(scientific %in% spList)

  # Filter all other tables
  fishbase <- lapply(fishbase, function(x) {
    x <- x |>
      janitor::clean_names() |>
      janitor::clean_names(replace = c(`Speccode` = "spec_code")) |>
      dplyr::filter(spec_code %in% fishbase$species$spec_code)
  })
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Format each table
  # ----------------------------
  # Species
  fishbase$species <- fishbase$species |>
    dplyr::select(
      spec_code, scientific, pic_preferred_name, body_shape_i, fresh, brack,
      saltwater, demers_pelag, air_breathing, ana_cat, longevity_wild,
      length, common_length, weight, importance, game_fish
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), tolower)) |>
    dplyr::mutate(
      body_shape_i = stringr::str_replace_all(body_shape_i, "fusiform / normal", "fusiform"),
      body_shape_i = stringr::str_replace_all(body_shape_i, "short and / or deep", "short or deep"),
      scientific = stringr::str_to_sentence(scientific)
    ) |>
    dplyr::mutate(fb_table = "species")



  # ----------------------------
  # fecundity
  fishbase$fecundity <- fishbase$fecundity |>
    dplyr::select(spec_code, c_code, fecundity_min, fecundity_max) |>
    dplyr::select(-c_code) |> # This is redundant, but I wish to keep trace of this column
    dplyr::group_by(spec_code) |>
    dplyr::summarize(
      fecundity_min = mean(fecundity_min, na.rm = TRUE),
      fecundity_max = mean(fecundity_max, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!(is.na(fecundity_min) & is.na(fecundity_max))) |>
    dplyr::mutate(fb_table = "fecundity")


  # ----------------------------
  # spawning
  # --- traits
  fishbase$spawning_traits <- dplyr::full_join(
    # Spawning information table
    fishbase$spawning |>
      dplyr::select(
        spec_code, spawning_ground, c_code, temp_low, temp_high,
        fecundity_min, fecundity_max, spawning_cycles
      ) |>
      dplyr::select(-c_code) |> # This is redundant, but I wish to keep trace of this column
      dplyr::group_by(spec_code) |>
      dplyr::summarize(
        temp_low = mean(temp_low, na.rm = TRUE),
        temp_high = mean(temp_high, na.rm = TRUE),
        fecundity_min = mean(fecundity_min, na.rm = TRUE),
        fecundity_max = mean(fecundity_max, na.rm = TRUE),
        spawning_cycles = mean(spawning_cycles, na.rm = TRUE)
      ) |>
      dplyr::ungroup(),

    # Spawning grounds table
    fishbase$spawning |>
      dplyr::select(spec_code, spawning_ground) |>
      dplyr::mutate(value = 1) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = spec_code,
        names_from = spawning_ground,
        values_from = value,
        values_fill = list(value = 0)
      ) |>
      dplyr::select(-`NA`) |>
      dplyr::filter(rowSums(dplyr::across(-spec_code)) > 0),

    # Join by
    by = "spec_code"
  ) |>
    dplyr::mutate(fb_table = "spawning")

  # --- phenology
  fishbase$spawning_phenology <- fishbase$spawning |>
    dplyr::select(
      spec_code, c_code, jan, feb, mar, apr,
      may, jun, jul, aug, sep, oct, nov, dec
    ) |>
    dplyr::mutate(dplyr::across(
      -c(spec_code, c_code), # Exclude these columns
      ~ dplyr::case_when(
        . == 111 ~ 1,
        is.na(.) ~ 0,
        .default = .
      )
    )) |>
    dplyr::filter(rowSums(dplyr::across(-c(spec_code, c_code))) > 0) |>
    dplyr::mutate(fb_table = "spawning")

  # --- remove original table from list
  fishbase$spawning <- NULL

  # ----------------------------
  # reproduc
  fishbase$reproduc <- fishbase$reproduc |>
    dplyr::select(
      spec_code, repro_mode, fertilization, mating_system, spawning,
      batch_spawner, rep_guild1, rep_guild2, parental_care
    ) |>
    dplyr::mutate(fb_table = "reproduc")

  # ----------------------------
  # eggdev
  fishbase$eggdev <- dplyr::left_join(
    # Temperature
    fishbase$eggdev |>
      dplyr::select(spec_code, temperature) |>
      dplyr::group_by(spec_code) |>
      dplyr::summarize(temperature = mean(temperature, na.rm = TRUE)) |>
      dplyr::ungroup(),

    # Salinity
    fishbase$eggdev |>
      dplyr::select(spec_code, salinity2) |>
      dplyr::mutate(value = 1) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = spec_code,
        names_from = salinity2,
        values_from = value,
        values_fill = list(value = 0)
      ) |>
      dplyr::select(-`NA`) |>
      dplyr::filter(rowSums(dplyr::across(-spec_code)) > 0),

    # Join by
    by = "spec_code"
  ) |>
    dplyr::mutate(fb_table = "eggdev")


  # ----------------------------
  # larvae
  # --- traits
  fishbase$larvae_traits <- fishbase$larvae |>
    dplyr::select(
      spec_code, placeof_development, larval_duration_min, larval_duration_max,
      larval_duration_mod, shapeofyolksac, body_form
    ) |>
    dplyr::mutate(body_form = dplyr::if_else(body_form == "eel-like", "elongated", body_form)) |>
    dplyr::distinct() |>
    dplyr::mutate(fb_table = "larvae")

  # --- phenology
  fishbase$larvae_phenology <- fishbase$larvae |>
    dplyr::select(
      spec_code,
      locality = larval_area, jan_larv, feb_larv, mar_larv, apr_larv, may_larv,
      jun_larv, jul_larv, aug_larv, sep_larv, oct_larv, nov_larv, dec_larv
    ) |>
    dplyr::rename_with(~ sub("_larv$", "", .x)) |>
    dplyr::mutate(dplyr::across(
      -c(spec_code, locality), # Exclude these columns
      ~ dplyr::case_when(
        . == -1 ~ 1,
        is.na(.) ~ 0,
        .default = .
      )
    )) |>
    dplyr::filter(rowSums(dplyr::across(-c(spec_code, locality))) > 0) |>
    dplyr::mutate(fb_table = "larvae")

  # --- remove original table from list
  fishbase$larvae <- NULL

  # ----------------------------
  # larvaepresence
  fishbase$larvaepresence_phenology <- fishbase$larvaepresence |>
    dplyr::select(spec_code, c_code, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) |>
    dplyr::mutate(dplyr::across(
      -c(spec_code, c_code), # Exclude these columns
      ~ dplyr::case_when(
        . == 111 ~ 1,
        is.na(.) ~ 0,
        .default = .
      )
    )) |>
    dplyr::filter(rowSums(dplyr::across(-c(spec_code, c_code))) > 0) |>
    dplyr::mutate(fb_table = "larvaepresence")

  # --- remove original table from list
  fishbase$larvaepresence <- NULL

  # ----------------------------
  # larvdyn
  fishbase$larvdyn <- fishbase$larvdyn |>
    dplyr::select(spec_code, ecosystem, temperature, duration) |>
    dplyr::mutate(fb_table = "larvdyn")


  # ----------------------------
  # fooditems
  fishbase$fooditems <- fishbase$fooditems |>
    dplyr::select(spec_code, c_code, food_i, food_ii, food_iii, prey_stage, predator_stage) |>
    dplyr::mutate(fb_table = "fooditems")

  # ----------------------------
  # ecology
  fishbase$ecology <- fishbase$ecology |>
    dplyr::select(
      spec_code, neritic, supra_littoral_zone, saltmarshes, littoral_zone, tide_pools,
      intertidal, sub_littoral, caves, oceanic, epipelagic, mesopelagic, bathypelagic,
      abyssopelagic, hadopelagic, estuaries, mangroves, marshes_swamps, cave_anchialine,
      stream, lakes, cave, cave2, diet_troph, food_troph,
      soft_bottom, sand, coarse, fine, level, sloping, silt, mud, ooze, detritus,
      organic, hard_bottom, rocky, rubble, gravel, vegetation, driftwood
    ) |>
    dplyr::relocate(spec_code, diet_troph, food_troph) |>
    dplyr::mutate(dplyr::across(
      -c(spec_code, diet_troph, food_troph), # Exclude these columns
      ~ dplyr::case_when(
        . == -1 ~ 1,
        is.na(.) ~ 0,
        .default = .
      )
    )) |>
    dplyr::filter(rowSums(dplyr::across(-c(spec_code, diet_troph, food_troph))) > 0) |>
    dplyr::group_by(spec_code) |>
    dplyr::summarise(
      diet_troph = mean(diet_troph, na.rm = TRUE),
      food_troph = mean(food_troph, na.rm = TRUE),
      dplyr::across(
        -c(diet_troph, food_troph),
        ~ as.integer(sum(.x, na.rm = TRUE) >= 1)
      )
    ) |>
    dplyr::mutate(fb_table = "ecology")


  # ----------------------------
  fishbase$swimming <- fishbase$swimming |>
    dplyr::select(spec_code, adult_type, adult_mode) |>
    dplyr::mutate(fb_table = "swimming")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add species names from species list
  for (i in names(fishbase)[!names(fishbase) %in% "species"]) {
    fishbase[[i]] <- dplyr::left_join(
      fishbase[[i]], fishbase$species[, c("spec_code", "scientific")],
      by = "spec_code"
    )
  }

  # Add species id
  fishbase <- fishbase |>
    lapply(function(x) {
      x <- x |>
        dplyr::left_join(
          species[, c("species_id", "species")],
          by = c("scientific" = "species")
        ) |>
        dplyr::relocate(species_id) |>
        dplyr::select(-spec_code, -scientific)
    })

  # Add country
  fishbase <- fishbase |>
    lapply(function(x) {
      if ("c_code" %in% colnames(x)) {
        x |>
          dplyr::left_join(
            rfishbase::fb_tbl("countref")[, c("C_Code", "PAESE", "CenterLat", "CenterLong")],
            by = c("c_code" = "C_Code")
          ) |>
          dplyr::relocate(species_id, country = PAESE, longitude = CenterLat, latitude = CenterLong) |>
          dplyr::select(-c_code)
      } else {
        x
      }
    })

  # Export
  purrr::walk2(
    fishbase,
    names(fishbase),
    ~ vroom::vroom_write(.x, file.path(output_path, paste0(.y, ".csv")), delim = ",")
  )
}
