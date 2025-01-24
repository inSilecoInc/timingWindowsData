prc_freshwater_fish_canada <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/freshwater_fish_canada-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c("fishbase_freshwater_fish_canada.csv",
  #     "fishbase_fish_status_canada.csv",
  #     "fishbase_game_fish_canada.csv",
  #     "fishbase_commercial_fish_canada.csv"
  #   )
  # )
  input_files <- unlist(input_files)

  # Freshwater fish species
  fish <- input_files[grepl("fishbase_freshwater_fish_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(-name_in_country) |>
    dplyr::rename(vernacular = fish_base_name) |>
    dplyr::select(species, vernacular, order, family, occurrence)

  # Status fish species
  status <- input_files[grepl("fishbase_fish_status_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(species, threat_category)

  # Game fish species
  game <- input_files[grepl("fishbase_game_fish_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(species) |>
    dplyr::mutate(game = 1)

  # Game fish species
  commercial <- input_files[grepl("fishbase_commercial_fish_canada.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(species) |>
    dplyr::mutate(commercial = 1)

  # Species list
  fish <- fish |>
    dplyr::left_join(status, by = "species") |>
    dplyr::left_join(game, by = "species") |>
    dplyr::mutate(game = dplyr::if_else(is.na(game), 0, game)) |>
    dplyr::left_join(commercial, by = "species") |>
    dplyr::mutate(commercial = dplyr::if_else(is.na(commercial), 0, commercial)) |>
    dplyr::mutate(species_id = sprintf("sp_%06d", seq_len(dplyr::n()))) |>
    dplyr::relocate(species_id)

  # Export
  vroom::vroom_write(fish, file.path(output_path, "freshwater_fish_species_canada.csv"), delim = ",")
}
