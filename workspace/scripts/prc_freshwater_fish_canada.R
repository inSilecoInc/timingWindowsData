prc_freshwater_fish_canada <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/freshwater_fish_canada-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   "fishbase_freshwater_fish_canada.csv"
  # )

  input_files <- unlist(input_files)

  # Data
  fish <- input_files |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(-name_in_country) |>
    dplyr::rename(vernacular = fish_base_name) |>
    dplyr::select(species, vernacular, order, family, occurrence)

  # Export
  vroom::vroom_write(fish, file.path(output_path, "freshwater_fish_species_canada.csv"), delim = ",")
}
