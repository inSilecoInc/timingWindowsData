prc_north_american_freshwater_migratory_fish_database <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/north_american_freshwater_migratory_fish_database-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/north_american_freshwater_migratory_fish_database-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "NAFMFD_finalcopy.xlsx",
  #     "NAFMFD_metadata_12-20-2021.xml"
  #   )
  # )
  input_files <- unlist(input_files)

  # Data
  suppressWarnings({
    dat <- input_files[grep(".xlsx", input_files)] |>
      readxl::read_excel() |>
      janitor::clean_names()
  })

  # Export
  vroom::vroom_write(dat, file.path(output_path, "north_american_freshwater_migratory_fish_database.csv"), delim = ",")
}
