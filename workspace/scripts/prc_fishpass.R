prc_fishpass <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/fishpass-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/fishpass-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "fishpass_database.zip"
  #   )
  # )
  input_files <- unlist(input_files)

  # Unzip data
  tmp <- file.path(output_path, "tmp/")
  archive::archive_extract(input_files, tmp)

  # Behaviour database
  behaviour <- file.path(tmp, "FishPass_Behaviour_Database.csv") |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Morphology database
  morphology <- file.path(tmp, "FishPass_Morphology_Database.csv") |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Phenology database
  phenology <- file.path(tmp, "FishPass_Phenology_Database.csv") |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Physiology database
  physiology <- file.path(tmp, "FishPass_Physiology_Database.csv") |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Export
  vroom::vroom_write(behaviour, file.path(output_path, "fishpass_behaviour.csv"), delim = ",")
  vroom::vroom_write(morphology, file.path(output_path, "fishpass_morphology.csv"), delim = ",")
  vroom::vroom_write(phenology, file.path(output_path, "fishpass_phenology.csv"), delim = ",")
  vroom::vroom_write(physiology, file.path(output_path, "fishpass_physiology.csv"), delim = ",")

  # Clean up temporary files
  fs::dir_delete(tmp)
}
