prc_roberge_2002 <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/roberge_2002-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/roberge_2002-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "Roberge2002.txt"
  #   )
  # )
  input_files <- unlist(input_files)

  # Data
  Roberge.raw <- read.delim(
    input_files,
    header = FALSE,
    na.strings = ""
  ) |>
    tidyr::fill(V1:V2)

  # Split and pivot data
  Roberge.split <- Roberge.raw |> dplyr::group_split(V1)
  pivoted_list <- list()

  for (i in seq_along(Roberge.split)) {
    pivoted_data <- Roberge.split[[i]] |>
      dplyr::rename(
        name = V1,
        scientificname = V2,
        MigrationStrategy = V3,
        characteristics = V4,
        Spawning = V5,
        Ammocoete = V6,
        YOY = V7,
        Juvenile = V8,
        Adult = V9
      ) |>
      dplyr::filter(!dplyr::row_number() %in% 1) |>
      tidyr::pivot_longer(
        cols = tidyr::starts_with("Spawning"):tidyr::ends_with("Adult"),
        names_to = "LifeStage",
        values_to = "Value"
      ) |>
      dplyr::mutate(
        name = stringr::str_to_sentence(name),
        scientificname = stringr::str_to_sentence(scientificname),
        name = trimws(name),
        Value = stringr::str_replace_all(Value, "�C", "°C")
      )

    pivoted_list[[i]] <- pivoted_data
  }

  Roberge.df <- dplyr::bind_rows(pivoted_list) |>
    dplyr::filter(!is.na(Value)) |>
    janitor::clean_names()

  # Export
  vroom::vroom_write(
    Roberge.df,
    file.path(output_path, "roberge.csv"),
    delim = ","
  )
}
