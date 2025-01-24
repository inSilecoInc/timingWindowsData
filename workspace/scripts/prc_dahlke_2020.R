prc_dahlke_2020 <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/dahlke_2020-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/dahlke_2020-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "experimental_and_imputed_tolerance_data.xlsx",
  #     "thermal_safety_margins.xlsx",
  #     "thermal_responsiveness.xlsx",
  #     "thermal_tolerance.xlsx"
  #   )
  # )
  input_files <- unlist(input_files)

  # ========================================================================
  # Loading and Cleaning Thermal Tolerance Data (Dahlke et al., 2020)
  # ========================================================================
  # Load and clean data from "Thermal_tolerance.xlsx"
  file_path_1 <- input_files[grep("thermal_tolerance.xlsx", input_files)]
  sheet_names_1 <- readxl::excel_sheets(file_path_1)[-1] # Exclude the first sheet

  # Initialize a list to store data from each sheet
  thermal_data_list <- list()

  # Loop through each sheet and load the data
  for (sheet in sheet_names_1) {
    # Read the data from the current sheet
    sheet_data <- readxl::read_excel(file_path_1, sheet = sheet)

    # Add the sheet data to the list with the sheet name as the key
    thermal_data_list[[sheet]] <- sheet_data
  }

  Thermal_tolerance <- dplyr::bind_rows(thermal_data_list, .id = "lifeStage") |>
    tidyr::fill(Species) |>
    dplyr::mutate(
      Reference = apply(
        dplyr::pick(tidyr::starts_with("Reference"):tidyr::last_col()),
        1,
        \(x) paste(na.omit(x), collapse = " ")
      )
    ) |>
    dplyr::select(-tidyr::starts_with("...")) |>
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, c("\\s\\(°C\\)" = "_C")),
      dplyr::everything()
    ) |>
    janitor::clean_names()


  # ========================================================================
  # Loading and Cleaning Experimental and Imputed Data
  # ========================================================================
  # Load and clean data from "Experimental_and_imputed_tolerance_data.xlsx"
  file_path_2 <- input_files[grep("experimental_and_imputed_tolerance_data.xlsx", input_files)]
  sheet_names_2 <- readxl::excel_sheets(file_path_2)[-1]

  # Initialize a list to store data from each sheet
  Exp_Imp_tolerance_data_list <- list()

  # Loop through each sheet and load the data
  for (sheet in sheet_names_2) {
    # Read the data from the current sheet
    sheet_data <- readxl::read_excel(file_path_2, sheet = sheet)

    # Add the sheet data to the list with the sheet name as the key
    Exp_Imp_tolerance_data_list[[sheet]] <- sheet_data
  }

  Exp_Imp_tolerance <- dplyr::bind_rows(Exp_Imp_tolerance_data_list, .id = "Thermal_tolerance") |>
    dplyr::rename_with(~ stringr::str_replace_all(.x, c("\\s\\(°C\\)" = "_C")), dplyr::everything()) |>
    dplyr::mutate(Realm = dplyr::if_else(is.na(Realm), realm, Realm)) |>
    dplyr::select(-realm) |>
    dplyr::arrange(SpeciesFishBase, Lifestage, Thermal_tolerance) |>
    tidyr::fill(Realm) |>
    janitor::clean_names()


  # ========================================================================
  # Loading and Cleaning Thermal responsiveness
  # ========================================================================
  file_path <- input_files[grep("thermal_responsiveness.xlsx", input_files)]
  sheet_names <- readxl::excel_sheets(file_path)[-c(1, 5)]

  # Initialize a list to store data from each sheet
  dat <- list()

  # Loop through each sheet and load the data
  for (sheet in sheet_names) {
    # Read the data from the current sheet
    sheet_data <- readxl::read_excel(file_path, sheet = sheet) |>
      dplyr::mutate(Lifestage = sheet)

    # Add the sheet data to the list with the sheet name as the key
    dat[[sheet]] <- sheet_data
  }

  Thermal_responsiveness <- dplyr::bind_rows(dat) |>
    janitor::clean_names() |>
    dplyr::select(species, lifestage, response, trange_c, tmid_c, reference) |>
    na.omit()

  # ========================================================================
  # Loading and Cleaning Thermal safety margins
  # ========================================================================
  file_path <- input_files[grep("thermal_safety_margins.xlsx", input_files)]
  sheet_names <- readxl::excel_sheets(file_path)[2]

  # Initialize a list to store data from each sheet
  dat <- list()

  # Loop through each sheet and load the data
  for (sheet in sheet_names) {
    # Read the data from the current sheet
    sheet_data <- readxl::read_excel(file_path, sheet = sheet)

    # Add the sheet data to the list with the sheet name as the key
    dat[[sheet]] <- sheet_data
  }

  Thermal_safety_margins <- dplyr::bind_rows(dat) |>
    janitor::clean_names() |>
    na.omit()


  # ========================================================================
  # Export
  # ========================================================================
  vroom::vroom_write(Thermal_tolerance, file.path(output_path, "experimental_imputed_tolerance.csv"), delim = ",")
  vroom::vroom_write(Exp_Imp_tolerance, file.path(output_path, "thermal_safety_margins.csv"), delim = ",")
  vroom::vroom_write(Thermal_responsiveness, file.path(output_path, "thermal_responsiveness.csv"), delim = ",")
  vroom::vroom_write(Thermal_safety_margins, file.path(output_path, "thermal_tolerance.csv"), delim = ",")
}
