#' Retrieve Data from Ontario Freshwater Fishes Life History Database
#'
#'
#' @references
#' * Eakins, R.J. 2024. Ontario Freshwater Fishes Life History Database.
#' Version 5.31. Online database. (https://www.ontariofishes.ca)
#'
#' @return
#' A list of two data frames:
#' * `characteristics` that includes all characteristics (43)
#' * `references` that includes all references.
#'
#' @export

dwn_ontario_freshwater_fishes_life_history <- function(input_files, output_path) {
  # number of species covered by the database, determined manually
  n <- 161
  ls_data <- list()
  cli::cli_progress_bar("Getting species info", total = n, type = "iterator")
  for (i in seq_len(n)) {
    tmp <- paste0("http://www.ontariofishes.ca/fish_detail.php?FID=", i) |>
      rvest::read_html()
    cli::cli_progress_update()
    lab <- tmp |>
      rvest::html_elements(".DataLabel") |>
      rvest::html_text()
    txt <- tmp |>
      rvest::html_elements(".DataText") |>
      rvest::html_text()
    bib <- tmp |>
      rvest::html_elements(".Biblio") |>
      rvest::html_text()

    ls_data[[i]] <- list(
      characteristics = data.frame(
        field = lab,
        value = txt[1:43] # manually checked
      ),
      references = bib
    )
  }
  cli::cli_progress_done()

  d_references <- ls_data |>
    lapply(
      \(x) {
        if (length(x$references)) {
          data.frame(
            species = x$characteristics$value[x$characteristics$field == "Species"],
            reference = x$references
          )
        } else {
          NULL
        }
      }
    ) |>
    do.call(what = rbind)

  d_characteristics <- ls_data |>
    lapply(\(x) x$characteristics) |>
    lapply(\(x) cbind(
      data.frame(
        species_name = x$value[x$field == "Species"]
      ),
      x
    )) |>
    do.call(what = rbind) |>
    tidyr::pivot_wider(
      names_from = field,
      values_from = value
    ) |>
    janitor::clean_names()

  list(
    characteristics = d_characteristics,
    refrences = d_references
  )

  # Export
  vroom::vroom_write(d_characteristics, file.path(output_path, "ontario_fishes_characteristics.csv"), delim = ",")
  vroom::vroom_write(d_references, file.path(output_path, "ontario_fishes_references.csv"), delim = ",")
}
