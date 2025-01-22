prc_canada_outline <- function(input_files, output_path) {
  tmp <- file.path(output_path, "tmp/")
  geodata::gadm("CAN", 1, tmp) |>
    terra::simplifyGeom(0.1, preserveTopology = FALSE) |>
    janitor::clean_names() |>
    terra::writeVector(file.path(output_path, "can_1_simplified.gpkg"))

  # Clean up temporary files
  fs::dir_delete(tmp)
}
