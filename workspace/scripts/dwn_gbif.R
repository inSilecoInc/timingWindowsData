download_gbif <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/gbif-1.0.0/"
  # # dir.create(output_path)
  # input_files <- c(
  #   "workspace/data/harvested/freshwater_fish_canada-1.0.0/processed/freshwater_fish_species_canada.csv"
  # )
  input_files <- unlist(input_files)

  # ---------------------------------------------------
  # Credentials
  # ---------------------------------------------------
  # Load the credentials from the JSON file
  credentials <- jsonlite::fromJSON("workspace/credentials/gbif.json")

  # Extract credentials for further use
  user <- credentials$user
  email <- credentials$email
  password <- credentials$password

  # ---------------------------------------------------
  # Query arguments
  # ---------------------------------------------------
  # Species
  species <- input_files |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)
  spOn <- species$species

  taxonKey <- rgbif::name_backbone_checklist(spOn) |>
    dplyr::filter(usageKey > 500) |>
    dplyr::pull(usageKey)

  # Other arguments
  country <- "CA"
  hasCoordinate <- TRUE
  hasGeospatialIssue <- FALSE
  occurrenceStatus <- "PRESENT"

  # ---------------------------------------------------
  # Metadata
  # ---------------------------------------------------
  occurrence_count <- rgbif::occ_count(
    taxonKey = paste0(taxonKey, collapse = ";"),
    country = country,
    hasCoordinate = hasCoordinate,
    hasGeospatialIssue = hasGeospatialIssue,
    occurrenceStatus = occurrenceStatus
  )

  # Path to save metadata locally
  metadata_path <- file.path(output_path, "metadata.json")

  # Check if the metadata file exists, if not, create a new one
  if (file.exists(metadata_path)) {
    # Load the saved metadata
    saved_metadata <- jsonlite::fromJSON(metadata_path)

    # Compare saved metadata with current metadata
    if (saved_metadata$occurrence_count == occurrence_count &&
      saved_metadata$last_modified == last_modified) {
      cat("No new data updates. Skipping download...\n")
      skip_download <- TRUE
    } else {
      cat("Data updated. Proceeding with download...\n")
      skip_download <- FALSE
    }
  } else {
    cat("No previous metadata found. Proceeding with download...\n")
    skip_download <- FALSE
  }

  # Save the current metadata for future comparison
  current_metadata <- list(
    occurrence_count = occurrence_count
  )
  jsonlite::write_json(current_metadata, metadata_path)

  # ---------------------------------------------------
  # Download
  # ---------------------------------------------------
  # Check if we should download the data based on metadata comparison
  if (!skip_download) {
    # Create the download query
    query <- rgbif::occ_download(
      rgbif::pred_default(),
      # rgbif::pred_gte("year", "2010"), # Select records with eventDate after 2010
      rgbif::pred("country", country),
      rgbif::pred_in("taxonKey", taxonKey),
      user = user, email = email, pwd = password
    )
    download_key <- query[1]
    rgbif::occ_download_wait(download_key)

    # Download the data using the download key
    rgbif::occ_download_get(download_key, path = output_path)

    cat("Data downloaded successfully.\n")
  } else {
    cat("Skipping download as no updates were found.\n")
  }

  # ---------------------------------------------------
  # Citation
  # ---------------------------------------------------
  # Fetch the citation for the downloaded data
  citation <- rgbif::gbif_citation(download_key)

  # Example of a parsed BibTeX entry using manually extracted fields from the citation
  # Assuming `citation$citation` provides enough detail to extract these fields

  bib_entry <- paste0(
    "@TechReport{gbif_", Sys.Date(), ",\n",
    "  title = {GBIF Occurrence Download https://doi.org/", attributes(query)$doi, " Accessed from R via rgbif (https://github.com/ropensci/rgbif) on ", Sys.Date(), "},\n",
    "  author = {{Global Biodiversity Information Facility}},\n", # Adjust as needed
    "  year = {", format(Sys.Date(), "%Y"), "},\n",
    "  institution = {{GBIF Secretariat}},\n",
    "}"
  )

  # Save the citation as a .bib file
  writeLines(bib_entry, file.path(output_path, "gbif.bib"))

  # Save query number
  # Save the current metadata for future comparison
  query_info <- c(
    query_number = query[[1]],
    attributes(query)
  )
  jsonlite::write_json(query_info, file.path(output_path, "query.json"))

  cat("Citation saved as a .bib file.\n")
}
