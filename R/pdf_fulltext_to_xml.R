#' Process PDFs with API
#'
#' This function processes PDF files using an API and saves the results as TEI XML files in the specified output folder.
#'
#' @param input The path to a PDF file or a folder containing PDF files to be processed.
#' @param output The folder where the processed TEI XML files will be saved. Default is the current working directory.
#' @param api_url The URL of the API to process the PDF files.
#' @param tei_coordinates A character vector specifying the TEI coordinates to be included in the API request. Default is c("figure", "biblStruct").
#' @param force Logical, if TRUE, existing output files will be overwritten. Default is FALSE.
#' @return invisible(NULL)
#' @examples
#' \dontrun{
#' process_with_api("path/to/input/folder", "path/to/output/folder", "http://example.com/api")
#' }
#' @export
pdf_fulltext_to_xml <- function(
  input,
  output = getwd(),
  api_url,
  tei_coordinates = c("figure", "biblStruct"),
  force = FALSE) {
  # Ensure required packages are installed
  if (!requireNamespace("httr", quietly = TRUE)) {
  stop(
    "The 'httr' package is required. Please install it using install.packages('httr')."
  )
  }
  if (!requireNamespace("tools", quietly = TRUE)) {
  stop("The 'tools' package is required. Please install it using install.packages('tools').")
  }

  # Check if input is a file or a folder
  if (file.exists(input)) {
  if (dir.exists(input)) {
    # Input is a folder, list all PDF files in the folder
    pdf_files <- list.files(input, pattern = "\\.pdf$", full.names = TRUE)
  } else if (grepl("\\.pdf$", input, ignore.case = TRUE)) {
    # Input is a single PDF file
    pdf_files <- list(input)
  } else {
    stop("Input is not a PDF file or a folder: ", input)
  }
  } else {
  stop("Input does not exist: ", input)
  }

  if (length(pdf_files) == 0) {
  stop("No PDF files found in the input: ", input)
  }

  # Check if output is a folder or a file
  if (file.exists(output)) {
  if (!dir.exists(output)) {
    stop("Output exists but is not a directory: ", output)
  }
  } else {
  dir.create(output, recursive = TRUE)
  }

  # Process each PDF file
  for (pdf_file in pdf_files) {
  # Extract the base name of the file for naming the output
  file_name <- tools::file_path_sans_ext(basename(pdf_file))
  output_file <- file.path(output, paste0(file_name, ".tei.xml"))

  # Check if the output file already exists
  if (file.exists(output_file) && !force) {
    message("Output file already exists: ", output_file,
       ". To overwrite the file, set force = TRUE.")
    next
  }

  # Prepare the form data with multiple teiCoordinates
  form_data <- list(
    input = httr::upload_file(pdf_file)
  )
  for (coord in tei_coordinates) {
    form_data[[paste0("teiCoordinates[", coord, "]")]] <- coord
  }

  # Make the API request
  response <- httr::POST(
    url = api_url,
    httr::add_headers(`Accept` = "application/xml"),
    body = form_data,
    encode = "multipart"
  )

  # Check response status
  if (httr::status_code(response) == 200) {
    # Save the response content as a TEI XML file
    writeLines(httr::content(response, as = "text", encoding = "UTF-8"), output_file)
    message("Processed: ", pdf_file, " -> Saved as: ", output_file)
    } else {
      message("Failed to process file: ", pdf_file,
          " (HTTP status: ", httr::status_code(response), ")")
    }
  }
}
