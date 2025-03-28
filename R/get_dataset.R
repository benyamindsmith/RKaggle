#' Download and Read a Dataset from Kaggle
#'
#' This function retrieves a dataset from Kaggle by downloading its metadata and associated ZIP file, then reading all CSV files contained in the archive. The ZIP file is downloaded to a temporary location and extracted to another temporary directory. Each CSV file is then imported as a data frame, and the function the dataset as a single data frame. In the case of multiple csv files, a unnamed list is returned. 
#'
#' @param dataset A character string specifying the dataset identifier on Kaggle. It should follow the format "username/dataset-name".
#'
#' @details The function constructs the metadata URL based on the provided dataset identifier, then sends a GET request using the \code{httr} package. If the request is successful, the returned JSON metadata is parsed. The function searches the metadata for a file with an encoding format of "application/zip", then downloads that ZIP file using a temporary file (managed by the \code{withr} package). After unzipping the file into a temporary directory, the function locates all CSV files, reads each using \code{readr::read_csv}, and returns a list of data frames. If any of the following occur, the function stops with an error:
#' \itemize{
#'   \item The HTTP request fails.
#'   \item No ZIP file URL is found in the metadata.
#'   \item No CSV files are found in the unzipped contents.
#' }
#'
#'
#' @examples
#' \dontrun{
#'   # Download and read the "canadian-prime-ministers" dataset from Kaggle
#'   canadian_prime_ministers <- get_dataset("benjaminsmith/canadian-prime-ministers")
#'   canadian_prime_ministers
#'   
#'   # Download the titanic dataset
#'   titanic <- get_dataset("azeembootwala/titanic")
#'   titanic
#' }
#'
#' @import httr readr withr utils
#' @export
get_dataset <- function(dataset) {
  metadata_url <- paste0("www.kaggle.com/datasets/", dataset, "/croissant/download")
  response <- httr::GET(metadata_url)
  
  # Ensure the request succeeded
  if (httr::http_status(response)$category != "Success") {
    stop("Failed to fetch metadata.")
  }
  
  # Parse the metadata
  metadata <- httr::content(response, as = "parsed", type = "application/json")
  
  # Locate the ZIP file URL
  distribution <- metadata$distribution
  zip_url <- NULL
  
  for (file in distribution) {
    if (file$encodingFormat == "application/zip") {
      zip_url <- file$contentUrl
      break
    }
  }
  
  if (is.null(zip_url)) {
    stop("No ZIP file URL found in the metadata.")
  }
  
  # Download the ZIP file. We'll use the withr package to make sure the downloaded
  # files get cleaned up when we're done.
  temp_file <- withr::local_tempfile(fileext = ".zip") 
  utils::download.file(zip_url, temp_file, mode = "wb")
  
  # Unzip and read the CSV
  unzip_dir <- withr::local_tempdir()
  utils::unzip(temp_file, exdir = unzip_dir)
  
  # Locate the CSV file within the extracted contents
  csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV file found in the unzipped contents.")
  }
  
  datasets <- lapply(
    csv_files, function(x) readr::read_csv(x)
  )
  
  if(length(datasets)==1){
    return(datasets[[1]])
  }
  return(datasets)
}

