#' Download and Read a Dataset from Kaggle
#'
#' This function retrieves a dataset from Kaggle by downloading its metadata and associated ZIP file, then reading all supported files contained in the archive. The ZIP file is downloaded to a temporary location and extracted to another temporary directory. Each supported file is imported into R using the appropriate function, and the function returns a single data frame if only one file is detected or an unnamed list of data frames otherwise.
#'
#' @param dataset A character string specifying the dataset identifier on Kaggle. It should follow the format "username/dataset-name".
#'
#' @details The function constructs the metadata URL based on the provided dataset identifier, then sends a GET request using the \code{httr} package. If the request is successful, the returned JSON metadata is parsed. The function searches the metadata for a file with an encoding format of "application/zip", then downloads that ZIP file using a temporary file (managed by the \code{withr} package). After unzipping the file into a temporary directory, the function locates all files with extensions corresponding to popular dataset formats (\code{csv}, \code{tsv}, \code{xlsx}, \code{json}, \code{rds}, and \code{parquet}). Each file is then read using the appropriate function:
#' \itemize{
#'   \item \code{readr::read_csv} for CSV files.
#'   \item \code{readr::read_tsv} for TSV files.
#'   \item \code{readxl::read_excel} for xlsx files.
#'   \item \code{jsonlite::fromJSON} for JSON files.
#'   \item \code{readRDS} for RDS files.
#'   \item \code{arrow::read_parquet} for Parquet files.
#'   \item \code{readODS::read_ods} for ODS files
#' }
#' The function stops with an error if any of the following occur:
#' \itemize{
#'   \item The HTTP request fails.
#'   \item No ZIP file URL is found in the metadata.
#'   \item No supported data files are found in the unzipped contents.
#' }
#'
#' @examples
#' \dontrun{
#'   # Download and read the "canadian-prime-ministers" dataset from Kaggle
#'   canadian_prime_ministers <- get_dataset("benjaminsmith/canadian-prime-ministers")
#'   canadian_prime_ministers
#'   
#'   # csv 
#'   canadian_prime_ministers <- get_dataset("benjaminsmith/canadian-prime-ministers")
#'   # tsv 
#'   arabic_twitter <- get_dataset("mksaad/arabic-sentiment-twitter-corpus")
#'   # xlsx
#'   hr_data <- get_dataset("kmldas/hr-employee-data-descriptive-analytics")
#'   # json
#'   iris_json <- get_dataset("rtatman/iris-dataset-json-version")
#'   # rds
#'   br_pop_2019<-get_dataset("ianfukushima/br-pop-2019")
#'   # parquet
#'   iris_datasets<-get_dataset("gpreda/iris-dataset")
#'   #ods
#'   new_houses <- get_dataset("nm8883/new-houses-built-each-year-in-england")
#' }
#'
#' @import httr readr withr utils readxl jsonlite arrow
#' @export
get_dataset <- function(dataset) {
  # Construct the metadata URL and fetch the metadata from Kaggle
  metadata_url <- paste0("www.kaggle.com/datasets/", dataset, "/croissant/download")
  response <- httr::GET(metadata_url)
  
  # Ensure the request succeeded
  if (httr::http_status(response)$category != "Success") {
    stop("Failed to fetch metadata.")
  }
  
  # Parse the metadata
  metadata <- httr::content(response, as = "parsed", type = "application/json")
  
  # Locate the ZIP file URL in the metadata
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
  
  # Download the ZIP file. withr::local_tempfile ensures temporary cleanup.
  temp_file <- withr::local_tempfile(fileext = ".zip")
  utils::download.file(zip_url, temp_file, mode = "wb")
  
  # Unzip the downloaded file into a temporary directory
  unzip_dir <- withr::local_tempdir()
  utils::unzip(temp_file, exdir = unzip_dir)
  
  # List files of popular dataset types in the unzipped folder.
  data_files <- list.files(
    unzip_dir, 
    pattern = "\\.(csv|tsv|xls|xlsx|json|rds|parquet|ods)$", 
    ignore.case = TRUE, 
    full.names = TRUE
  )
  
  if (length(data_files) == 0) {
    stop("No supported data files found in the unzipped contents.")
  }
  
  # Helper function to read a file based on its extension
  read_data_file <- function(file) {
    ext <- tolower(tools::file_ext(file))
    if (ext == "csv") {
      return(readr::read_csv(file))
    } else if (ext == "tsv") {
      return(readr::read_tsv(file))
    } else if (ext %in% c("xlsx")) {
      return(readxl::read_excel(file))
    }else if (ext == "json") {
      return(jsonlite::fromJSON(file))
    } else if (ext == "rds") {
      return(readRDS(file))
    } else if (ext == "parquet") {
      return(arrow::read_parquet(file))
    } else if (ext == "ods") {
      return(readODS::read_ods(file))
    } else {
      warning(paste("File type", ext, "is not supported."))
      return(NULL)
    }
  }
  
  # Read each file into R using the appropriate function
  datasets <- lapply(data_files, function(x) read_data_file(x)|> tibble::as_tibble())
  

  
  # Remove any unsuccessful reads (if any file returned NULL)
  datasets <- datasets[!sapply(datasets, is.null)]
  
  # Return a single dataset if there's only one, or a list of datasets otherwise.
  if (length(datasets) == 1) {
    return(datasets[[1]])
  }
  return(datasets)
}