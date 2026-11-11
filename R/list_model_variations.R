#' List a Kaggle model's variations
#'
#' Lists every framework/variation of a public Kaggle model, to help find the
#' handle to pass to [load_model()].
#'
#' @param model `"owner/model"` or a `https://www.kaggle.com/models/owner/model`
#'   link.
#'
#' @return A data frame with columns `framework`, `variation`, `version`
#'   (latest version number) and `handle`.
#'
#' @seealso [load_model()]
#'
#' @examples
#' \dontrun{
#' kaggle_variations("qwen-lm/qwen2.5")
#' }
#'
#' @md
#' @export
list_model_variations <- function(model) {
  h <- sub("^https?://(www\\.)?kaggle\\.com/models/", "", trimws(model))
  h <- strsplit(h, "/", fixed = TRUE)[[1]]
  if (length(h) < 2) {
    stop("Use 'owner/model', e.g. 'qwen-lm/qwen2.5'.", call. = FALSE)
  }
  
  meta <- .kaggle_model_meta(h[1], h[2])
  if (!length(meta$instances)) {
    stop("No variations listed for ", h[1], "/", h[2], call. = FALSE)
  }
  
  out <- do.call(rbind, lapply(meta$instances, function(i) {
    data.frame(
      framework = i$framework %||% NA_character_,
      variation = i$slug %||% NA_character_,
      version   = i$versionNumber %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
  out$handle <- paste(h[1], h[2], out$framework, out$variation, out$version, sep = "/")
  out[order(out$framework, out$variation), , drop = FALSE]
}