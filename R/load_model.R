#' Download a public model and return a prediction function
#'
#' Downloads a model from Kaggle, TF Hub or any direct URL, caches it on disk,
#' detects its format and returns an R function that runs it.
#'
#' No Kaggle account or token is needed: public Kaggle models can be
#' downloaded anonymously. Private and license-gated models still require
#' signing in to Kaggle and accepting the model's terms.
#'
#' @section Optional dependencies:
#' Only the packages for the model types you use need to be installed:
#' * Keras / KerasHub: `keras3` and `reticulate`
#' * TensorFlow SavedModel / TF Hub: `tensorflow`
#' * TorchScript: `torch` (then run `torch::install_torch()` once)
#' * XGBoost: `xgboost`
#' * GGUF language models: `localLLM` (llama.cpp; no Python needed)
#'
#' @param url Where the model lives. Any of:
#'   * a Kaggle handle: `"owner/model/framework/variation"` (latest version)
#'     or `"owner/model/framework/variation/version"`;
#'   * a Kaggle web link, e.g.
#'     `https://www.kaggle.com/models/keras/densenet/keras/densenet_121_imagenet/3`;
#'   * a Kaggle API link, e.g.
#'     `https://www.kaggle.com/api/v1/models/.../3/download`;
#'   * a TF Hub link, e.g.
#'     `https://tfhub.dev/google/tf2-preview/mobilenet_v2/classification/2`;
#'   * any direct URL to an archive (`.tar.gz`, `.tgz`, `.tar`, `.zip`) or to
#'     a single model file.
#' @param model_type One of `"auto"` (detect from the downloaded files),
#'   `"keras"`, `"torch_script"`, `"xgboost"` or `"gguf"`.
#' @param cache_dir Directory where downloads are unpacked and reused on later
#'   calls. Defaults to this package's user cache directory (see
#'   [tools::R_user_dir()]). Use `NULL` to download into a fresh temporary
#'   directory every time.
#'
#' @return A function. For most models it is `function(input_data)` and
#'   returns predictions as R objects. For language models it is
#'   `function(prompt, max_length, sampler, strip_prompt, ...)` and returns a
#'   character vector of generated text.
#'
#' @seealso [list_model_variations()] to find the handle of a Kaggle model.
#'
#' @examples
#' \dontrun{
#' # Kaggle (no token needed): DenseNet-121 KerasHub preset
#' predict_fn <- load_model("keras/densenet/keras/densenet_121_imagenet/3")
#'
#' # TF Hub link
#' predict_fn <- load_model(
#'   "https://tfhub.dev/google/tf2-preview/mobilenet_v2/classification/2"
#' )
#'
#' tmp <- tempfile(fileext = ".jpg")
#' download.file(
#'   paste0(
#'     "https://storage.googleapis.com/download.tensorflow.org/",
#'     "example_images/grace_hopper.jpg"
#'   ),
#'   tmp,
#'   mode = "wb"
#' )
#' img <- keras3::image_load(tmp, target_size = c(224, 224)) |>
#'   keras3::image_to_array() |>
#'   abind::abind(along = 0)
#'
#' # TF Hub MobileNet expects pixels in [0, 1]; KerasHub presets rescale
#' # 0-255 input themselves.
#' result <- predict_fn(img / 255)
#' keras::mobilenet_decode_predictions(result[, -1, drop = FALSE])
#'
#' # Small language model: GPT-2 base (124M parameters, ~500 MB download).
#' # One-time Python setup:
#' #   keras3::install_keras(backend = "tensorflow")
#' #   reticulate::py_install("keras-hub", envname = "r-keras")
#' generate <- load_model("keras/gpt2/keras/gpt2_base_en")
#'
#' # Greedy decoding: deterministic, same output every time
#' generate("The R programming language is", max_length = 40, sampler = "greedy")
#'
#' # Sampling: more varied text; several prompts at once
#' generate(
#'   c("Once upon a time,", "The best way to learn statistics is"),
#'   max_length = 50,
#'   sampler = "top_k"
#' )
#'
#' # Return only the continuation, without the prompt
#' generate("My favourite R package is", max_length = 30, strip_prompt = TRUE)
#' }
#'
#' @md
#' @export

load_model <- function(url,
                       model_type = c("auto", "keras", "torch_script", "xgboost", "gguf"),
                       cache_dir = tools::R_user_dir(.pkg_name(), which = "cache")) {
  model_type <- match.arg(model_type)
  
  dl <- .resolve_download(url)
  model_dir <- .fetch_and_extract(dl, cache_dir)
  all_files <- list.files(model_dir, full.names = TRUE, recursive = TRUE)
  if (length(all_files) == 0) {
    stop("The download contained no files.", call. = FALSE)
  }
  
  if (model_type == "auto") {
    model_type <- .detect_type(all_files)
    message("--> Detected model type: ", model_type)
  }
  
  switch(model_type,
         keras        = .load_keras(all_files),
         torch_script = .load_torch_script(all_files),
         xgboost      = .load_xgboost(all_files),
         gguf         = .load_gguf(all_files)
  )
}