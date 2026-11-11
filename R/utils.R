# Base URL of Kaggle's public API.
.kaggle_api <- "https://www.kaggle.com/api/v1"

#' Fetch a Kaggle model's public metadata
#'
#' No authentication is needed for public models.
#'
#' @param owner,model Kaggle owner and model slugs.
#' @return The parsed JSON metadata as a list.
#' @keywords internal
#' @noRd
.kaggle_model_meta <- function(owner, model) {
  resp <- httr::GET(sprintf("%s/models/%s/%s/get", .kaggle_api, owner, model))
  .stop_for_access(resp, paste(owner, model, sep = "/"))
  httr::content(resp, as = "parsed", type = "application/json", simplifyVector = FALSE)
}

#' Turn a handle or link into a download URL and cache key
#'
#' @param x A Kaggle handle, Kaggle web/API link, TF Hub link or direct URL.
#' @return A list with elements `url` (what to download) and `key` (a string
#'   that identifies the download in the cache).
#' @keywords internal
#' @noRd
.resolve_download <- function(x) {
  x <- trimws(x)
  
  # Bare Kaggle handle
  if (!grepl("://", x, fixed = TRUE)) {
    return(.kaggle_handle_download(strsplit(x, "/")[[1]]))
  }
  
  u <- httr::parse_url(x)
  host <- tolower(u$hostname %||% "")
  parts <- strsplit(u$path %||% "", "/")[[1]]
  parts <- parts[nzchar(parts)]
  
  # Kaggle web page link: kaggle.com/models/owner/model/framework/variation[/version]
  if (grepl("(^|\\.)kaggle\\.com$", host) && length(parts) >= 5 && parts[1] == "models") {
    return(.kaggle_handle_download(parts[-1]))
  }
  
  # TF Hub link: ask for the compressed SavedModel (tfhub.dev now redirects to Kaggle)
  if (grepl("(^|\\.)tfhub\\.dev$", host)) {
    u$query <- utils::modifyList(u$query %||% list(), list(`tf-hub-format` = "compressed"))
    return(list(url = httr::build_url(u), key = paste(c("tfhub", parts), collapse = "_")))
  }
  
  # Kaggle API link or any other direct URL: use as-is
  list(url = x, key = paste(c(host, parts), collapse = "_"))
}

#' Build the download URL for a Kaggle model handle
#'
#' @param h Character vector `c(owner, model, framework, variation[, version])`.
#'   If the version is missing, the latest one is looked up.
#' @return A list with elements `url` and `key`, as for `.resolve_download()`.
#' @keywords internal
#' @noRd
.kaggle_handle_download <- function(h) {
  if (!length(h) %in% c(4, 5)) {
    stop("A Kaggle model handle looks like 'owner/model/framework/variation[/version]'.",
         call. = FALSE
    )
  }
  version <- if (length(h) == 5) h[5] else .kaggle_latest_version(h)
  list(
    url = sprintf(
      "%s/models/%s/%s/%s/%s/%s/download",
      .kaggle_api, h[1], h[2], h[3], h[4], version
    ),
    key = paste(c("kaggle", h[1:4], version), collapse = "_")
  )
}

#' Look up the latest version of a Kaggle model variation
#'
#' @param h Character vector `c(owner, model, framework, variation)`.
#' @return The latest version number.
#' @keywords internal
#' @noRd
.kaggle_latest_version <- function(h) {
  meta <- .kaggle_model_meta(h[1], h[2])
  for (inst in meta$instances) {
    if (identical(tolower(inst$framework %||% ""), tolower(h[3])) &&
        identical(inst$slug, h[4])) {
      message("--> Latest version of ", paste(h, collapse = "/"), " is ", inst$versionNumber)
      return(inst$versionNumber)
    }
  }
  stop("No variation '", h[3], "/", h[4], "' found for Kaggle model '",
       h[1], "/", h[2], "'.",
       call. = FALSE
  )
}

#' Stop with a helpful message for failed HTTP requests
#'
#' @param resp An `httr` response.
#' @param what A description of what was requested, used in the error message.
#' @return `resp`, invisibly, if the request succeeded.
#' @keywords internal
#' @noRd
.stop_for_access <- function(resp, what) {
  code <- httr::status_code(resp)
  if (code %in% c(401, 403)) {
    stop(what, " isn't publicly downloadable (HTTP ", code, "). Private and license-gated ",
         "models need you to sign in to Kaggle and accept the model's terms.",
         call. = FALSE
    )
  }
  if (code == 404) {
    stop(what, " was not found (HTTP 404). Check the handle or URL.", call. = FALSE)
  }
  httr::stop_for_status(resp, task = paste("download", what))
  
}

#' Download a model and extract it into the cache
#'
#' A `.complete` marker file is written once extraction succeeds, so an
#' interrupted download is never mistaken for a cached copy.
#'
#' @param dl A list with `url` and `key`, from `.resolve_download()`.
#' @param cache_dir Cache directory, or `NULL` for a temporary directory.
#' @return The path of the directory holding the extracted files.
#' @keywords internal
#' @noRd
.fetch_and_extract <- function(dl, cache_dir) {
  target <- if (is.null(cache_dir)) {
    tempfile("model_")
  } else {
    file.path(cache_dir, gsub("[^A-Za-z0-9._-]+", "_", dl$key))
  }
  done_flag <- file.path(target, ".complete")
  if (file.exists(done_flag)) {
    message("--> Using cached copy in ", target)
    return(target)
  }
  
  unlink(target, recursive = TRUE)
  dir.create(target, recursive = TRUE)
  archive <- tempfile("download_")
  on.exit(unlink(archive), add = TRUE)
  
  message("--> Downloading ", dl$url)
  resp <- httr::GET(
    dl$url,
    httr::write_disk(archive, overwrite = TRUE),
    if (interactive()) httr::progress()
  )
  .stop_for_access(resp, dl$url)
  
  ctype <- httr::headers(resp)[["content-type"]]
  if (!is.null(ctype) && grepl("text/html", ctype, fixed = TRUE)) {
    stop("The server returned a web page instead of a model file. Check the URL.",
         call. = FALSE
    )
  }
  
  .extract_download(archive, target, resp)
  file.create(done_flag)
  target
}

#' Extract a downloaded file
#'
#' Picks the extraction method from the file's magic bytes, so `.tar.gz`,
#' `.zip` and single raw files all work regardless of what the URL looks like.
#'
#' @param path Path of the downloaded file.
#' @param exdir Directory to extract into.
#' @param resp The `httr` response, used to name single-file downloads.
#' @return `exdir`, invisibly.
#' @keywords internal
#' @noRd
.extract_download <- function(path, exdir, resp) {
  bytes <- readBin(path, "raw", n = 512)
  is_zip <- .has_magic(bytes, as.raw(c(0x50, 0x4b, 0x03, 0x04)))
  is_gzip <- .has_magic(bytes, as.raw(c(0x1f, 0x8b)))
  is_tar <- .has_magic(bytes, charToRaw("ustar"), offset = 257)
  
  if (is_zip) {
    .check_entries(utils::unzip(path, list = TRUE)$Name)
    utils::unzip(path, exdir = exdir)
  } else if (is_gzip || is_tar) {
    entries <- utils::untar(path, list = TRUE)
    if (is_gzip && length(entries) == 0) {
      stop("Got a gzip file that is not a tar archive; unsupported.", call. = FALSE)
    }
    .check_entries(entries)
    if (!identical(as.integer(utils::untar(path, exdir = exdir)), 0L)) {
      stop("Failed to extract the tar archive.", call. = FALSE)
    }
  } else {
    name <- .remote_filename(resp)
    # A raw GGUF download may come without a useful file name.
    if (.has_magic(bytes, charToRaw("GGUF")) &&
        !grepl("\\.gguf$", name, ignore.case = TRUE)) {
      name <- paste0(tools::file_path_sans_ext(name), ".gguf")
    }
    file.copy(path, file.path(exdir, name))
  }
  invisible(exdir)
}

#' Check a file's leading bytes against a signature
#'
#' Compares raw bytes directly (rather than with `rawToChar()`), so binary
#' data containing NUL bytes can't cause an error.
#'
#' @param bytes Raw vector read from the start of the file.
#' @param magic Raw vector with the expected signature.
#' @param offset Number of bytes to skip before the signature.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
.has_magic <- function(bytes, magic, offset = 0) {
  idx <- offset + seq_along(magic)
  length(bytes) >= max(idx) && identical(bytes[idx], magic)
}

#' Refuse archives that would write outside the extraction directory
#'
#' @param entries Character vector of paths listed in the archive.
#' @return `NULL`, invisibly; called for its error.
#' @keywords internal
#' @noRd
.check_entries <- function(entries) {
  bad <- grepl("^(/|[A-Za-z]:)", entries) |
    grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", entries)
  if (any(bad)) {
    stop("Archive contains unsafe paths: ", paste(entries[bad], collapse = ", "),
         call. = FALSE
    )
  }
  invisible(NULL)
}

#' Work out the file name of a single-file download
#'
#' Uses the `Content-Disposition` header if present, otherwise the last part
#' of the final URL.
#'
#' @param resp An `httr` response.
#' @return A file name.
#' @keywords internal
#' @noRd
.remote_filename <- function(resp) {
  cd <- httr::headers(resp)[["content-disposition"]]
  if (!is.null(cd)) {
    m <- regmatches(
      cd,
      regexec("filename\\*?=(?:UTF-8'')?\"?([^\";]+)\"?", cd, perl = TRUE)
    )[[1]]
    if (length(m) == 2) {
      return(basename(utils::URLdecode(m[2])))
    }
  }
  name <- basename(httr::parse_url(resp$url)$path %||% "")
  if (!nzchar(name)) "model.bin" else utils::URLdecode(name)
}


#' Detect the kind of model from its files
#'
#' @param files Paths of all downloaded files.
#' @return One of `"gguf"`, `"keras"`, `"torch_script"` or `"xgboost"`.
#' @keywords internal
#' @noRd
.detect_type <- function(files) {
  b <- basename(files)
  if (any(grepl("\\.gguf$", b, ignore.case = TRUE))) {
    return("gguf")
  }
  if (!is.na(.keras_hub_dir(files)) ||
      any(grepl("\\.(keras|h5|hdf5)$", b, ignore.case = TRUE)) ||
      any(b %in% c("saved_model.pb", "saved_model.pbtxt"))) {
    return("keras")
  }
  if (any(grepl("\\.(pt|pth|ts|torchscript)$", b, ignore.case = TRUE))) {
    return("torch_script")
  }
  if (length(.xgb_candidates(files))) {
    return("xgboost")
  }
  
  if (!is.na(.hf_transformers_dir(files))) {
    return("transformers")
  }
  
  stop("Couldn't tell what kind of model this is. Files found:\n  ",
       paste(utils::head(b, 20), collapse = "\n  "),
       "\nSet `model_type` explicitly.",
       call. = FALSE
  )
}

#' Load a Keras, KerasHub or TensorFlow SavedModel model
#'
#' Tries, in order: a KerasHub preset, a full saved Keras model (`.keras` /
#' `.h5`), then a TensorFlow SavedModel directory.
#'
#' @param all_files Paths of all downloaded files.
#' @return A prediction (or text generation) function.
#' @keywords internal
#' @noRd
.load_keras <- function(all_files) {
  # 1. KerasHub preset (what Kaggle's "keras" framework models are):
  #    config.json + *.weights.h5, loaded with keras_hub's from_preset().
  hub_dir <- .keras_hub_dir(all_files)
  if (!is.na(hub_dir)) {
    return(.load_keras_hub(hub_dir))
  }
  
  # 2. A full saved Keras model (.keras / .h5), not a weights-only file.
  full <- grep("\\.(keras|h5|hdf5)$", all_files, value = TRUE, ignore.case = TRUE)
  full <- full[!grepl("\\.weights\\.h5$", full, ignore.case = TRUE)]
  if (length(full)) {
    return(.load_keras_file(full[[1]]))
  }
  
  # 3. TensorFlow SavedModel directory (TF Hub models, tf.saved_model.save, ...)
  sm <- grep("(^|/)saved_model\\.(pb|pbtxt)$", all_files, value = TRUE)
  if (length(sm)) {
    return(.load_saved_model(dirname(sm[[1]])))
  }
  
  stop("No KerasHub preset, .keras/.h5 model, or TensorFlow SavedModel found.",
       call. = FALSE
  )
}

#' Find a KerasHub preset directory
#'
#' A preset is a directory holding `config.json` and a `*.weights.h5` file.
#'
#' @param files Paths of all downloaded files.
#' @return The preset directory, or `NA_character_` if there is none.
#' @keywords internal
#' @noRd
.keras_hub_dir <- function(files) {
  cfg <- files[basename(files) == "config.json"]
  for (f in cfg) {
    d <- dirname(f)
    if (any(dirname(files) == d & grepl("\\.weights\\.h5$", files, ignore.case = TRUE))) {
      return(d)
    }
  }
  NA_character_
}

#' Load a KerasHub preset
#'
#' @param dir The preset directory.
#' @return For language models, `function(prompt, max_length, sampler,
#'   strip_prompt)`; otherwise `function(input_data)`.
#' @keywords internal
#' @noRd
.load_keras_hub <- function(dir) {
  .require_pkg("reticulate")
  message("--> Loading KerasHub preset...")
  if (requireNamespace("keras3", quietly = TRUE)) loadNamespace("keras3")
  if ("py_require" %in% getNamespaceExports("reticulate")) {
    try(getExportedValue("reticulate", "py_require")("keras-hub"), silent = TRUE)
  }
  if (!reticulate::py_module_available("keras_hub")) {
    stop("This model is a KerasHub preset. Install the Python package once with:\n",
         "  reticulate::py_install(\"keras-hub\")",
         call. = FALSE
    )
  }
  kh <- reticulate::import("keras_hub")
  
  # Pick the class: task.json if present; otherwise a text backbone with a
  # matching *CausalLM class is a language model (GPT2Backbone -> GPT2CausalLM).
  # Current Kaggle presets (incl. GPT-2) ship no task.json.
  task_json <- file.path(dir, "task.json")
  class_name <- "Backbone"
  module <- NULL
  if (file.exists(task_json)) {
    cfg <- jsonlite::fromJSON(task_json, simplifyVector = FALSE)
    class_name <- cfg$class_name
    module <- cfg$module
  } else if (file.exists(file.path(dir, "tokenizer.json"))) {
    backbone <- jsonlite::fromJSON(file.path(dir, "config.json"),
                                   simplifyVector = FALSE
    )$class_name %||% ""
    lm <- sub("Backbone$", "CausalLM", backbone)
    if (grepl("Backbone$", backbone) && reticulate::py_has_attr(kh$models, lm)) {
      class_name <- lm
    }
  }
  cls <- if (reticulate::py_has_attr(kh$models, class_name)) {
    reticulate::py_get_attr(kh$models, class_name)
  } else {
    reticulate::py_get_attr(reticulate::import(module), class_name)
  }
  
  # KerasHub's preprocessing layers need tensorflow + tensorflow-text, and
  # tensorflow-text has no Windows build after 2.10. Without them, image
  # models skip KerasHub's preprocessor and are preprocessed in R instead.
  plan <- keras_hub_preprocessing_plan(dir, have_tf_libs = keras_hub_tf_libs_available())
  if (plan == "unsupported_text") stop(tf_text_missing_message(), call. = FALSE)
  
  message("--> Loading as keras_hub.models.", class_name)
  if (plan != "kerashub") {
    message(
      "--> tensorflow-text isn't available, so KerasHub preprocessing is skipped",
      switch(plan,
             r_image = " and images are resized/rescaled in R",
             r_bpe = " and text is tokenized in R",
             ""
      )
    )
  }
  args <- if (plan == "kerashub") list(dir) else list(dir, preprocessor = NULL)
  
  # No silent fallback: if loading fails, show KerasHub's real error.
  model <- tryCatch(
    do.call(cls$from_preset, args),
    error = function(e) {
      stop("KerasHub couldn't load this preset as ", class_name, ":\n",
           conditionMessage(e),
           call. = FALSE
      )
    }
  )
  
  # Language models (GPT-2, OPT, SmolLM, ...) get a text-generation function.
  if (reticulate::py_has_attr(model, "generate")) {
    message("--> Language model: call the result with a prompt string.")
    if (plan == "r_bpe") {
      return(.bpe_generator(model, .bpe_tokenizer(dir)))
    }
    current_sampler <- NULL
    return(function(prompt, max_length = 64L, sampler = "top_k", strip_prompt = FALSE) {
      # Recompiling is slow, so only do it when the sampler changes.
      if (!identical(sampler, current_sampler)) {
        model$compile(sampler = sampler)
        current_sampler <<- sampler
      }
      prompt <- as.character(prompt)
      out <- model$generate(
        if (length(prompt) == 1) prompt else as.list(prompt),
        max_length = as.integer(max_length),
        strip_prompt = strip_prompt
      )
      unlist(out)
    })
  }
  
  converter <- if (plan == "r_image") keras_hub_image_converter(dir) else NULL
  prep <- if (is.null(converter)) {
    identity
  } else {
    function(x) apply_image_converter(x, converter)
  }
  function(input_data) model$predict(prep(.as_numeric_input(input_data)))
}

#' Are the Python libraries for KerasHub preprocessing available?
#'
#' @return `TRUE` if both `tensorflow` and `tensorflow_text` can be imported.
#' @keywords internal
#' @noRd
keras_hub_tf_libs_available <- function() {
  reticulate::py_module_available("tensorflow") &&
    reticulate::py_module_available("tensorflow_text")
}

#' Decide how to preprocess inputs for a KerasHub preset
#'
#' @param dir The preset directory.
#' @param have_tf_libs Whether `tensorflow` and `tensorflow_text` are available.
#' @return One of:
#'   * `"kerashub"`: KerasHub's own preprocessor (tensorflow-text available);
#'   * `"r_image"`: image model; skip KerasHub's preprocessor and do it in R;
#'   * `"none"`: no preprocessing config; pass inputs through as-is;
#'   * `"r_bpe"`: GPT-2 text model; tokenize in R (see `.bpe_tokenizer()`);
#'   * `"unsupported_text"`: other text model without tensorflow-text.
#' @keywords internal
#' @noRd
keras_hub_preprocessing_plan <- function(dir, have_tf_libs) {
  if (have_tf_libs) {
    return("kerashub")
  }
  tok_json <- file.path(dir, "tokenizer.json")
  if (file.exists(tok_json)) {
    tok_class <- jsonlite::fromJSON(tok_json, simplifyVector = FALSE)$class_name %||% ""
    if (tok_class == "GPT2Tokenizer" && !is.null(.bpe_files(dir))) {
      return("r_bpe")
    }
    return("unsupported_text")
  }
  if (!is.null(keras_hub_image_converter(dir))) {
    return("r_image")
  }
  "none"
}

#' Error message for text models that need tensorflow-text
#'
#' @return A single string.
#' @keywords internal
#' @noRd
tf_text_missing_message <- function() {
  paste0(
    "This KerasHub text model's tokenizer needs the Python package 'tensorflow-text' ",
    "(only GPT-2 models can be tokenized in R without it).\n",
    if (.Platform$OS.type == "windows") {
      paste0(
        "tensorflow-text has no Windows build after version 2.10, so this can't run in ",
        "native Windows R. Run it in WSL2 (Ubuntu) or on Linux/macOS instead."
      )
    } else {
      "Install it with: reticulate::py_install(\"tensorflow-text\")"
    }
  )
}

#' Load a full saved Keras model file
#'
#' @param path Path to a `.keras`, `.h5` or `.hdf5` file.
#' @return `function(input_data)`.
#' @keywords internal
#' @noRd
.load_keras_file <- function(path) {
  message("--> Loading Keras model...")
  if (requireNamespace("keras3", quietly = TRUE)) {
    model <- keras3::load_model(path)
  } else if (requireNamespace("keras", quietly = TRUE)) {
    model <- if (grepl("\\.(h5|hdf5)$", path, ignore.case = TRUE)) {
      keras::load_model_hdf5(path)
    } else {
      keras::load_model_tf(path)
    }
  } else {
    .require_pkg("keras3")
  }
  function(input_data) stats::predict(model, .as_numeric_input(input_data))
}

#' Load a TensorFlow SavedModel
#'
#' Plain TensorFlow loading works for TF Hub models (which aren't Keras
#' models) as well as SavedModels exported from Keras.
#'
#' @param dir The SavedModel directory.
#' @return `function(input_data)`.
#' @keywords internal
#' @noRd
.load_saved_model <- function(dir) {
  .require_pkg("tensorflow")
  .require_pkg("reticulate")
  message("--> Loading TensorFlow SavedModel...")
  tf <- tensorflow::tf
  obj <- tf$saved_model$load(dir)
  
  call_model <- if (reticulate::py_has_attr(obj, "__call__")) {
    function(x) reticulate::py_call(obj, x)
  } else {
    sig <- obj$signatures[["serving_default"]]
    arg <- names(sig$structured_input_signature[[2]])[1]
    function(x) {
      out <- do.call(reticulate::py_call, c(list(sig), stats::setNames(list(x), arg)))
      if (is.list(out) && length(out) == 1) out[[1]] else out
    }
  }
  
  to_r <- function(x) if (is.list(x)) lapply(x, to_r) else as.array(x)
  function(input_data) {
    x <- tf$constant(as.array(.as_numeric_input(input_data)), dtype = tf$float32)
    to_r(call_model(x))
  }
}

#' Read a KerasHub preset's image converter settings
#'
#' Searches the preset's JSON files for a `*ImageConverter` object and returns
#' its settings (`image_size`, `scale`, `offset`, ...).
#'
#' @param dir The preset directory.
#' @return The converter's config list, or `NULL` if there is none.
#' @keywords internal
#' @noRd
keras_hub_image_converter <- function(dir) {
  candidates <- file.path(dir, c("image_converter.json", "preprocessor.json", "task.json"))
  for (f in candidates[file.exists(candidates)]) {
    hit <- find_image_converter(jsonlite::fromJSON(f, simplifyVector = FALSE))
    if (!is.null(hit)) {
      return(hit)
    }
  }
  NULL
}

#' Recursively search parsed JSON for an image converter
#'
#' @param x A parsed JSON value.
#' @return The first `*ImageConverter` config found, or `NULL`.
#' @keywords internal
#' @noRd
find_image_converter <- function(x) {
  if (!is.list(x)) {
    return(NULL)
  }
  cls <- x[["class_name"]]
  if (is.character(cls) && grepl("ImageConverter$", cls)) {
    return(x[["config"]] %||% list())
  }
  for (el in x) {
    hit <- find_image_converter(el)
    if (!is.null(hit)) {
      return(hit)
    }
  }
  NULL
}

#' Apply KerasHub's ImageConverter in R
#'
#' Resizes to `image_size`, then computes `input * scale + offset` (scalar or
#' per channel, channels last).
#'
#' @param x An image array `[height, width, channels]` or a batch
#'   `[batch, height, width, channels]`.
#' @param cfg Converter settings from `keras_hub_image_converter()`.
#' @return The preprocessed batch as an array.
#' @keywords internal
#' @noRd
apply_image_converter <- function(x, cfg) {
  x <- as.array(x)
  if (length(dim(x)) == 3) dim(x) <- c(1L, dim(x)) # single image -> batch of 1
  if (length(dim(x)) != 4) {
    stop("Expected images as an array [batch, height, width, channels].", call. = FALSE)
  }
  
  size <- as.integer(unlist(cfg[["image_size"]]))
  if (length(size) == 2 && !identical(dim(x)[2:3], size)) {
    .require_pkg("keras3")
    x <- as.array(keras3::op_image_resize(
      x,
      size = size,
      interpolation = cfg[["interpolation"]] %||% "bilinear",
      crop_to_aspect_ratio = isTRUE(cfg[["crop_to_aspect_ratio"]])
    ))
  }
  
  x <- per_channel(x, cfg[["scale"]], `*`)
  per_channel(x, cfg[["offset"]], `+`)
}

#' Apply a scalar or per-channel operation to an image array
#'
#' @param x An array with channels in the last dimension.
#' @param v A scalar, a per-channel vector, or `NULL` (no-op).
#' @param op A binary function such as `*` or `+`.
#' @return The transformed array.
#' @keywords internal
#' @noRd
per_channel <- function(x, v, op) {
  v <- unlist(v)
  if (is.null(v)) {
    return(x)
  }
  if (length(v) == 1) {
    return(op(x, v))
  }
  n_channels <- dim(x)[length(dim(x))]
  if (length(v) != n_channels) {
    stop("Image has ", n_channels, " channels but the model expects ",
         length(v), ".",
         call. = FALSE
    )
  }
  sweep(x, length(dim(x)), v, op)
}

# GPT-2 tokenizer in plain R
#
# KerasHub's GPT-2 tokenizer needs tensorflow-text (no Windows build), so this
# re-implements GPT-2's byte-level BPE from the preset's own vocabulary.json
# and merges.txt, and feeds token ids to the model directly.

#' Find a preset's BPE vocabulary and merges files
#'
#' @param dir The preset directory.
#' @return A list with `vocab` and `merges` paths, or `NULL` if either is
#'   missing.
#' @keywords internal
#' @noRd
.bpe_files <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  vocab <- files[basename(files) == "vocabulary.json"]
  merges <- files[basename(files) == "merges.txt"]
  if (!length(vocab) || !length(merges)) {
    return(NULL)
  }
  list(vocab = vocab[[1]], merges = merges[[1]])
}

#' Build a GPT-2 byte-level BPE tokenizer
#'
#' @param dir The preset directory.
#' @return A list with functions `encode(text)` and `decode(ids)`, and
#'   `end_id`, the id of `<|endoftext|>` (or `NULL`).
#' @keywords internal
#' @noRd
.bpe_tokenizer <- function(dir) {
  f <- .bpe_files(dir)
  
  # GPT-2 maps each byte to a printable character (space -> "Ġ", ...).
  bs <- c(33:126, 161:172, 174:255)
  cs <- bs
  extra <- setdiff(0:255, bs)
  bs <- c(bs, extra)
  cs <- c(cs, 256L + seq_along(extra) - 1L)
  char_byte <- stats::setNames(bs, intToUtf8(cs, multiple = TRUE))
  
  # Internally every token is a string of 2-digit hex byte codes ("4865" = "He").
  # Plain-ASCII keys make hashed lookups safe whatever the system encoding
  # (environment keys are converted to the native encoding, which would mangle
  # "Ġthe" on e.g. Windows R < 4.2).
  to_hex <- function(tokens) {
    chars <- strsplit(enc2utf8(tokens), "", fixed = TRUE)
    hex <- sprintf("%02x", char_byte[unlist(chars)])
    vapply(split(hex, rep.int(seq_along(chars), lengths(chars))), paste, character(1),
           collapse = "", USE.NAMES = FALSE
    )
  }
  
  vocab <- unlist(jsonlite::fromJSON(f$vocab, simplifyVector = FALSE)) # token -> id
  vocab_hex <- to_hex(names(vocab))
  vocab_ids <- as.integer(vocab)
  token_id <- list2env(stats::setNames(as.list(vocab_ids), vocab_hex), hash = TRUE)
  id_hex <- character(max(vocab_ids) + 1)
  id_hex[vocab_ids + 1] <- vocab_hex
  
  merges <- readLines(f$merges, encoding = "UTF-8", warn = FALSE)
  merges <- merges[nzchar(merges) & !startsWith(merges, "#version")]
  halves <- do.call(rbind, strsplit(merges, " ", fixed = TRUE))
  merge_keys <- paste(to_hex(halves[, 1]), to_hex(halves[, 2]))
  ranks <- list2env(stats::setNames(as.list(seq_along(merge_keys)), merge_keys), hash = TRUE)
  
  cache <- new.env(hash = TRUE)
  pattern <- paste0(
    "(*UCP)'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+",
    "|\\s+(?!\\S)|\\s+"
  )
  
  # Standard GPT-2 BPE on one pre-token: repeatedly merge the lowest-ranked
  # adjacent pair (all its occurrences) until no pair is in the merge list.
  bpe <- function(parts) {
    key <- paste(parts, collapse = "")
    hit <- get0(key, envir = cache, inherits = FALSE)
    if (!is.null(hit)) {
      return(hit)
    }
    while (length(parts) > 1) {
      pairs <- paste(parts[-length(parts)], parts[-1])
      r <- unlist(mget(pairs, envir = ranks, ifnotfound = Inf), use.names = FALSE)
      best <- which.min(r)
      if (is.infinite(r[best])) break
      a <- parts[best]
      b <- parts[best + 1]
      merged <- character(0)
      i <- 1L
      n <- length(parts)
      while (i <= n) {
        if (i < n && parts[i] == a && parts[i + 1] == b) {
          merged <- c(merged, paste0(a, b))
          i <- i + 2L
        } else {
          merged <- c(merged, parts[i])
          i <- i + 1L
        }
      }
      parts <- merged
    }
    assign(key, parts, envir = cache)
    parts
  }
  
  encode <- function(text) {
    text <- enc2utf8(text)
    pieces <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
    tokens <- unlist(lapply(pieces, function(p) {
      bpe(sprintf("%02x", as.integer(charToRaw(p))))
    }))
    unlist(mget(tokens, envir = token_id), use.names = FALSE)
  }
  
  decode <- function(ids) {
    if (!length(ids)) {
      return("")
    }
    hex <- paste(id_hex[ids + 1], collapse = "")
    bytes <- strtoi(substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2)), 16L)
    out <- rawToChar(as.raw(bytes[bytes != 0]))
    Encoding(out) <- "UTF-8"
    out
  }
  
  end_id <- get0(to_hex("<|endoftext|>"), envir = token_id, inherits = FALSE)
  list(encode = encode, decode = decode, end_id = end_id)
}

#' Text generation for a CausalLM loaded without its preprocessor
#'
#' Packs prompts into `token_ids` / `padding_mask` (zeros after each prompt
#' are filled in by the model, so the matrix width is `max_length`), then
#' decodes the result.
#'
#' @param model A KerasHub CausalLM loaded with `preprocessor = NULL`.
#' @param tok A tokenizer from `.bpe_tokenizer()`.
#' @return `function(prompt, max_length, sampler, strip_prompt)` returning a
#'   character vector.
#' @keywords internal
#' @noRd
.bpe_generator <- function(model, tok) {
  current_sampler <- NULL
  stop_ids <- if (is.null(tok$end_id)) NULL else reticulate::tuple(as.integer(tok$end_id))
  
  function(prompt, max_length = 64L, sampler = "top_k", strip_prompt = FALSE) {
    if (!identical(sampler, current_sampler)) { # recompiling is slow
      model$compile(sampler = sampler)
      current_sampler <<- sampler
    }
    prompt <- as.character(prompt)
    max_length <- as.integer(max_length)
    ids <- lapply(prompt, tok$encode)
    n <- lengths(ids)
    if (any(n == 0)) stop("Prompts must not be empty.", call. = FALSE)
    if (any(n >= max_length)) {
      stop("max_length (", max_length, ") must be larger than the prompt length (",
           max(n), " tokens).",
           call. = FALSE
      )
    }
    
    token_ids <- matrix(0L, length(prompt), max_length)
    padding_mask <- matrix(FALSE, length(prompt), max_length)
    for (i in seq_along(ids)) {
      token_ids[i, seq_len(n[i])] <- ids[[i]]
      padding_mask[i, seq_len(n[i])] <- TRUE
    }
    
    out <- model$generate(
      list(token_ids = token_ids, padding_mask = padding_mask),
      stop_token_ids = stop_ids
    )
    out_ids <- .to_r_matrix(out$token_ids)
    out_mask <- .to_r_matrix(out$padding_mask)
    
    vapply(seq_along(prompt), function(i) {
      row <- out_ids[i, as.logical(out_mask[i, ])]
      if (strip_prompt) row <- row[-seq_len(n[i])]
      if (!is.null(tok$end_id)) row <- row[row != tok$end_id]
      tok$decode(row)
    }, character(1))
  }
}

#' Convert a backend tensor to an R matrix
#'
#' `generate()` may return backend tensors (TF/JAX/torch) instead of numpy
#' arrays.
#'
#' @param x A Python tensor or an R array.
#' @return An R matrix.
#' @keywords internal
#' @noRd
.to_r_matrix <- function(x) {
  if (inherits(x, "python.builtin.object")) {
    keras_ops <- reticulate::import("keras", convert = FALSE)$ops
    x <- reticulate::py_to_r(keras_ops$convert_to_numpy(x))
  }
  as.matrix(x)
}

#' Load a TorchScript model
#'
#' R torch can only load models saved with `torch.jit.save()` (traced or
#' scripted), not `torch.save()` state dicts or pickled modules.
#'
#' @param all_files Paths of all downloaded files.
#' @return `function(input_data)`.
#' @keywords internal
#' @noRd
.load_torch_script <- function(all_files) {
  .require_pkg("torch")
  pt_file <- .first_match(all_files, "\\.pt$")
  if (is.na(pt_file)) pt_file <- .first_match(all_files, "\\.(pth|ts|torchscript)$")
  if (is.na(pt_file)) stop("No .pt/.pth TorchScript file found.", call. = FALSE)
  
  message("--> Loading TorchScript model...")
  model <- tryCatch(
    torch::jit_load(pt_file),
    error = function(e) {
      stop(
        "Could not load '", basename(pt_file), "' as TorchScript. R torch can only load ",
        "models saved with torch.jit.save() (traced/scripted), not torch.save() ",
        "state dicts or pickled modules.\nOriginal error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  try(model$eval(), silent = TRUE)
  
  to_r <- function(x) {
    if (inherits(x, "torch_tensor")) {
      torch::as_array(x$cpu())
    } else if (is.list(x)) {
      lapply(x, to_r)
    } else {
      x
    }
  }
  
  function(input_data) {
    x <- if (inherits(input_data, "torch_tensor")) {
      input_data
    } else {
      torch::torch_tensor(
        as.array(.as_numeric_input(input_data)),
        dtype = torch::torch_float()
      )
    }
    to_r(torch::with_no_grad(model(x)))
  }
}

#' Find native XGBoost model files
#'
#' JSON files only count if they look like an XGBoost model (contain a
#' `"learner"` key). Candidates are ordered by preferred format.
#'
#' @param files Paths of all downloaded files.
#' @return Character vector of candidate model files, best first.
#' @keywords internal
#' @noRd
.xgb_candidates <- function(files) {
  cand <- grep("\\.(ubj|json|model|bin|xgb)$", files, value = TRUE, ignore.case = TRUE)
  is_model_json <- function(f) {
    !grepl("\\.json$", f, ignore.case = TRUE) ||
      grepl("\"learner\"", paste(readLines(f, n = 50, warn = FALSE), collapse = ""),
            fixed = TRUE
      )
  }
  cand <- Filter(is_model_json, cand)
  preferred <- c("ubj", "json", "model", "xgb", "bin")
  cand[order(match(tolower(tools::file_ext(cand)), preferred))]
}

#' Load a native XGBoost model
#'
#' @param all_files Paths of all downloaded files.
#' @return `function(input_data)`.
#' @keywords internal
#' @noRd
.load_xgboost <- function(all_files) {
  .require_pkg("xgboost")
  cand <- .xgb_candidates(all_files)
  if (!length(cand)) {
    stop("No native XGBoost model (.ubj/.json/.model/.bin) found. ",
         "Pickle/joblib files can't be read from R.",
         call. = FALSE
    )
  }
  message("--> Loading XGBoost model...")
  model <- xgboost::xgb.load(cand[[1]])
  function(input_data) {
    stats::predict(model, newdata = as.matrix(.as_numeric_input(input_data)))
  }
}

#' Load a GGUF language model
#'
#' Runs the model with llama.cpp through the `localLLM` package; no Python is
#' needed. Instruct/chat models (detected from the file name) get their chat
#' template applied; base models get raw text.
#'
#' @param all_files Paths of all downloaded files.
#' @return `function(prompt, max_length, sampler, strip_prompt, chat,
#'   system_prompt, seed)` returning a character vector.
#' @keywords internal
#' @noRd
.load_gguf <- function(all_files) {
  .require_pkg("localLLM")
  ggufs <- grep("\\.gguf$", all_files, value = TRUE, ignore.case = TRUE)
  if (!length(ggufs)) stop("No .gguf file found.", call. = FALSE)
  path <- .pick_gguf(ggufs)
  
  if (!localLLM::lib_is_installed()) {
    message("--> Installing the llama.cpp backend for localLLM (one time)...")
    localLLM::install_localLLM()
  }
  message("--> GGUF language model: ", basename(path))
  
  is_chat <- grepl("instruct|chat|[-_.]it([-_.]|$)", basename(path), ignore.case = TRUE)
  
  function(prompt,
           max_length = 64L,
           sampler = "greedy",
           strip_prompt = TRUE,
           chat = is_chat,
           system_prompt = "You are a helpful assistant.",
           seed = 1234L) {
    s <- .gguf_sampler(sampler)
    prompt <- as.character(prompt)
    out <- localLLM::quick_llama(
      prompt,
      model_path = path,
      max_tokens = as.integer(max_length), # number of new tokens to generate
      temperature = s$temperature,
      top_k = s$top_k,
      top_p = s$top_p,
      auto_format = chat,
      system_prompt = system_prompt,
      seed = as.integer(seed),
      progress = FALSE
    )
    out <- unname(unlist(out))
    if (!strip_prompt && !chat) out <- paste0(prompt, out)
    out
  }
}

#' Choose one GGUF file from a variation
#'
#' If a variation ships several quantizations, prefer Q4_K_M, else the
#' smallest file.
#'
#' @param files Paths of `.gguf` files.
#' @return A single path.
#' @keywords internal
#' @noRd
.pick_gguf <- function(files) {
  q4 <- grep("q4_k_m", basename(files), ignore.case = TRUE)
  if (length(q4)) {
    return(files[q4[1]])
  }
  files[which.min(file.size(files))]
}

#' Sampling settings for a named sampler
#'
#' @param sampler One of `"greedy"`, `"top_k"` or `"top_p"`.
#' @return A list with `temperature`, `top_k` and `top_p`.
#' @keywords internal
#' @noRd
.gguf_sampler <- function(sampler) {
  switch(sampler,
         greedy = list(temperature = 0, top_k = 40L, top_p = 1),
         top_k  = list(temperature = 0.8, top_k = 40L, top_p = 1),
         top_p  = list(temperature = 0.8, top_k = 40L, top_p = 0.9),
         stop("sampler must be \"greedy\", \"top_k\" or \"top_p\".", call. = FALSE)
  )
}


#' Find a Hugging Face Transformers checkpoint directory
#'
#' A checkpoint is a directory holding a `config.json` with a `model_type`
#' field plus `*.safetensors` or `pytorch_model*.bin` weights.
#'
#' @param files Paths of all downloaded files.
#' @return The checkpoint directory, or `NA_character_` if there is none.
#' @keywords internal
#' @noRd
.hf_transformers_dir <- function(files) {
  cfg <- files[basename(files) == "config.json"]
  for (f in cfg) {
    d <- dirname(f)
    here <- basename(files[dirname(files) == d])
    has_weights <- any(grepl("\\.safetensors$|^pytorch_model.*\\.bin$", here))
    model_type <- tryCatch(
      jsonlite::fromJSON(f, simplifyVector = FALSE)$model_type,
      error = function(e) NULL
    )
    if (has_weights && is.character(model_type)) {
      return(d)
    }
  }
  NA_character_
}

#' Undo browser duplicate-download names
#'
#' Renames e.g. `tokenizer (1).json` to `tokenizer.json`, unless a file with
#' the clean name already exists. Loaders look for exact file names.
#'
#' @param dir Directory to tidy.
#' @keywords internal
#' @noRd
.tidy_download_names <- function(dir) {
  f <- list.files(dir, full.names = TRUE)
  clean <- sub(" \\([0-9]+\\)(\\.[^.]*)?$", "\\1", basename(f))
  todo <- clean != basename(f) & !file.exists(file.path(dir, clean))
  if (any(todo)) {
    file.rename(f[todo], file.path(dir, clean[todo]))
  }
  invisible(dir)
}

#' Load a Hugging Face Transformers language model
#'
#' Converts the checkpoint with KerasHub (`CausalLM$from_preset()` reads
#' Hugging Face `config.json` + safetensors directly for the architectures it
#' supports: GPT-2, Llama, Mistral, Gemma, Qwen, ...).
#'
#' @param all_files Paths of all downloaded files.
#' @return `function(prompt, max_length, sampler, strip_prompt)` returning a
#'   character vector of generated text.
#' @keywords internal
#' @noRd
.load_hf_transformers <- function(all_files) {
  dir <- .hf_transformers_dir(all_files)
  if (is.na(dir)) {
    stop("No Hugging Face checkpoint (config.json + safetensors) found.",
         call. = FALSE
    )
  }
  .tidy_download_names(dir)
  
  .require_pkg("reticulate")
  if (requireNamespace("keras3", quietly = TRUE)) loadNamespace("keras3")
  if ("py_require" %in% getNamespaceExports("reticulate")) {
    try(getExportedValue("reticulate", "py_require")("keras-hub"), silent = TRUE)
  }
  if (!reticulate::py_module_available("keras_hub")) {
    stop("Hugging Face checkpoints are loaded through KerasHub. Install it once with:\n",
         "  reticulate::py_install(\"keras-hub\")",
         call. = FALSE
    )
  }
  kh <- reticulate::import("keras_hub")
  
  arch <- jsonlite::fromJSON(file.path(dir, "config.json"),
                             simplifyVector = FALSE
  )$model_type
  message("--> Hugging Face checkpoint (", arch, "); converting with KerasHub...")
  
  lm <- tryCatch(
    kh$models$CausalLM$from_preset(dir),
    error = function(e) {
      stop("KerasHub couldn't load this '", arch, "' checkpoint:\n  ",
           conditionMessage(e),
           "\nIf KerasHub doesn't support this architecture, try a GGUF build ",
           "of the model instead (model_type = \"gguf\").",
           call. = FALSE
      )
    }
  )
  
  function(prompt, max_length = 64L, sampler = "greedy", strip_prompt = FALSE) {
    prompt <- as.character(prompt)
    lm$compile(sampler = sampler)
    # KerasHub's max_length counts the prompt tokens too.
    out <- lm$generate(
      if (length(prompt) == 1L) prompt else as.list(prompt),
      max_length = as.integer(max_length)
    )
    out <- as.character(unlist(out))
    if (strip_prompt) {
      keep <- startsWith(out, prompt)
      out[keep] <- trimws(substring(out[keep], nchar(prompt[keep]) + 1L), "left")
    }
    out
  }
}
#' Stop if a suggested package isn't installed
#'
#' @param pkg Package name.
#' @return `NULL`, invisibly; called for its error.
#' @keywords internal
#' @noRd
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Package '%s' is required. Install it with install.packages(\"%s\").",
      pkg, pkg
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' Name of the package this code lives in
#'
#' Used for the default cache directory, so it follows whatever package these
#' files are added to.
#'
#' @return A single string.
#' @keywords internal
#' @noRd
.pkg_name <- function() {
  utils::packageName(environment(.pkg_name)) %||% "load_model"
}

#' First file matching a pattern
#'
#' @param files Character vector of paths.
#' @param pattern A regular expression (case-insensitive).
#' @return The first match, or `NA_character_`.
#' @keywords internal
#' @noRd
.first_match <- function(files, pattern) {
  hit <- grep(pattern, files, value = TRUE, ignore.case = TRUE)
  if (length(hit)) hit[[1]] else NA_character_
}

#' Convert data frames to numeric matrices
#'
#' @param x Model input.
#' @return `x`, with data frames converted by [data.matrix()].
#' @keywords internal
#' @noRd
.as_numeric_input <- function(x) {
  if (is.data.frame(x)) data.matrix(x) else x
}

#' Default value for `NULL`
#'
#' @param a,b Any values.
#' @return `a` unless it is `NULL`, otherwise `b`.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

