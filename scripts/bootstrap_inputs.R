suppressPackageStartupMessages({
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
repkit_index <- match("--repkit", args)
if (is.na(repkit_index) || repkit_index == length(args)) {
  stop("Usage: Rscript scripts/bootstrap_inputs.R --repkit ../final_repkit", call. = FALSE)
}

repkit <- normalizePath(args[[repkit_index + 1]], mustWork = FALSE)
if (!dir.exists(repkit)) {
  stop(sprintf("Replication kit path does not exist: %s", repkit), call. = FALSE)
}

input_dir <- Sys.getenv("DEFICIT_SCORECARD_INPUT_DIR", "data-raw")
manifest_path <- file.path("config", "input_manifest.csv")
manifest <- read_csv(manifest_path, show_col_types = FALSE)

required_columns <- c("source_path", "dest_path", "description")
missing_columns <- setdiff(required_columns, names(manifest))
if (length(missing_columns) > 0) {
  stop(sprintf("Input manifest is missing columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)
}

for (i in seq_len(nrow(manifest))) {
  source_file <- file.path(repkit, manifest$source_path[[i]])
  dest_file <- file.path(input_dir, manifest$dest_path[[i]])
  if (!file.exists(source_file)) {
    stop(sprintf("Required source file is missing: %s", source_file), call. = FALSE)
  }
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(source_file, dest_file, overwrite = TRUE)
  if (!ok) {
    stop(sprintf("Failed to copy %s to %s", source_file, dest_file), call. = FALSE)
  }
}

cat(sprintf("Copied %s input files into %s\n", nrow(manifest), input_dir))
