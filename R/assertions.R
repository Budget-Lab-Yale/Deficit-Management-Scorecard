abort <- function(message) {
  stop(message, call. = FALSE)
}

assert_required_columns <- function(data, columns, label = deparse(substitute(data))) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0) {
    abort(sprintf(
      "%s is missing required columns: %s",
      label,
      paste(missing_columns, collapse = ", ")
    ))
  }
  invisible(data)
}

assert_no_missing <- function(data, columns, label = deparse(substitute(data))) {
  assert_required_columns(data, columns, label)
  missing_counts <- vapply(data[columns], function(x) sum(is.na(x)), integer(1))
  missing_counts <- missing_counts[missing_counts > 0]
  if (length(missing_counts) > 0) {
    abort(sprintf(
      "%s has unexpected missing values: %s",
      label,
      paste(sprintf("%s=%s", names(missing_counts), missing_counts), collapse = ", ")
    ))
  }
  invisible(data)
}

assert_unique_key <- function(data, key, label = deparse(substitute(data))) {
  assert_required_columns(data, key, label)
  duplicates <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(key)), name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(duplicates) > 0) {
    abort(sprintf(
      "%s has duplicate key rows for {%s}",
      label,
      paste(key, collapse = ", ")
    ))
  }
  invisible(data)
}

checked_left_join <- function(x, y, by, x_label = "left data", y_label = "right data") {
  assert_unique_key(y, by, y_label)
  n_before <- nrow(x)
  joined <- dplyr::left_join(x, y, by = by)
  if (nrow(joined) != n_before) {
    abort(sprintf(
      "Joining %s to %s changed row count from %s to %s",
      y_label,
      x_label,
      n_before,
      nrow(joined)
    ))
  }
  joined
}

assert_files_exist <- function(paths) {
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    abort(sprintf(
      "Missing required files:\n%s",
      paste(missing_paths, collapse = "\n")
    ))
  }
  invisible(paths)
}

