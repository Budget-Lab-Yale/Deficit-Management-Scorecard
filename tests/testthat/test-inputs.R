test_that("bundled inputs are present and match the manifest", {
  manifest <- readr::read_csv(
    file.path(scorecard_repo_root, "config", "input_manifest.csv"),
    show_col_types = FALSE
  )
  paths <- file.path(scorecard_input_dir, manifest$path)

  expect_true(all(file.exists(paths)), info = paste(paths[!file.exists(paths)], collapse = "\n"))
  actual_hashes <- vapply(
    paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  expect_identical(unname(actual_hashes), manifest$sha256)
})
