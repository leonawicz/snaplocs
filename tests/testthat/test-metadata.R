context("metadata")

test_that("metadata helpers return as expected", {
  x <- c("Fairbanks", "Calgary", "Vancouver", "Whitehorse")
  reg <- c("Alaska", "Alberta", "British Columbia", "Yukon")
  country <- c("United States", rep("Canada", 3))
  purrr::walk2(x, reg, ~expect_identical(get_region(.x), .y))
  purrr::walk2(x, country, ~expect_identical(get_country(.x), .y))
  purrr::walk(x, ~expect_is(get_coords(.x), "tbl_df"))
  purrr::walk(x, ~expect_identical(dim(get_coords(.x)), c(1L, 2L)))

  x <- c("Fairbanks", "Calgary", "Vancouver", "Whitehorse")
  reg <- c("Alaska", "Alberta", "British Columbia", "Yukon")
  country <- c("United States", rep("Canada", 3))
  expect_identical(get_region(x), reg)
  expect_identical(get_country(x), country)
  expect_is(get_coords(x), "tbl_df")
  expect_identical(dim(get_coords(x)), c(length(x), 2L))

  x <- "A"
  err <- paste0("'", x, "' is not an available location in `locs`.")
  expect_error(get_region(x), err)
  expect_error(get_country(x), err)
  expect_error(get_coords(x), err)

  x <- c("Fairbanks", "A")
  err <- paste0("At least one location is not an available location in `locs`.")
  expect_error(get_region(x), err)
  expect_error(get_country(x), err)
  expect_error(get_coords(x), err)
})
