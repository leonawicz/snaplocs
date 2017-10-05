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

test_that("metadata region argument works", {
  x <- rep(c("Fairbanks", "Galena"), 2)
  reg <- rep(c("Alaska", "British Columbia"), each = 2)
  country <- rep(c("United States", "Canada"), each = 2)
  good <- c(1:2, 4)
  bad <- 3
  purrr::walk2(x[good], reg[good], ~expect_identical(get_region(.x, .y), .y))
  purrr::walk(good, ~expect_identical(get_country(x[.x], reg[.x]), country[.x]))
  purrr::walk(x[good], ~expect_is(get_coords(.x, .y), "tbl_df"))
  purrr::walk2(x[good], reg[good], ~expect_identical(dim(get_coords(.x, .y)), c(1L, 2L)))

  x <- "Fairbanks"
  reg <- "British Columbia"
  err <- paste0("'", x, "' is not an available location in `locs`.")
  expect_error(get_region(x, reg), err)
  expect_error(get_country(x, reg), err)
  expect_error(get_coords(x, reg), err)

  x <- c("Fairbanks")
  reg <- "A"
  err <- "Invalid `region`."
  expect_error(get_region(x, reg), err)
  expect_error(get_country(x, reg), err)
  expect_error(get_coords(x, reg), err)
})
