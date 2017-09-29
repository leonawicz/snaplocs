#' snaplocs: Functions for working with SNAP point location data.
#'
#' The \code{snaplocs} package provides functions for working with point location data used in various applications and projects by the
#' Scenarios Network for Alaska and Arctic Planning at the University of Alaska Fairbanks. Point location data is included.
#' Locations include major and minor cities, towns, villages, and some other locations such as mines.
#' This package is part of the SNAPverse collection of R packages.
#'
#' @docType package
#' @name snaplocs
NULL

#' @importFrom magrittr %>%
NULL

#' Get point location metadata.
#'
#' These functions return metadata associate with a point location.
#'
#' \code{get_region} returns the state, province or territory of a point location.
#' \code{get_country} returns the name of the country (US or Canada).
#' \code{get_coords} returns the longitutde and latitude of a point location as a data frame.
#'
#' @param location character, a name of a location in the \code{locs} data frame.
#' @name metadata
#'
#' @return a character string, or a dat frame for \code{get_coords}.
#' @export
#'
#' @examples
#' x <- "Calgary"
#' get_region(x)
#' get_country(x)
#' get_coords(x)
NULL

#' @export
#' @rdname metadata
get_region <- function(location){
  idx <- which(locs$loc == location)
  .no_loc(idx)
  as.character(locs$region[idx])
}

#' @export
#' @rdname metadata
get_country <- function(location){
  get_region(location) %>%
    purrr::map_chr(x, ~ifelse(.x == "Alaska", "United States", "Canada"))
}

#' @export
#' @rdname metadata
get_coords <- function(location){
  idx <- which(locs$loc == location)
  .no_loc(idx)
  dplyr::filter(locs, .data[["loc"]] == location) %>%
    dplyr::select(.data[["lon"]], .data[["lat"]])
}

.no_loc <- function(idx){
  if(!length(idx)) stop(paste("'", location, "' is not an available location in `locs`."))
}
