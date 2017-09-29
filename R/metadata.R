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
  idx <- which(snaplocs::locs$loc == location)
  .no_loc(location, idx)
  as.character(snaplocs::locs$region[idx])
}

#' @export
#' @rdname metadata
get_country <- function(location){
  get_region(location) %>%
    purrr::map_chr(~ifelse(.x == "Alaska", "United States", "Canada"))
}

#' @export
#' @rdname metadata
get_coords <- function(location){
  idx <- which(snaplocs::locs$loc == location)
  .no_loc(location, idx)
  dplyr::filter(snaplocs::locs, .data[["loc"]] == location) %>%
    dplyr::select(.data[["lon"]], .data[["lat"]])
}

.no_loc <- function(location, idx){
  if(!length(idx)) stop(paste0("'", location, "' is not an available location in `locs`."))
}
