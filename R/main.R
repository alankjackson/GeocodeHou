# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Search for exact matches
#'
#' Search for an exact address match in the following order:
#' Zipcode, Street Name, Street Type, Street Number, and Direction Prefix.
#' On success, the Lat Long will be returned.
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @return A tibble will be returned with the Longitude (Lon), Latitude (Lat),
#'     a success flag  (Success) (T/F), and a Fail code describing which
#'     part of the address failed.
#' @examples
#' match_exactly("1311", "", "Tulane", "ST", "77008")
match_exactly <- function( Street_num, Prefix="", Street_name,
                           Street_type="", Zipcode) {

  Prefix      <- stringr::str_to_upper(Prefix)
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  #####    Test Zipcode

  tmp <- dt[dt$Zipcode==Zipcode,]

  if (nrow(tmp)==0){
    return(tibble::tribble(~Lon, ~Lat, ~Success, ~Fail,
                              0,    0,    FALSE, "Zipcode"))
  }

  #####    Test Name

  tmp <- tmp[tmp$Street_name==Street_name,]

  if (nrow(tmp)==0){
    return(tibble::tribble(~Lon, ~Lat, ~Success, ~Fail,
                              0,    0,    FALSE, "Street_name"))
  }

  #####    Test Type

  tmp <- tmp[tmp$Street_type==Street_type,]

  if (nrow(tmp)==0){
    return(tibble::tribble(~Lon, ~Lat, ~Success, ~Fail,
                              0,    0,    FALSE, "Street_type"))
  }

  #####    Test Number

  tmp <- tmp[tmp$Street_num==Street_num,]

  if (nrow(tmp)==0){
    return(tibble::tribble(~Lon, ~Lat, ~Success, ~Fail,
                              0,    0,    FALSE, "Street_num"))
  }

  #####    Test Prefix

  tmp <- tmp[tmp$Prefix==Prefix,]

  if (nrow(tmp)==0){
    return(tibble::tribble(~Lon, ~Lat, ~Success, ~Fail,
                              0,    0,    FALSE, "Prefix"))
  }

  ####  Success

  return(tibble::tribble(~Lon,    ~Lat,       ~Success, ~Fail,
                         tmp$Lon, tmp$Lat,    TRUE,     ""))

}

Testme <- function(){
  df <- tibble::as_tibble(dt)
  print(df[1,])
}








