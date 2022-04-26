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
################################################################
#                        end of match_exactly
################################################################

#' Try to repair the street name
#'
#' We will search in the given zipcode for street names that are close
#' to the given name, using the generalized Levenshtein (edit) distance.
#' We will ignore street names likely to fail badly:
#' * names with fewer than 4 letters
#' * names containing STREET (A STREET, B STREET)
#' * Number streets (4TH, 5TH, etc)
#' * County Roads (CR 235)
#' On success, the Lat Long will be returned. Maybe several.
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @param  Distance    The zip code for the address
#' @return A tibble will be returned with the Longitude (Lon), Latitude (Lat),
#'     a success flag  (Success) (T/F), and a Fail code describing why
#'     the repair attempt failed.
#'     There may be several points returned if several matched.
#' @examples
#' repair_name("1311", "", "Tuline", "ST", "77008", Distance=2)
repair_name <- function( Street_num, Prefix="", Street_name,
                         Street_type="", Zipcode, Distance=2) {

  Prefix      <- stringr::str_to_upper(Prefix)
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  if ((stringr::str_length(Street_name) < 4) | # no short names
      (stringr::str_detect(Street_name, "STREET")) | # no A STREET, etc
      (stringr::str_detect(Street_name, "^\\d+TH")) | # no 4TH, 5TH, etc
      (stringr::str_detect(Street_name, "^CR \\d+"))) { # no county roads

      return(tibble::tribble(~Lon, ~Lat, ~New_name, ~Success, ~Fail,
                              0,    0,  "No Name",  FALSE,   "Short or Ambiguous"))
  }
  #####################################################33
  # Archive_path <- "/home/ajackson/Dropbox/Rprojects/Curated_Data_Files/CoH_Address_Points/"
  # dt <- readRDS(paste0(Archive_path, "COH_Geocoding_Locations_table.rds"))
  # library(tidyverse)
  # Street_num <- "1311"
  # Street_name <- "TULINE"
  # Prefix <- ""
  # Street_type <- "ST"
  # Zipcode <- "77008"
  # Distance <- 2
  #####################################################33

  df_names_only <- dt %>%
    tibble::as_tibble() %>%
    dplyr::group_by(Zipcode, Street_name, Street_type, Prefix) %>%
    dplyr::summarize(Zipcode=dplyr::last(Zipcode),
              Street_name=dplyr::last(Street_name),
              Street_type=dplyr::last(Street_type),
              Prefix=dplyr::last(Prefix),
              n=dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n>1)  # I don't trust the singletons


  tmp <- df_names_only %>%
    #   Filter out streets where this will likely fail
    dplyr::filter(stringr::str_length(Street_name)>3) %>% #  eliminate short names
    dplyr::filter(!stringr::str_detect(Street_name, "STREET")) %>% # eliminate A STREET, B STREET
    dplyr::filter(!stringr::str_detect(Street_name, "^\\d+TH")) %>% # eliminate 4TH, 5TH, etc
    dplyr::filter(!stringr::str_detect(Street_name, "^CR \\d+")) %>%  # eliminate county roads
    dplyr::filter(Zipcode==!!Zipcode,
                  Street_type==!!Street_type,
                  Prefix==!!Prefix)  %>%
    dplyr::mutate(dist=utils::adist(Street_name, !!Street_name)) %>%
    dplyr::filter(dist<=Distance,
                  dist > 0)

  if(nrow(tmp)>0){
    #   Now look for address with new name(s) in hand

    tmp_new <- NULL
    for (i in 1:nrow(tmp)) { # check each possible street name
      foo <- match_exactly(Street_num, Prefix, tmp[i,]$Street_name,
                           Street_type, Zipcode)
      if (foo$Success) {
        tmp_new <- rbind(tmp_new, tibble::tribble(~Lon, ~Lat, ~New_name,
                                                  ~Success, ~Fail,
                                                 foo[i,]$Lon, foo[i,]$Lat,
                                                 tmp[i,]$Street_name, TRUE, ""))
      }
    }
    if (!is.null(tmp_new)) {
      return(tmp_new)
    }
  }
  return(tibble::tribble(~Lon, ~Lat, ~New_name, ~Success, ~Fail,
                          0,    0,  "No Name",  FALSE,   "No name found"))
}

################################################################
#                        end of repair_name
################################################################


Testme <- function(){
  df <- tibble::as_tibble(dt)
  print(df[1,])
}








