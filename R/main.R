
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
#' @return A tibble will be returned with the:
#' - Longitude (Lon)
#' - Latitude (Lat)
#' - a success flag  (Success) (T/F)
#' - Fail code describing which part of the address failed.
#' @examples
#' match_exactly("1311", "", "Tulane", "ST", "77008")
#' @export
match_exactly <- function( Street_num, Prefix="", Street_name,
                           Street_type="", Zipcode) {

  if (is.na(Prefix)) {Prefix <- ""}
  if (is.na(Street_type)) {Street_type <- ""}
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
#' On success, the Lat Long will be returned. Maybe several.
#' We will ignore street names likely to fail badly:
#' - names with fewer than 4 letters
#' - names containing STREET (A STREET, B STREET)
#' - Number streets (4TH, 5TH, etc)
#' - County Roads (CR 235)
#' - Farm-to-Market Roads (FM 1960)
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @param  Distance    The maximum edit distance allowed
#' @return A tibble will be returned with the:
#' - Longitude (Lon),
#' - Latitude (Lat),
#' - the new name (New_name),
#' - a success flag  (Success) (T/F),
#' - and a Fail code describing why the repair attempt failed.
#'
#'     There may be several points returned if several matched.
#' @examples
#' repair_name("1311", "", "Tuline", "ST", "77008", Distance=2)
#' @export
repair_name <- function( Street_num, Prefix="", Street_name,
                         Street_type="", Zipcode, Distance=2) {

  if (is.na(Prefix)) {Prefix <- ""}
  if (is.na(Street_type)) {Street_type <- ""}
  Prefix      <- stringr::str_to_upper(Prefix)
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  if ((stringr::str_length(Street_name) < 4) | # no short names
      (stringr::str_detect(Street_name, "STREET")) | # no A STREET, etc
      (stringr::str_detect(Street_name, "^\\d+TH")) | # no 4TH, 5TH, etc
      (stringr::str_detect(Street_name, "^CR \\d+"))| # no county roads
      (stringr::str_detect(Street_name, "^FM \\d+"))) { # no FM roads

      return(tibble::tribble(~Lon, ~Lat, ~New_name, ~Success, ~Fail,
                              0,    0,  "No Name",  FALSE,   "Short or Ambiguous"))
  }

  # df_names_only <- dt %>%
  #   tibble::as_tibble() %>%
  #   dplyr::group_by(Zipcode, Street_name, Street_type, Prefix) %>%
  #   dplyr::summarize(Zipcode=dplyr::last(Zipcode),
  #             Street_name=dplyr::last(Street_name),
  #             Street_type=dplyr::last(Street_type),
  #             Prefix=dplyr::last(Prefix),
  #             n=dplyr::n()
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(n>1)  # I don't trust the singletons


  tmp <- df_names_only %>%
    #   Filter out streets where this will likely fail
    dplyr::filter(stringr::str_length(Street_name)>3) %>% #  eliminate short names
    dplyr::filter(!stringr::str_detect(Street_name, "STREET")) %>% # eliminate A STREET, B STREET
    dplyr::filter(!stringr::str_detect(Street_name, "^\\d+TH")) %>% # eliminate 4TH, 5TH, etc
    dplyr::filter(!stringr::str_detect(Street_name, "^CR \\d+")) %>%  # eliminate county roads
    dplyr::filter(!stringr::str_detect(Street_name, "^FM \\d+")) %>%  # eliminate FM roads
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
                                                 foo$Lon, foo$Lat,
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

#' Try to repair the street type
#'
#' We will search in the given zipcode for street types that are available
#' for the given name, and if we only find one we are done. If there are
#' more than one, then we will search for the exact address for each.
#' On success, the Lat Long will be returned.
#' We will ignore street types likely to fail badly:
#' - CT
#' - CIR
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @return A tibble will be returned with the:
#' - Longitude (Lon),
#' - Latitude (Lat),
#' - the new type (New_type),
#' - a success flag  (Success) (T/F),
#' - and a Fail code describing why the repair attempt failed.
#' @examples
#' repair_type("1311", "", "Tulane", "", "77008")
#' @export
repair_type <- function( Street_num, Prefix="", Street_name,
                         Street_type="", Zipcode) {

  if (is.na(Prefix)) {Prefix <- ""}
  if (is.na(Street_type)) {Street_type <- ""}
  Prefix      <- stringr::str_to_upper(Prefix)
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  if ((stringr::str_detect(Street_type, "CT")) | # no Court
      (stringr::str_detect(Street_type, "CIR"))  # no Circle
     ) {

      return(tibble::tribble(~Lon, ~Lat, ~New_type, ~Success, ~Fail,
                              0,    0,  "No Type",  FALSE,   "CT or CIR"))
  }
  tmp <- dt %>%
         dplyr::filter(Zipcode==!!Zipcode,
                       Street_name==!!Street_name,
                       Prefix==!!Prefix) %>%
         dplyr::filter((Street_type!="CT") &
                       (Street_type!="CIR"))
  if (nrow(tmp)>0) { # yes Virginia there are some candidates
    # Gin up a list of available types
    types <- unique(tmp$Street_type)
    if (length(types)==1){ # found a single candidate, so possible success
      foo <- match_exactly(Street_num, Prefix, Street_name,
                           types[[1]], Zipcode)
      if (foo$Success) {
        return(tibble::tribble(~Lon, ~Lat, ~New_type,
                               ~Success, ~Fail,
                               foo$Lon, foo$Lat,
                               types[[1]], TRUE, ""))
      } else {Failmode <- paste("Type found but address not matched", types)}

    }  else {Failmode <- paste("Several Types found", types)}
  } else { Failmode <- "No Types found"}
  return(tibble::tribble(~Lon, ~Lat, ~New_type, ~Success, ~Fail,
                          0,    0,  "No Type",  FALSE, Failmode))
}

################################################################
#                        end of repair_type
################################################################

#' Try to repair the street prefix
#'
#' We will search in the given zipcode for street prefixes that are available
#' for the given name, and if we only find one we are done. If there are
#' more than one, then we will search for the exact address for each.
#' On success, the Lat Long will be returned.
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @return A tibble will be returned with the:
#' - Longitude (Lon),
#' - Latitude (Lat),
#' - The new prefix (New_prefix),
#' - a success flag  (Success) (T/F),
#' - and a Fail code describing why the repair attempt failed.
#' @examples
#' repair_prefix("1311", "", "Tulane", "", "77008")
#' @export
repair_prefix <- function( Street_num, Prefix="", Street_name,
                         Street_type="", Zipcode) {

  if (is.na(Street_type)) {Street_type <- ""}
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  tmp <- dt %>%
         dplyr::filter(Zipcode==!!Zipcode,
                       Street_name==!!Street_name,
                       Street_type==!!Street_type)

  if (nrow(tmp)>0) { # yes Virginia there are some candidates
    # Gin up a list of available types
    Prefixes <- unique(tmp$Prefix)
    if (length(Prefixes)==1){ # found a single candidate, so possible success
      foo <- match_exactly(Street_num, Prefixes[[1]], Street_name,
                           Street_type, Zipcode)
      if (foo$Success) {
        return(tibble::tribble(~Lon, ~Lat, ~New_prefix, ~Success, ~Fail,
                               foo$Lon, foo$Lat, Prefixes[[1]], TRUE, ""))

      } else {Failmode <- paste("Prefix found but address not matched", Prefixes)}

    }  else {Failmode <- paste("Several Prefix found", Prefixes)}
  } else { Failmode <- "No Prefix found"}
  return(tibble::tribble(~Lon, ~Lat, ~New_prefix, ~Success, ~Fail,
                          0,    0,  "No Type",  FALSE, Failmode))
}

################################################################
#                        end of repair_prefix
################################################################

#' Try to repair the street number
#'
#' We will search in the given zipcode for street numbers that are available
#' for the given name, and then try to interpolate. We will honor odd/even
#' to stay on one side of the street and look no further than 3 blocks total.
#' On success, the Lat Long will be returned.
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @param  Distance    The max distance in feet to interpolate, 250 default
#' @return A tibble will be returned with the:
#' - Longitude (Lon),
#' - Latitude (Lat),
#' - A description of the result - how far the interpolation was (Result),
#' - a success flag  (Success) (T/F),
#' - and a Fail code describing why the repair attempt failed.
#' @examples
#' repair_number("1313", "", "Tulane", "ST", "77008")
#' @export
repair_number <- function( Street_num, Prefix="", Street_name,
                         Street_type="", Zipcode, Distance=250) {

  if (is.na(Street_type)) {Street_type <- ""}
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  tmp <- dt %>%
         dplyr::filter(Zipcode==!!Zipcode,
                       Street_name==!!Street_name,
                       Prefix==!!Prefix,
                       Street_type==!!Street_type) %>%
    tibble::as_tibble()

  Target <- as.numeric(Street_num) # set up for comparisons
  tmp$Street_num <- as.numeric(tmp$Street_num)

  #   Restrict to one block
  Target_min <- round(Target, -2)
  Target_max <- Target_min+99

  #   Restrict to same side of street
  Sample <- tmp %>%
    dplyr::filter(dplyr::between(Street_num, (Target_min), (Target_max))) %>%
    dplyr::filter(Street_num%%2 == Target%%2)

  if (nrow(Sample)<2){ # hard to interpolate one value

    #   open up to 3 blocks
    Target_min <- round(Target, -2)-100
    Target_max <- Target_min+199
    Sample <- tmp %>%
      dplyr::filter(dplyr::between(Street_num, (Target_min), (Target_max))) %>%
      dplyr::filter(Street_num%%2 == Target%%2)
  }

  if (nrow(Sample)<2){ # If still too few, hang it up
    return(tibble::tribble(~Lon, ~Lat, ~Result, ~Success, ~Fail,
                            0,    0,  "No Number",  FALSE, "Too few neighbors"))
  }

  #   Main event. Collapse identical numbers,
  #   Calculate a delta lat/long scaled by delta address
  #   Then use the nearest address to apply that.

  Interpolator <-
    Sample %>%
    dplyr::arrange(Street_num) %>%
    dplyr::group_by(Street_num) %>%
    dplyr::summarize(Street_num=dplyr::last(Street_num),
                     Lon=mean(Lon),
                     Lat=mean(Lat)) %>%
    dplyr::mutate(Dx=Lon-dplyr::lag(Lon),
                  Dy=Lat-dplyr::lag(Lat),
                  Dnum=Street_num-dplyr::lag(Street_num)) %>%
    dplyr::mutate(Dx=(Dx/Dnum),
                  Dy=(Dy/Dnum))

  Interpolator[1,]$Dx <- Interpolator[2,]$Dx # replace NA
  Interpolator[1,]$Dy <- Interpolator[2,]$Dy # replace NA

  tmp2 <- Interpolator[which.min(abs(Interpolator$Street_num-Target)),]

  #New_lon <- tmp2$Lon-(tmp2$Street_num-Target)*mean(Interpolator$Dx)
  #New_lat <- tmp2$Lat-(tmp2$Street_num-Target)*mean(Interpolator$Dy)
  New_lon <- tmp2$Lon-(tmp2$Street_num-Target)*tmp2$Dx
  New_lat <- tmp2$Lat-(tmp2$Street_num-Target)*tmp2$Dy

  #   return the distance interpolated

  dist <- round(sqrt(((New_lon-tmp2$Lon)*364173)**2 +
                                  ((New_lat-tmp2$Lat)*364000)**2))
  cdist <- as.character(dist)

  if (dist < Distance) { # good
    return(tibble::tribble(~Lon, ~Lat, ~Result, ~Success, ~Fail,
                         New_lon, New_lat,
                         paste("Nearest address was", cdist, "feet") , TRUE, ""))
  } else {
    return(tibble::tribble(~Lon, ~Lat, ~Result, ~Success, ~Fail,
                            0,    0,  "No Number",  FALSE,
                           paste("Interp distance too large =", cdist)))
  }

}

################################################################
#                        end of repair_number
################################################################

#' Try to repair the zipcode
#'
#' We will search for an exact match in a zipcode, where there is no match
#' for the street name in the given zipcode.
#' On success, the Lat Long will be returned.
#'
#' @param  Street_num  Number portion of address.
#' @param  Prefix      Directional prefix (N, S, E, W) if exists.
#' @param  Street_name Name of the street.
#' @param  Street_type Designation like RD, ST, LN, etc.
#' @param  Zipcode     The zip code for the address
#' @return A tibble will be returned with the:
#' - Longitude (Lon),
#' - Latitude (Lat),
#' - The Zip code (New_zipcode),
#' - a success flag  (Success) (T/F),
#' - and a Fail code describing why the repair attempt failed.
#' @examples
#' repair_number("1311", "", "Tulane", "ST", "77088")
#' @export
repair_zipcode <- function( Street_num, Prefix="", Street_name,
                            Street_type="", Zipcode) {

  if (is.na(Street_type)) {Street_type <- ""}
  Street_name <- stringr::str_to_upper(Street_name)
  Street_type <- stringr::str_to_upper(Street_type)

  tmp <- dt %>%
         dplyr::filter(Street_num==!!Street_num,
                       Street_name==!!Street_name,
                       Prefix==!!Prefix,
                       Street_type==!!Street_type) %>%
    tibble::as_tibble()

  if (nrow(tmp)==1) { # found just one hit
    tmp2 <- dt %>%
         dplyr::filter(Street_name==!!Street_name,
                       Zipcode==!!Zipcode) %>%
      tibble::as_tibble()
    if(nrow(tmp2)==0) { # looks good, return success
      return(tibble::tribble(~Lon, ~Lat, ~New_zipcode, ~Success, ~Fail,
                             tmp$Lon, tmp$Lat, tmp$Zipcode, TRUE, ""))

    }
      #   found hit, but street also lives in original zip
    return(tibble::tribble(~Lon, ~Lat, ~New_zipcode, ~Success, ~Fail,
                            0,    0,  "No Zipcode",  FALSE,
                           paste("hit in", tmp$Zipcode, "but street lives in original")))
  }
  # found no hits or too many hits
  return(tibble::tribble(~Lon, ~Lat, ~New_zipcode, ~Success, ~Fail,
                          0,    0,  "No Zipcode",  FALSE, "No hits on address"))
}

################################################################
#                        end of repair_zipcode
################################################################

#' Try to delete stuff after the good address
#'
#' THIS IS EXPERIMENTAL AND NOT WELL TESTED
#' We will search for an exact match,  where there are more than 9
#' points with that address for direction, name, suffix
#' Return either the input or the cleaned value plus what was removed
#'
#' @param  Address :  Prefix, Name, Type.
#' @return A tibble will be returned with the:
#' - Good data (either the input or cleaned input)
#' - What was removed (may be blank)
#' @examples
#' delete_danglers("Tulane ST #5")
#' @export
delete_danglers <- function( Address) {

  Address <- stringr::str_to_upper(Address)

  tmp <- df_names_only %>%
    dplyr::mutate(Address=paste(Prefix, Street_name, Street_type)) %>%
    dplyr::group_by(Address) %>%
       dplyr::summarize(n=sum(n)) %>%
    dplyr::filter(n>9) %>%
    dplyr::mutate(Address=stringr::str_squish(Address))

  Output <- NULL
  for (i in 1:nrow(tmp)) {
    if(stringr::str_detect(Address, tmp[i,]$Address)){
      Output <- rbind(Output, tmp[i,])
      #print(tmp[i,])
    }
  }

  if (nrow(Output)==1) { # success
    return(tibble::tribble(~Clean, ~Removed,
                           Output$Address,
                           stringr::str_remove(Address, Output$Address)))

  } else {  # fail
    return(tibble::tribble(~Clean, ~Removed,
                           Address, ""))
  }

}

################################################################
#                        end of delete_danglers
################################################################

Testme <- function(){
  Archive_path <- "/home/ajackson/Dropbox/Rprojects/Curated_Data_Files/CoH_Address_Points/"
  dt <- readRDS(paste0(Archive_path, "COH_Geocoding_Locations_table.rds"))

}








