# GeocodeHou
Geocoding based on the City of Houston GIS address and location file

This package uses a lightly cleaned version of the Houston GIS file to
try to match addresses and produce lat long locations. 

The package assumes that the input GIS file is perfect, so any errors that
remain in that file will be propagated. I believe, too, that the lat longs in
that file generally represent either building or plat center locations, similar
to Google Maps. 

As a rule, the various routines will return a tibble with the Latitude, Longitude,
a success flag (T/F), and a fail code describing why the match failed, if indeed
it did.

Inputs are usually the separated components of the address, the street number, 
street direction (NSEW), the street name, street type (ST, RD, DR, ...), and
the Zip code. The very useful package 
[postmastr](https://slu-opengis.github.io/postmastr/ "postmastr")
by Christopher Prener is recommended for parsing address data.

An example of using this code on a large permit file from the city of Houston
may be found at <https://github.com/alankjackson/Curated_Data> in the file
Clean_City_Permit_Addresses.Rmd .

- - - -

## Example

I recommend running the routines in this order.

1. match_exactly()
2. repair_zipcode()
3. repair_name()
4. repair_type()
5. repair_number()
6. repair_prefix()

###  match_exactly

Match exactly looks for an exact match to the entire address. In my tests this
was successful about 90% of the time. It successively looks for failures in
the zipcode, street name, street type, street number, and finally the prefix.
If any one of these fails, the rest are not checked.

```R

Exact_match <- NULL
Failed_match <- NULL

for (i in 1:nrow(foo)){ # first look for exact matches
  tmp <- match_exactly(foo[i,]$pm.house, foo[i,]$pm.preDir, foo[i,]$pm.street,
                       foo[i,]$pm.streetSuf, foo[i,]$Zipcode)
  if (tmp$Success){ #   success
    Exact_match <- cbind(foo[i,], tmp) %>% 
      select(pm.id, pm.house, pm.preDir, pm.street, pm.streetSuf, Zipcode, Lat, Lon) %>% 
      rbind(., Exact_match)
  } else { #  Fail exact match
    Failed_match <- cbind(foo[i,], tmp) %>% 
      select(pm.id, pm.house, pm.preDir, pm.street, pm.streetSuf, Zipcode, Fail,
             Lat, Lon) %>% 
      rbind(., Failed_match)
  }
}

saveRDS(Exact_match, paste0(savepath, "Keep_Exactmatch.rds"))
saveRDS(Failed_match, paste0(savepath, "Keep_Failedmatch.rds"))

```

###  repair_zipcode

Here, I first look to see if the street name exists in the original zip code. If
so, then I bail - too many ways that could go bad. If the original zip code does
not contain the street name, then I look for an exact match for the address in any
zip code. If only one match occurs, then I call that good and return the successful
zip code. Note that in all of the repair routines, the proposed correction is 
returned separately so that it can be eye-balled.

```R
#   add a field to hold the corrected data if any
Failed_match <- Failed_match %>% mutate(Correction=NA)

for (i in 1:nrow(Failed_match)){
  target <- Failed_match[i,]
  tmp <- repair_zipcode(target$pm.house, target$pm.preDir, target$pm.street,
                       target$pm.streetSuf, target$Zipcode)
  if (tmp$Success){ #   success
    Failed_match[i,]$Lat <- tmp$Lat
    Failed_match[i,]$Lon <- tmp$Lon
    Failed_match[i,]$Fail <- paste(Failed_match[i,]$Fail, "Zipcode")
    Failed_match[i,]$Correction <- tmp$New_zipcode
  } 
}
```

###  repair_name

Here we will restrict our search to names in the given zip code, and look for
names that are close to the target name using the generalized Levenshtein 
(edit) distance. The default edit distance is 2, but this is a parameter
that can be played with. We will also ignore street names that are likely
to fail badly, like :

 - Names with fewer than 4 letters
 - Names containing STREET (A STREET, B STREET)
 - Number streets (4TH, 5TH, etc)
 - County Roads (CR 235)
 - Farm-to-Market Roads (FM 1960)

```R
for (i in 1:nrow(Failed_match)){
  if (Failed_match[i,]$Fail!="Street_name") {next} # skip if name isn't the issue
  if (Failed_match[i,]$Lat > 0) {next} # skip if zipcode resolved it
  target <- Failed_match[i,]
  tmp <- repair_name(target$pm.house, target$pm.preDir, target$pm.street,
                       target$pm.streetSuf, target$Zipcode)
  if (tmp$Success){ #   success
    Failed_match[i,]$Lat <- tmp$Lat
    Failed_match[i,]$Lon <- tmp$Lon
    Failed_match[i,]$Correction <- tmp$New_name
  } else {
    Failed_match[i,]$Fail <- paste("Street_name",tmp$Fail)
  }
}
```

###  repair_type

We will search in the provided zip code for streets that match the name, and 
see what the available types are. If we find only one, then we are successful 
and done. If there are more than one, we will look at the full address, and if
only one of those matches, we will again declare victory.

We will ignore types that are too likely to fail, "CT", and "CIR".

```R
for (i in 1:nrow(Failed_match)){
  if (Failed_match[i,]$Fail!="Street_type") {next} # skip if type isn't the issue
  target <- Failed_match[i,]
  tmp <- repair_type(target$pm.house, target$pm.preDir, target$pm.street,
                       target$pm.streetSuf, target$Zipcode)
  if (tmp$Success){ #   success
    Failed_match[i,]$Lat <- tmp$Lat
    Failed_match[i,]$Lon <- tmp$Lon
    Failed_match[i,]$Correction <- tmp$New_type
  } else {
    Failed_match[i,]$Fail <- paste("Street_type",tmp$Fail)
  }
}
```

###   repair_number

This basically amounts to a careful interpolation. First we gather all the numbers
for the given street prefix, name, type, and zip code. We toss any that are more
than about 2 blocks away. We honor the odd/even address convention and only 
look at points that should be on the same side of the street.

If we end up with 3 or more points available to use to interpolate, calculate
a (delta lat/long)/(delta house number) and use that to interpolate from
the house number nearest the target. As a final check. if that is greater than
250 feet, then reject the interpolation. That reject distance is selectable.

```R
for (i in 1:nrow(Failed_match)){
  if (Failed_match[i,]$Fail!="Street_num") {next} # skip if num isn't the issue
  target <- Failed_match[i,]
  tmp <- repair_number(target$pm.house, target$pm.preDir, target$pm.street,
                       target$pm.streetSuf, target$Zipcode)
  if (tmp$Success){ #   success
    Failed_match[i,]$Lat <- tmp$Lat
    Failed_match[i,]$Lon <- tmp$Lon
    Failed_match[i,]$Correction <- tmp$Result
  } else {
    Failed_match[i,]$Fail <- paste("Street_num",tmp$Fail)
  }
}
```

###    repair_prefix

Similar to repairing type, look for all the possible prefixes that match
the zip code, name, and type for the target. If there is only one, then we
are done. If there is not just one, then we fail.

```R
for (i in 1:nrow(Failed_match)){
  if (Failed_match[i,]$Fail!="Prefix") {next} # skip if name isn't the issue
  target <- Failed_match[i,]
  tmp <- repair_prefix(target$pm.house, target$pm.preDir, target$pm.street,
                       target$pm.streetSuf, target$Zipcode)
  if (tmp$Success){ #   success
    Failed_match[i,]$Lat <- tmp$Lat
    Failed_match[i,]$Lon <- tmp$Lon
    Failed_match[i,]$Correction <- tmp$New_prefix
  } else {
    Failed_match[i,]$Fail <- paste("Prefix",tmp$Fail)
  }
}
```



