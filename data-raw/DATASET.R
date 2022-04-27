## code to prepare `DATASET` dataset goes here

Archive_path <- "/home/ajackson/Dropbox/Rprojects/Curated_Data_Files/CoH_Address_Points/"
dt <- readRDS(paste0(Archive_path, "COH_Geocoding_Locations_table.rds"))

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

usethis::use_data(df_names_only, overwrite = TRUE)
