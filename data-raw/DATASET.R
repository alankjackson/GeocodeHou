## code to prepare `DATASET` dataset goes here

library(tidyverse)

#####    Rawish file

Archive_path <- "/home/ajackson/Dropbox/Rprojects/Curated_Data_Files/CoH_Address_Points/"
dt <- readRDS(paste0(Archive_path, "COH_Geocoding_Locations_table.rds"))

usethis::use_data(dt, overwrite = TRUE)

#####    Names only

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

#####     Standard USPS road types

Allowed_types <- c(
  "ALY", "ANX", "ARC", "AVE", "BYU", "BCH", "BND", "BLF", "BLFS", "BTM", "BLVD",
  "BR", "BRG", "BRK", "BRKS", "BG", "BGS", "BYP", "CP", "CYN", "CPE", "CSWY", "CTR",
  "CTRS", "CIR", "CIRS", "CLF", "CLFS", "CLB", "CMN", "CMNS", "COR", "CORS",
  "CRSE", "CT", "CTS", "CV", "CVS", "CRK", "CRES", "CRST", "XING", "XRD", "XRDS",
  "CURV", "DL", "DM", "DV", "DR", "DRS", "EST", "ESTS", "EXPY", "EXT", "EXTS", "FALL",
  "FLS", "FRY", "FLD", "FLDS", "FLT", "FLTS", "FRD", "FRDS", "FRST", "FRG", "FRGS",
  "FRK", "FRKS", "FT", "FWY", "GDN", "GDNS", "GTWY", "GLN", "GLNS", "GRN", "GRNS",
  "GRV", "GRVS", "HBR", "HBRS", "HVN", "HTS", "HWY", "HL", "HLS", "HOLW", "INLT",
  "IS", "ISS", "ISLE", "JCT", "JCTS", "KY", "KYS", "KNL", "KNLS", "LK", "LKS",
  "LAND", "LNDG", "LN", "LGT", "LGTS", "LF", "LCK", "LCKS", "LDG", "LOOP", "MALL",
  "MNR", "MNRS", "MDW", "MDWS", "MEWS", "ML", "MLS", "MSN", "MTWY", "MT", "MTN",
  "MTNS", "NCK", "ORCH", "OVAL", "OPAS", "PARK", "PKWY", "PASS", "PSGE", "PATH",
  "PIKE", "PNE", "PNES", "PL", "PLN", "PLNS", "PLZ", "PT", "PTS", "PRT", "PRTS",
  "PR", "RADL", "RAMP", "RNCH", "RPD", "RPDS", "RST", "RDG", "RDGS", "RIV", "RD",
  "RDS", "RTE", "ROW", "RUE", "RUN", "SHL", "SHLS", "SHR", "SHRS", "SKWY", "SPG",
  "SPGS", "SPUR", "SQ", "SQS", "STA", "STRA", "STRM", "ST", "STS", "SMT", "TER",
  "TRWY", "TRCE", "TRAK", "TRFY", "TRL", "TRLR", "TUNL", "TPKE", "UPAS", "UN",
  "UNS", "VLY", "VLYS", "VIA", "VW", "VWS", "VLG", "VLGS", "VL", "VIS", "WALK",
  "WALL", "WAY", "WAYS", "WL", "WLS", "SPWY")

usethis::use_data(Allowed_types, overwrite = TRUE)






