colnames_lrs <- c(
  "LRS_name",
  "LRS_class",
  "ID_type",
  "Right_Ascension",
  "Declination",
  "Scale_Factor",
  "Blue_base_1",
  "Blue_base_2",
  "Red_base_1",
  "Red_base_2",
  # then the spectral intensity columns:
  paste0("blue_flux_", seq(1, 44)),
  paste0("red_flux_", seq(1, 49))
)

lrs <- read.csv("data-raw/lrs.csv", header = FALSE, sep = ",",
                  na.strings = c("?", "NA", ""), col.names = colnames_lrs)

usethis::use_data(lrs, overwrite = TRUE)
