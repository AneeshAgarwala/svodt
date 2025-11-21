ctg <- read_excel("data-raw/CTG.xls", sheet = "Data", .name_repair = "universal")

ctg <- ctg |>
  dplyr::select(LB,
                AC = AC...11,
                FM = FM...12,
                UC = UC...13,
                DL = DL...14,
                DS = DS...15,
                DP = DP...16,
                ASTV:Tendency,
                CLASS,
                NSP) |>
  na.omit()

usethis::use_data(ctg, overwrite = TRUE)
