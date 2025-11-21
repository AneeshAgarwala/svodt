australian_credit <- read.csv("data-raw/australian.dat", sep = " ",
                              header = FALSE,
                              col.names = c(paste0("A", 1:15)))

usethis::use_data(australian_credit, overwrite = TRUE)
