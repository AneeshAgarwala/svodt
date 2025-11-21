australian_credit <- read.csv("data-raw/australian.dat", sep = " ",
                              header = FALSE,
                              col.names = c(paste0("A", 1:15))) |>
  mutate(
    #A1 = as.factor(A1),
    #A4 = as.factor(A4),
    #A5 = as.factor(A5),
    #A6 = as.factor(A6),
    #A8 = as.factor(A8),
    #A9 = as.factor(A9),
    #A11 = as.factor(A11),
    #A12 = as.factor(A12),
    A15 = as.factor(A15),
    )

usethis::use_data(australian_credit, overwrite = TRUE)

