names <- paste0("Attribute", 1:34)

ionosphere <- read.csv("data-raw/ionosphere.data",
                       header = FALSE,
                       col.names = c(names, "Class")) |>
  mutate(Class = as.factor(Class))

usethis::use_data(ionosphere, overwrite = TRUE)


