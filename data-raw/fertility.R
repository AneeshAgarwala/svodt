fertility <- read.csv("D:/SVMODT/project-svodt/data-raw/fertility_Diagnosis.txt",
                      header=FALSE,
                      col.names = c("season",
                                    "age",
                                    "child_diseases",
                                    "accident",
                                    "surgical_intervention",
                                    "high_fevers",
                                    "alcohol",
                                    "smoking",
                                    "hrs_sitting",
                                    "diagnosis"))

fertility$diagnosis <- as.factor(fertility$diagnosis)
usethis::use_data(fertility, overwrite = TRUE)
