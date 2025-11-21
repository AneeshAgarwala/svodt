dermatology <- read.csv("D:/SVMODT/project-svodt/data-raw/dermatology.data",
                        header=FALSE,
                        col.names = c(
                          # Clinical features
                          "erythema",
                          "scaling",
                          "definite_borders",
                          "itching",
                          "koebner_phenomenon",
                          "polygonal_papules",
                          "follicular_papules",
                          "oral_mucosal_involvement",
                          "knee_elbow_involvement",
                          "scalp_involvement",
                          "family_history",
                          # Histopathological features
                          "melanin_incontinence",
                          "eosinophils_infiltrate",
                          "pnl_infiltrate",
                          "papillary_dermis_fibrosis",
                          "exocytosis",
                          "acanthosis",
                          "hyperkeratosis",
                          "parakeratosis",
                          "rete_ridges_clubbing",
                          "rete_ridges_elongation",
                          "suprapapillary_thinning",
                          "spongiform_pustule",
                          "munro_microabscess",
                          "focal_hypergranulosis",
                          "granular_layer_disappearance",
                          "basal_layer_vacuolisation",
                          "spongiosis",
                          "saw_tooth_retes",
                          "follicular_horn_plug",
                          "perifollicular_parakeratosis",
                          "mononuclear_infiltrate",
                          "band_like_infiltrate",
                          "age",
                          # Target
                          "class"
                        ), na.strings = "?"
) |>
  na.


dermatology$class <- factor(dermatology$class,
                          levels = 1:6,
                          labels = c(
                            "psoriasis",
                            "seboreic_dermatitis",
                            "lichen_planus",
                            "pityriasis_rosea",
                            "cronic_dermatitis",
                            "pityriasis_rubra_pilaris"
                          ))


usethis::use_data(dermatology)
