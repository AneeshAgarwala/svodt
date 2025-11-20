#' Wine Chemical Composition Dataset (UCI Machine Learning Repository)
#'
#' This dataset contains the results of a chemical analysis of wines grown in the
#' same region in Italy but derived from three different cultivars. The goal is
#' typically to classify wine samples based on their chemical composition.
#'
#' @details
#' The dataset consists of 178 observations and 13 chemical properties measured
#' for each wine sample. The response variable \code{class} indicates the cultivar.
#'
#' @format
#' A data frame with 178 rows and 14 variables:
#' \describe{
#'   \item{class}{Target class indicating cultivar (1, 2, or 3).}
#'   \item{alcohol}{Alcohol content.}
#'   \item{malic_acid}{Malic acid concentration.}
#'   \item{ash}{Ash content.}
#'   \item{alcalinity_of_ash}{Alkalinity of ash.}
#'   \item{magnesium}{Magnesium content (integer).}
#'   \item{total_phenols}{Total phenolic content.}
#'   \item{flavanoids}{Flavonoid content.}
#'   \item{nonflavanoid_phenols}{Non-flavonoid phenolic content.}
#'   \item{proanthocyanins}{Proanthocyanin concentration.}
#'   \item{color_intensity}{Color intensity of the wine.}
#'   \item{hue}{Hue of the wine.}
#'   \item{od280_od315_of_diluted_wines}{OD280/OD315 ratio for diluted wines,
#'     an indicator of taste and quality.}
#'   \item{proline}{Proline concentration (integer).}
#' }
#'
#' @source
#' Aeberhard, S. & Forina, M. (1992). Wine [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C5PC7J.
#' #' Original dataset available at: <https://archive.ics.uci.edu/ml/datasets/wine>
"wine"
