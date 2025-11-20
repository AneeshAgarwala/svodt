#' Heart Attack Survival Dataset
#'
#' This dataset contains clinical and diagnostic measurements for patients
#' who experienced a heart attack. It is commonly used for survival prediction
#' and binary classification tasks. The variables include survival time,
#' cardiac function measurements, and indicators of disease severity.
#'
#' @details
#' The original dataset included patient names and a grouping variable;
#' these have been removed in this cleaned version. The dataset includes
#' derived variables such as \code{alive_at_1}, based on survival outcomes.
#'
#' @format
#' A data frame with the following variables:
#' \describe{
#'   \item{still_alive}{Binary indicator: \code{1 = alive} at end of survival period,
#'     \code{0 = deceased}.}
#'
#'   \item{age_at_heart_attack}{Age (in years) at the time the heart attack occurred.}
#'
#'   \item{pericardial_effusion}{Binary indicator of pericardial effusion
#'     (fluid around the heart): \code{1 = present}, \code{0 = absent}.}
#'
#'   \item{fractional_shortening}{Measure of myocardial contractility
#'     (higher values generally indicate better function).}
#'
#'   \item{epss}{E-point septal separation; another measure of left ventricular
#'     contractility. Higher values often indicate worse function.}
#'
#'   \item{lvdd}{Left ventricular end-diastolic dimension, representing the size
#'     of the left ventricle at the end of the filling phase.}
#'
#'   \item{wall_motion_score}{Score summarizing abnormal movement across
#'     heart wall segments.}
#'
#'   \item{wall_motion_index}{Wall motion score divided by number of segments
#'     examined; a normalized measure of motion abnormality.}
#'
#'   \item{mult}{Derived variable included in the original dataset;
#'     not clinically meaningful but retained for completeness.}
#' }
#'
#' @source
#' Echocardiogram [Dataset]. (1988). UCI Machine Learning Repository.
#' Original dataset available at: <https://archive.ics.uci.edu/dataset/38/echocardiogram.>
"echocardiogram"
