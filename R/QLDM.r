#' QLDM: A package for simulating forest landscape dynamics in Quebec province
#' 
#' The QLDM package provides a set of functions to simulate landscape-scale processes that 
#' influence the structure and composition of forest stands in a region, such as forest management 
#' (timber harvesting, salvagge logging, ...), wildfires and insect outbreaks.
#' 
#' @section Author(s):
#' \bold{Maintainer}: Núria Aquilué \email{nuria.aquilue@ctfc.cat}  
#' 
#' \bold{Authors}: Mathieu Bouchard and Núria Aquilué
#'
#' @docType package
#' @name QLDM
#' 
#' @importFrom tidyr %>%
#' @importFrom dplyr group_by summarise filter select mutate count left_join
#' @importFrom stringr str_length
#' @importFrom RANN nn2
#' @importFrom purrr rdunif
NULL
#> NULL