#' EM-DAT: The international disasters database
#'
#' EM-DAT provides an objective basis for vulnerability assessment and rational
#' decision-making in disaster situationsk
#'
#' Start.date start date of the disaster
#'
#' End.date end date of the disaster
#'
#' Country country of the disaster
#'
#' ISO ISO code of the country
#'
#' Location location of the disaster
#'
#' Magnitude.value magnitude of the disaster
#'
#' Magnitude.scale scale of the magnitude metric
#'
#' Disaster.type type of the disaster
#'
#' Disaster.subtype subtype of the disaster
#'
#' Associated.disaster disasters that caused or were caused by the
#' disaster
#'
#' Associated.disaster2 the second disaster that caused was caused by
#' the disaster
#'
#' Total.deaths total deaths resulting from disaster
#'
#' Total.affected total number of people affected by the disaster
#'
#' Total.damage...000.US.. total damages in US dollars
#'
#' insured_losses insured losses
#'
#' Disaster.name disaster name
#'
#' Disaster.No. a unique identifying number for the disaster
#'
#' geom contains POINT geomtries that indicate disaster locations
#'
#' @docType data
#' @format An sf dataframe with 611 rows and 18 columns
#' \describe{
#'   \item{Start.date}{Start date}
#'   \item{End.date}{End date}
#'   \item{Country}{Country}
#'   \item{ISO}{ISO country code}
#'   \item{Location}{Location}
#'   \item{Magnitude.value}{Magnitude value}
#'   \item{Disaster.type}{Disaster type}
#'   \item{Disaster.subtype}{Disaster subtype}
#'   \item{Associated.disaster}{Associated disaster}
#'   \item{Associated.disaster2}{2nd associated disaster}
#'   \item{Total.deaths}{Total deaths}
#'   \item{Total.affected}{Total affected}
#'   \item{Total.damage...000.US..}{Total damages, USD}
#'   \item{insured_losses}{Insured losses}
#'   \item{Disaster.name}{Disaster name}
#'   \item{Disaster.No.}{Disaster number}
#'   \item{geom}{Point geometries}
#' }
#' @source \url{http://www.emdat.be}
#' @author chandler.m.armstrong@erdc.dren.mil
#' @examples
#' \dontrun{
#' data(dfrEmDat)
#' summary(dfrEmDat)
#' }
"dfrEmDat"
