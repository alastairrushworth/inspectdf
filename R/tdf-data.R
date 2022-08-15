#' Historical data about Tour de France riders, stages and winners.
#'
#' @docType data
#' @usage data(editions)
#'
#' @format A \code{dataframe} with 106 rows and 17 columns.
#'\describe{
#' \item{\code{edition}}{The edition of the Tour de France}
#' \item{\code{start_date}}{Start of the Tour de France edition}
#' \item{\code{winner_name}}{Name of the winner of the General Classification}
#' \item{\code{winner_team}}{Team name of the winning rider}
#' \item{\code{distance}}{Overall route distance (km)}
#' \item{\code{time_overall}}{Winning time in hours}
#' \item{\code{time_margin}}{Difference between winning time and runner up in hours}
#' \item{\code{stage_wins}}{Stages won by overall winner}
#' \item{\code{stages_led}}{Number of stages spent in the yellow jersey}
#' \item{\code{height}}{Rider height in meters}
#' \item{\code{weight}}{Rider weight in kilograms}
#' \item{\code{age}}{Rider age at \code{start_date}}
#' \item{\code{born}}{Date of borth of rider}
#' \item{\code{died}}{Date of death of rider}
#' \item{\code{full_name}}{Full name of rider (if different to \code{winner_name})}
#' \item{\code{nickname}}{Nickname of rider}
#' \item{\code{birth_town}}{Birth town or rider}
#' \item{\code{birth_country}}{Birth country of rider}
#' \item{\code{nationality}}{Nationality of rider}
#' \item{\code{stage_results}}{Each element is a list of dataframes containing stage results for each edition.}
#' }
#' @keywords datasets
#' @import lubridate
#' @details Winning times are those as at the conclusion of each race.  Several
#' winners were subsequently disqualified or excluded after the race finished
#' for doping or other rule violations, and a runner-up awarded the win.
#'
#' Data sourced from \href{https://en.wikipedia.org/}{Wikipedia}.  If you find
#' these data useful, please consider
#' \href{https://donate.wikimedia.org/}{making a donation to Wikimedia}.
#'
#' @examples
#' data(editions)
#' head(editions)
"tdf"