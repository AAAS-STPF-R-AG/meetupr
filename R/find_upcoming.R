#' Meetup search for upcoming events
#'
#' @param search Text to use to search events as a single character value
#' @param start_date Start date in the form
#' @param long Longitude of point to search near
#' @param lati Latitude of point to search near
#' @param .radius Number of miles of search radius around lat, long point to find events. Default is 10.
#' @param .page Maximum number of results to return. Default is 50.
#' @param ... Additional parameters for API call. See references.
#'
#' @references
#' \url{https://secure.meetup.com/meetup_api/console/?path=/find/upcoming_events}
#' @examples
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @export
find_upcoming <- function(search, start_date, long, lati, .radius = 10, .page = 50, ...){

  api_url <- "https://api.meetup.com/find/upcoming_events"

  parameters <- list(status = "upcoming", starte_date_range = start_date, text = search,
                     lon  = long, lat = lati, radius = .radius, page = .page, ...)

  # Only need API keys if OAuth is disabled...
  if (!getOption("meetupr.use_oauth")) {
    parameters <- append(parameters, list(key = get_api_key()))
  }

  req <- httr::GET(url = api_url,          # the endpoint
                   query = parameters,
                   config = meetup_token()
  )
  res <- content(req, "text")


  return(fromJSON(res)$events)
}
