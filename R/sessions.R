#' Extract information on upcoming sessions
#' @param n integer. Number of sessions to show. Default is to show all upcoming sessions. n = 1 will show the next session.
#' @export
upcoming_sessions <- function(n = NULL){

  current_date <- Sys.Date()
  sched <- retrieve_schedule()

  upcoming <- dplyr::filter(sched, date > current_date)
  if(!is.null(n)){
    upcoming <- dplyr::slice_head(upcoming, n = n)
  }
  return(upcoming)
}
