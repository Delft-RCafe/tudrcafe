# retrieve schedule

retrieve_schedule <- function(){
  schedule_url <- "https://docs.google.com/spreadsheets/d/1TyC3bBiVvtWF4UtacC1Ccwa3L99Xig2jxtmDwIHXXNY"
  schedule <- googlesheets4::read_sheet(schedule_url)
  return(schedule)
}

# parse schedule

parse_schedule <- function(schedule){
   dplyr::mutate(schedule,
      start_time = stringr::str_extract(time, "^(\\d\\d:\\d\\d)"),
      end_time = stringr::str_extract(time, "(\\d\\d:\\d\\d)$"),
      day = lubridate::day(date),
      month = lubridate::month(date),
      month_name = month.name[month],
      month_abbrev = month.abb[month],
      year = lubridate::year(date),
      wday = weekdays(date)
    )
}

