#' Retrieve R Cafe schedule

retrieve_schedule <- function(){
  googlesheets4::gs4_deauth()
  schedule_url <- "https://docs.google.com/spreadsheets/d/1TyC3bBiVvtWF4UtacC1Ccwa3L99Xig2jxtmDwIHXXNY"
  schedule <- googlesheets4::read_sheet(schedule_url)
  return(schedule)
}

#' Parse R Cafe Schedule
#'
#' Function to extract additional information from the basic schedule from Google
#' Sheets to be used for creating calendar events and website descriptions.
#' @param schedule data frame of R Cafe schedule as provided by `retrieve_schedule()`.
#' @return Returns the original data frame with additional columns.

parse_schedule <- function(schedule){
   dplyr::mutate(schedule,
      start_time = stringr::str_extract(time, "^(\\d\\d:\\d\\d)"),
      end_time = stringr::str_extract(time, "(\\d\\d:\\d\\d)$"),
      day = lubridate::day(date),
      month = lubridate::month(date),
      month_name = month.name[month],
      month_abbrev = month.abb[month],
      year = lubridate::year(date),
      wday = weekdays(date),
      dtstart = calendar::ic_char_datetime(lubridate::ymd_hm(paste(date, start_time), tz = "Europe/Amsterdam")),
      dtend <- calendar::ic_char_datetime(lubridate::ymd_hm(paste(date, end_time), tz = "Europe/Amsterdam"))
    )
}

create_description <- function(session){
  out <- paste0(
    "R Cafe @ TU Delft, ",
    session$day, " ",
    session$month_name, ", ",
    session$location,
    if(!is.na(session$presenter)) {
      paste0(". ", session$presenter, " will talk about ", session$theme, ".") # what if session with no presenter??
    } else {
      paste0(".")
    }
  )
  return(out)
}

create_ical <- function(session, path){
  ical_uid <- calendar::ic_guid()
  description <- create_description(session)
  summary <- paste("R Cafe", session$month_name)
  #dtstart <- calendar::ic_char_datetime(ymd_hm(paste(session$date, session$start_time), tz = "Europe/Amsterdam"))
  #dtend <- calendar::ic_char_datetime(ymd_hm(paste(session$date, session$end_time), tz = "Europe/Amsterdam"))
  #dtstart <- gsub("[-:]+", "", paste0(session$date, "T", session$start_time, "00"))
  #dtend <- gsub("[-:]+", "", paste0(session$date, "T", session$end_time, "00"))
  ical_output <- glue::glue_data(
    session,
    "
    BEGIN:VCALENDAR
    METHOD:PUBLISH
    BEGIN:VEVENT
    DTSTART:{dtstart}
    DTEND:{dtend}
    UID:{ical_uid}
    SUMMARY:{summary}
    LOCATION:{location}
    DESCRIPTION:{description}
    END:VEVENT
    END:VCALENDAR
    "
  )
  write(ical_output, path)
}


update_poster <- function() {
  poster <- magick::image_read('inst/poster_template.png')

  poster <- magick::image_annotate(poster, "SEPTEMBER MEET-UP", size = 65, color = "#00A6D6",
                           weight = 700, font = 'mono', location = "+1200+100") |>
    magick::image_annotate("Data jam: collaborate",
                   size = 45, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1250+200") |>
    magick::image_annotate("on real-world data from",
                   size = 45, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1250+250") |>
    magick::image_annotate("#tidytuesday",
                   size = 45, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1250+300")  |>

    magick::image_annotate("'Thursday 12 Sep 2024'",
                   size = 30, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1370+760")  |>

    magick::image_annotate("'15:00 - 16:30 CEST'",
                   size = 30, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1370+840")  |>

    magick::image_annotate("'Library, Albert Einstein'",
                   size = 30, color = "#FFF", weight = 600, font = 'mono',
                   location = "+1370+920")

  magick::image_write(poster, path = "poster.png", format = "png")

}
