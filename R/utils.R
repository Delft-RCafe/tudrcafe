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
      dtend = calendar::ic_char_datetime(lubridate::ymd_hm(paste(date, end_time), tz = "Europe/Amsterdam"))
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


update_poster <- function(session) {

  template_path <- system.file("poster_template.png", package = 'tudrcafe')

  poster <- magick::image_read(template_path)
  description <- session$poster_line |>
    stringr::str_wrap(21) |>
    stringr::str_split('\n') |>
    unlist()

  poster_date <- paste(session$wday, session$day, session$month_name)
  poster_time <- paste(session$start_time, "-", session$end_time)
  poster_location <-session$location
  poster_title <- paste0(toupper(session$month_name)," MEET-UP")
  outfile <- paste0("R_cafe_",session$month_abbrev, session$year,".png")


  poster <- magick::image_annotate(poster, poster_title, size = 65,
                                   color = "#00A6D6", weight = 700,
                                   font = 'mono', location = "+1200+100")

  for (i in 1:length(description)) {

    y_loc = 150 + 50*i

    loc_string = paste0("+1250+",y_loc)

    poster <- magick::image_annotate(poster, description[i],
                   size = 45, color = "#FFF", weight = 600, font = 'mono',
                   location = loc_string)
  }

    poster <- magick::image_annotate(poster, paste0("'", poster_date, "'"),
                                     size = 30, color = "#FFF", weight = 600,
                                     font = 'mono', location = "+1370+760") |>

      magick::image_annotate(paste0("'", poster_time, "'"),
                             size = 30, color = "#FFF", weight = 600, font = 'mono',
                             location = "+1370+840")  |>
      magick::image_annotate(paste0("'", poster_location, "'"),
                             size = 30, color = "#FFF", weight = 600, font = 'mono',
                             location = "+1370+920")

  magick::image_write(poster, path = outfile, format = "png")

}
