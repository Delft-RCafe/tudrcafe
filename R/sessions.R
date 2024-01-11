#' Extract information on upcoming sessions
#' @param url string. Give the url for the R Cafe website.
#' @param session_no integer. The number of the upcoming session. 1 = first, 2 = second, etc.
#' @export
upcoming_session <- function(url = "https://delft-rcafe.github.io/home", session_no = 1){

  session_no_conv <- session_no + 1
  upcoming <- url |>
    rvest::read_html() |>
    rvest::html_element(paste0("div.callout:nth-child(", session_no_conv, ")")) |>
    rvest::html_text2()

  upcoming_tbl <- upcoming |>
    tibble::as_tibble() |>
    dplyr::mutate(
      topic = stringr::str_extract(value, ".*(?=\\n)"),
      date = stringr::str_extract(value, "(?<=Date: ).*"),
      time = stringr::str_extract(value, "(?<=Time: ).*"),
      location = stringr::str_extract(value, "(?<=Place: ).*")
    ) |>
    dplyr::select(!value)

  return(upcoming_tbl)
}
