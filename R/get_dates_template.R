
#' Get a template of the file where to indicate the dates (start and end) of the physical activity 
#' measurement conducted in the participant(s)
#' 
#' This function creates a `DATES.csv` file in a `data` folder placed at the root
#'     of the current working directory. This file is a template to be filled so that it 
#'     contains, for the participant(s) to analyse, the start and end dates of the measurement.
#'
#' @return A message informing on the success or the failure of the creation of the `DATES.csv` file.
#' @export
#'
#' @examples
#'\dontrun{
#'get_dates_template()
#'}
get_dates_template <- function() {
  
  if (!(dir.exists(file.path(".", "data/")))) {
    dir.create("data")
  }
  
  if (file.exists("./data/DATES.csv")) {
    cli::cli_bullets(
      c("x" = "A {.file DATES.csv} file already exists in the {.file data/} folder of your current working directory.",
        "*" = "Please check this file and remove it if you want to get the default template to be filled for the analysis process.")
    )
    
  } else {
    data.frame(id = "001",
               start_date = "2024-01-01",
               end_date = "2024-12-31") |>
      utils::write.csv2("./data/DATES.csv", row.names = FALSE)
    
    cli::cli_alert_success(
      "The default template {.file DATES.csv} to be filled for the analysis process has been correctly created (see the {.file data/} folder)."
    )
    
  }
  
}


