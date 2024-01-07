
#' Get the template to be configured to process the files with 
#' physical activity data
#' 
#' The function creates a `config.csv` file at the root of the current working directory. 
#' This file is needed to perform batch analysis. If there already is a file named `config.csv` 
#' at the root of the current working directory, the file will not be created.
#'
#' @return A message informing on the success or the failure of the creation of the `config.csv` file.
#' @export
#'
#' @examples
#'\dontrun{
#'get_config_file()
#'}
get_config_file <- function() {
  
  if (file.exists("./config.csv")) {
    
    cli::cli_bullets(c(
      "x" = "A {.file config.csv} file already exists at the root of your current working directory.",
      "*" = "Please check this file and remove it if you want to get the default template to be configured for the analysis process."
      )
    )

  } else {
    
    file.copy(
      from = system.file("config.csv", package = "activAnalyzer.batch"),
      to = "."
    )
    
    wd <- getwd()

    cli::cli_alert_success("The default template {.file config.csv} to be configured for the analysis process has been successfully placed in the following directory: {.file {wd}}.")
    
  }

  
}
