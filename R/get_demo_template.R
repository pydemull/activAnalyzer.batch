
#' Get a template of the file where to indicate the demographic characteristics 
#'     of the participant(s) to be analysed
#' 
#' This function creates a `DEMO.csv` file in a `data` folder placed at the root
#'     of  the current working directory. This file is a template to be filled so that 
#'     it contains, for the participant(s) to analyse, the following variables: id number,
#'     sex, age (yr), and weight (kg).
#'
#' @return A message informing on the success or the failure of the creation of the `DEMO.csv` file.
#' @export
#'
#' @examples
#'\dontrun{
#'get_demo_template()
#'}
get_demo_template <- function() {
  
  if (!(dir.exists(file.path(".", "data/")))) {
    dir.create("data")
  }
  
  if (file.exists("./data/DEMO.csv")) {
    cli::cli_bullets(
      c("x" = "A {.file DEMO.csv} file already exists in the {.file data/} folder of your current working directory.",
        "*" = "Please check this file and remove it if you want to get the default template to be filled for the analysis process.")
    )
    
  } else {
    
    data.frame(id = "001", sex = "male", age = 18, weight = 70) |> 
      utils::write.csv2("./data/DEMO.csv", row.names = FALSE)
    
    cli::cli_alert_success(
      "The default template {.file DEMO.csv} to be filled for the analysis process has been correctly created (see the {.file data/} folder)."
    )
    
  }
  
}  
  

