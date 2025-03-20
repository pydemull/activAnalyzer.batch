
#' Process all .agd files
#' 
#' The function analyses all the .agd files present in a given directory using 
#'     the functions from the activAnalyzer R package and using information 
#'     relating to the demographic characteristics of the participant(s), 
#'     the dates (start and end) of the physical activity measurement(s) (optional),
#'     and the processing settings.
#'
#' @param agd_dir A character string to set the path to the directory where are all
#'     the .agd files to be analysed. The names of the files shoud begin with the 
#'     participant number id (e.g., "001").
#' @param config_path A character string to set the path to the `config.csv` file to 
#'      be filled to allow the analysis process. Use the \code{\link{get_config_file}} 
#'      function to get the default `config.csv` file.  
#' @param demo_path A character string to set the path to the .csv file that should
#'     contain the demographic variables needed to compute physical activity metrics, 
#'     that are:
#'     \itemize{
#'       \item `id`: participant inclusion number.
#'       \item `age`: age of the participant.
#'       \item `sex`: sex of the participant. Males could be indicated by 
#'       `male`, and females by `female`.
#'       \item `weight`: weight of the participant in kg.
#'      }   
#'      The names of these variables can be differently written in your own file
#'      containing these variables and should be set in the `config.csv` file to 
#'      be filled to allow the analysis process. Use the \code{\link{get_demo_template}} 
#'      function to get a template of the file to be imported with the required demographic 
#'      characteristics.  
#' @param dates_path A character string to set the path to the .csv file that 
#'     contains the start and end dates of the physical activity measurement(s). 
#'     Use the \code{\link{get_dates_template}} function to get a template of the 
#'     file to be imported with the required dates if wanted. Default is `NULL`. 
#'     If `NULL`,  all the days of measurement will be analysed. If the path to an
#'     appropriate file is provided, the analyses will be conducted using 
#'     the dates provided in the file.
#' @param id_config A numeric value to indicate how many numbers are used to 
#'     identify the participant in the names of the .agd files (e.g., 2 for "01", 3 for "001", 
#'     4 for "0001", etc). The names of the files to be analysed have to 
#'     begin by the participant number written as in the mentionned examples above.
#' @param content A character value to indicate what results should be contained 
#'     in the list of the exported results:
#'     \itemize{
#'       \item `option_1` provides only the metrics summarised using valid days.
#'       \item `option_2` provides, in addition to `option_1`, the results computed 
#'       for each day of measurement.
#'       \item `option_3` provides, in addition to `option_2`, the initial dataset(s)
#'        with all the marks of the computations performed during the analysis process.
#'      }
#' @param out_dir A character string to set the path to the directory that will be used
#'    to export the results if required.
#' @param export_results A logical value regarding the export of the results. The 
#'     results are exported as a dataframe (if `content` argument = "option_1") 
#'     or as a list object otherwise, embedded in a .rds file. The results exported are
#'     the followings: 
#'     \itemize{
#'       \item `option_1`: the metrics summarized using the valid days only.
#'       \item `option_2`: the results from `option_1` plus the results computed for each
#'        day of measurement.
#'       \item `option_3`: the results from `option_2` plus the initial dataset(s) with all the 
#'       marks of the computations performed during the analysis process.
#'      }
#' @param cores A positive integer representing the number of cores to use for parallel 
#'     processing (i.e., the number of processes to start). This value must be between 2 and 
#'     parallel::detectCores() - 1. Default is parallel::detectCores() - 1.
#'
#' @return The data required by the `content` argument of the function: 
#'     \itemize{
#'       \item `all_metrics`: the metrics summarized using the valid days and collapsed
#'       into a single dataframe.
#'       \item `results_by_day`: the results computed for each day of measurement 
#'       collapsed into a single dataframe (`option_2` and `option_3` only).
#'       \item `data_with_intensity_marks`: the initial dataset(s) with all the 
#'       marks of the computations performed during the analysis process collapsed 
#'       into a single dataframe (`option_3` only).
#'      }
#'      If the `content` argument is `option_1`, a dataframe is returned. Otherwise 
#'      a list of dataframes is returned.
#'
#' @export
#' @examples
#' DIR_AGD_PATH <- system.file("agd/", package = "activAnalyzer.batch")
#' DEMO_PATH <- system.file("DEMO.csv", package = "activAnalyzer.batch")
#' DATES_PATH <- system.file("DATES.csv", package = "activAnalyzer.batch")
#' CONFIG_PATH <- system.file("config.csv", package = "activAnalyzer.batch")
#' process_all_agd(
#' agd_dir = DIR_AGD_PATH, 
#' demo_path = DEMO_PATH, 
#' config_path = CONFIG_PATH,
#' dates_path = DATES_PATH, 
#' id_config = 3,
#' cores = 2
#' )
process_all_agd <- function(
    agd_dir,
    config_path,
    demo_path, 
    dates_path = NULL,
    id_config,
    content = c("option_1", "option_2", "option_3"),
    out_dir = "./out",
    export_results = FALSE,
    cores = NULL
    ) {

# Get content argument information
content <- match.arg(content)
  
# Get demographic data
demo <- suppressMessages(readr::read_csv2(demo_path))
  
# Get analysis settings
config <- suppressMessages(readr::read_csv2(config_path))

# Get dates
dates <- if (!is.null(dates_path)) {suppressMessages(readr::read_csv2(dates_path))}
  
# Get the list of the files to analyse
list_agd <- file.path(agd_dir, list.files(agd_dir))

# Enable progress tracking
parabar::set_option("progress_track", TRUE)

# Get the number of cores
if (!is.null(cores)) {cores <- cores} else {cores <- parallel::detectCores() - 1}

# Start an asynchronous backend
backend <- parabar::start_backend(cores = cores, cluster_type = "psock", backend_type = "async")

# Process files
results <- 
  parabar::par_lapply(
    backend = backend, 
    x = list_agd, 
    fun = process_agd, 
    demo = demo, 
    config = config, 
    dates = dates,
    id_config = id_config,
    content = content
    )

# Stop the backend
parabar::stop_backend(backend)

# Extract the different kinds of results from the list to put them into separate dataframes
if (content == "option_1") {all_metrics <- results |> dplyr::bind_rows()}
if (content == "option_2") {
  all_metrics <- lapply(results, "[[", "all_metrics") |> dplyr::bind_rows()
  results_by_day <- lapply(results, "[[", "results_by_day") |> dplyr::bind_rows()
  p_log <- lapply(results, "[[", "p_log")
  }
if (content == "option_3") {
  all_metrics <- lapply(results, "[[", "all_metrics") |> dplyr::bind_rows()
  results_by_day <- lapply(results, "[[", "results_by_day") |> dplyr::bind_rows()
  p_log <- lapply(results, "[[", "p_log")
  data_with_intensity_marks <- lapply(results, "[[", "data_with_intensity_marks") |> dplyr::bind_rows()
}

# Make final list of results
if (content == "option_1") {
  list_results <- all_metrics
}

if (content == "option_2") {
  list_results <- 
    list(
      all_metrics = all_metrics, 
      results_by_day = results_by_day,
      p_log = p_log
    )
}

if (content == "option_3") {
  list_results <- 
    list(
      all_metrics = all_metrics, 
      results_by_day = results_by_day,
      p_log = p_log,
      data_with_intensity_marks = data_with_intensity_marks
    )
}

# Export results
if (isTRUE(export_results)) {
  
  if (!(dir.exists(out_dir))) {
    dir.create(out_dir)
    dir.create(file.path(out_dir, "gt3x"))
  }
  
  if (content == "option_1") {
    saveRDS(all_metrics, file = file.path(out_dir, "gt3x/all_metrics.rds"))
  }
  
  if (content == "option_2") {
    saveRDS(all_metrics, file = file.path(out_dir, "gt3x/all_metrics.rds"))
    saveRDS(results_by_day, file = file.path(out_dir, "gt3x/results_by_day.rds"))
  }
  
  if (content == "option_3") {
    saveRDS(all_metrics, file = file.path(out_dir, "gt3x/all_metrics.rds"))
    saveRDS(results_by_day, file = file.path(out_dir, "gt3x/results_by_day.rds"))
    saveRDS(data_with_intensity_marks, file = file.path(out_dir, "gt3x/data_with_intensity_marks.rds"))
  }
  
}

return(list_results)

}
