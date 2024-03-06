
#' Process a single .agd file
#' 
#' The function analyses a .agd file using the functions from the activAnalyzer 
#'     R package and using information relating to the demographic characteristics 
#'     of the participant, the dates (start and end) of the physical activity 
#'     measurement (optional), and the processing settings.
#'
#' @param agd_file A character string to set the path to the .agd file. The name
#'     of the file shoud begin with the participant number id (e.g., "001").
#' @param config A dataframe obtained after importing the `config.csv` file needed 
#'     to allow the analysis process. Use the \code{\link{get_config_file}} function to
#'     get the default `config.csv` file.   
#' @param demo A dataframe containing the demographic variables needed to 
#'     compute physical activity metrics, that are:
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
#' @param dates A dataframe containing the start and end dates of the physical 
#'     activity measurement. Use the \code{\link{get_dates_template}} function to 
#'     get a template of the file to be imported with the required dates if wanted. 
#'     Default is `NULL`. If `NULL`,  all the days of measurement will be analysed. 
#'     If an appropriate dataframe is provided, the analyses will be conducted 
#'     using the dates provided in the dataframe.
#' @param id_config A numeric value to indicate how many numbers are used to 
#'     identify the participant in the name of the .agd file (e.g., 2 for "01", 
#'     3 for "001", 4 for "0001", etc). The name of the file to be analysed has to 
#'     begin by the participant number written as in the mentionned examples above.
#' @param content A character value to indicate what results should be contained 
#'     in the list of the exported results:
#'     \itemize{
#'       \item `option_1` provides only the metrics summarised using valid days.
#'       \item `option_2` provides, in addition to `option_1`, the results computed 
#'       for each day of measurement.
#'       \item `option_3` provides, in addition to `option_2`, the initial dataset
#'       with all the marks of the computations performed during the analysis process.
#'      }
#'     
#' @return The datasets required by the `content` argument of the function: 
#'     \itemize{
#'       \item `all_metrics`: the metrics summarized using the valid days.
#'       \item `results_by_day`: the results computed for each day of measurement 
#'       (`option_2` and `option_3` only).
#'       \item `data_with_intensity_marks`: the initial dataset with all the 
#'       marks of the computations performed during the analysis process (`option_3` 
#'       only).
#'      }
#'      If the `content` argument is `option_1`, a dataframe is returned. Otherwise 
#'      a list is returned.
#'      
#' @export
#'
#' @examples
#' FILE_PATH <- system.file("agd/001.agd", package = "activAnalyzer.batch")
#' CONFIG <- readr::read_csv2(system.file("config.csv", package = "activAnalyzer.batch"))
#' DEMO <- readr::read_csv2(system.file("DEMO.csv", package = "activAnalyzer.batch"))
#' DATES <- readr::read_csv2(system.file("DATES.csv", package = "activAnalyzer.batch"))
#' process_agd(
#' agd_file = FILE_PATH, 
#' config = CONFIG, 
#' demo = DEMO, 
#' dates = DATES, id_config = 3
#' )
process_agd <- function(
    agd_file, 
    config,
    demo, 
    dates = NULL, 
    id_config,
    content = c("option_1", "option_2", "option_3")
    ){
  
  # Get content argument information
  content <- match.arg(content)
  
  # Extract id from agd file name
  id <- as.numeric(substr(basename(agd_file), 1, id_config))
  
  # Link the names of the variables set by the user (cf. config.csv file) to the 
  # global names used in the code
  ID         <- config |> dplyr::filter(CODE_NAME == "ID") |> dplyr::pull(USER_VALUE)
  AGE        <- config |> dplyr::filter(CODE_NAME == "AGE") |> dplyr::pull(USER_VALUE)
  SEX        <- config |> dplyr::filter(CODE_NAME == "SEX") |> dplyr::pull(USER_VALUE)
  SEX_MALE   <- config |> dplyr::filter(CODE_NAME == "SEX_MALE") |> dplyr::pull(USER_VALUE)
  SEX_FEMALE <- config |> dplyr::filter(CODE_NAME == "SEX_FEMALE") |> dplyr::pull(USER_VALUE)
  WEIGHT     <- config |> dplyr::filter(CODE_NAME == "WEIGHT") |> dplyr::pull(USER_VALUE)

  # Reconfigure SEX variable to ensure sex levels names can be used by the subsequent functions
  demo <- 
    demo |> 
    dplyr::mutate({{ SEX }} := forcats::fct_recode(
      demo |> 
        dplyr::pull(SEX) |> 
        as.character(), "male" = SEX_MALE, "female" = SEX_FEMALE
       )
      )
  
  # Set parameters
  age    <- demo[demo[ID] == id, AGE][[1]]
  sex    <- as.character(demo[demo[ID] == id, SEX][[1]])
  weight <- demo[demo[ID] == id, WEIGHT][[1]]
  
  axis_preschooler <- config |> dplyr::filter(CODE_NAME == "AXIS_PRESCHOOLER") |> dplyr::pull(USER_VALUE)
  axis_child       <- config |> dplyr::filter(CODE_NAME == "AXIS_CHILD") |> dplyr::pull(USER_VALUE)
  axis_adolescent  <- config |> dplyr::filter(CODE_NAME == "AXIS_ADOLESCENT") |> dplyr::pull(USER_VALUE)
  axis_adult       <- config |> dplyr::filter(CODE_NAME == "AXIS_ADULT") |> dplyr::pull(USER_VALUE)
  axis_older       <- config |> dplyr::filter(CODE_NAME == "AXIS_OLDER") |> dplyr::pull(USER_VALUE)
  
  sed_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  mpa_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  vpa_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  equation_ee_preschooler <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_PRESCHOOLER") |> dplyr::pull(USER_VALUE)
  equation_ee_child       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_CHILD") |> dplyr::pull(USER_VALUE)
  equation_ee_adolescent  <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_ADOLESCENT") |> dplyr::pull(USER_VALUE)
  equation_ee_adult       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_ADULT") |> dplyr::pull(USER_VALUE)
  equation_ee_older       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_OLDER") |> dplyr::pull(USER_VALUE)
  
  epoch_target_preschooler <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_child       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_adolescent  <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_adult       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_older       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  frame_preschooler <- config |> dplyr::filter(CODE_NAME == "FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_child       <- config |> dplyr::filter(CODE_NAME == "FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_adolescent  <- config |> dplyr::filter(CODE_NAME == "FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_adult       <- config |> dplyr::filter(CODE_NAME == "FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_older       <- config |> dplyr::filter(CODE_NAME == "FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  allowanceFrame_preschooler <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_child       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_adolescent  <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_adult       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_older       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  streamFrame_preschooler <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_child       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_adolescent  <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_adult       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_older       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  valid_wear_time_start <- config |> dplyr::filter(CODE_NAME == "VALID_WEAR_TIME_START") |> dplyr::pull(USER_VALUE)
  valid_wear_time_end   <- config |> dplyr::filter(CODE_NAME == "VALID_WEAR_TIME_END") |> dplyr::pull(USER_VALUE)
  minimum_wear_time     <- config |> dplyr::filter(CODE_NAME == "MINIMUM_WEAR_TIME") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  # Set function parameters
  
      # Axis
     if (age < 6)                {axis <- axis_preschooler}
     if (age >= 6 && age < 13)   {axis <- axis_child}
     if (age >= 13 && age < 18)  {axis <- axis_adolescent}
     if (age >= 18 && age < 65)  {axis <- axis_adult}
     if (age >= 65)              {axis <- axis_older}
  
      # SED cut-point
     if (age < 6)                {sed_cutpoint <- sed_cutpoint_preschooler}
     if (age >= 6 && age < 13)   {sed_cutpoint <- sed_cutpoint_child}
     if (age >= 13 && age < 18)  {sed_cutpoint <- sed_cutpoint_adolescent}
     if (age >= 18 && age < 65)  {sed_cutpoint <- sed_cutpoint_adult}
     if (age >= 65)              {sed_cutpoint <- sed_cutpoint_older}

      # MPA cut-point
      if (age < 6)                {mpa_cutpoint <- mpa_cutpoint_preschooler}
      if (age >= 6 && age < 13)   {mpa_cutpoint <- mpa_cutpoint_child}
      if (age >= 13 && age < 18)  {mpa_cutpoint <- mpa_cutpoint_adolescent}
      if (age >= 18 && age < 65)  {mpa_cutpoint <- mpa_cutpoint_adult}
      if (age >= 65)              {mpa_cutpoint <- mpa_cutpoint_older}
      
      # VPA cut-point
      if (age < 6)                {vpa_cutpoint <- vpa_cutpoint_preschooler}
      if (age >= 6 && age < 13)   {vpa_cutpoint <- vpa_cutpoint_child}
      if (age >= 13 && age < 18)  {vpa_cutpoint <- vpa_cutpoint_adolescent}
      if (age >= 18 && age < 65)  {vpa_cutpoint <- vpa_cutpoint_adult}
      if (age >= 65)              {vpa_cutpoint <- vpa_cutpoint_older}
  
      # Equation for estimating energy expenditure
      if (age < 6)                {equation <- equation_ee_preschooler}
      if (age >= 6 && age < 13)   {equation <- equation_ee_child}
      if (age >= 13 && age < 18)  {equation <- equation_ee_adolescent}
      if (age >= 18 && age < 65)  {equation <- equation_ee_adult}
      if (age >= 65)              {equation <- equation_ee_older}

      # Epoch target
      if (age < 6)                {epoch_target <- epoch_target_preschooler}
      if (age >= 6 && age < 13)   {epoch_target <- epoch_target_child}
      if (age >= 13 && age < 18)  {epoch_target <- epoch_target_adolescent}
      if (age >= 18 && age < 65)  {epoch_target <- epoch_target_adult}
      if (age >= 65)              {epoch_target <- epoch_target_older}
  
     # Frame (wear time)
     if (age < 6)                {frame <- frame_preschooler}
     if (age >= 6 && age < 13)   {frame <- frame_child}
     if (age >= 13 && age < 18)  {frame <- frame_adolescent}
     if (age >= 18 && age < 65)  {frame <- frame_adult}
     if (age >= 65)              {frame <- frame_older}
  
      # Allowance Frame (wear time)
      if (age < 6)                {allowanceFrame <- allowanceFrame_preschooler}
      if (age >= 6 && age < 13)   {allowanceFrame <- allowanceFrame_child}
      if (age >= 13 && age < 18)  {allowanceFrame <- allowanceFrame_adolescent}
      if (age >= 18 && age < 65)  {allowanceFrame <- allowanceFrame_adult}
      if (age >= 65)              {allowanceFrame <- allowanceFrame_older}
  
      # Stream Frame (wear time)
      if (age < 6)                {streamFrame <- streamFrame_preschooler}
      if (age >= 6 && age < 13)   {streamFrame <- streamFrame_child}
      if (age >= 13 && age < 18)  {streamFrame <- streamFrame_adolescent}
      if (age >= 18 && age < 65)  {streamFrame <- streamFrame_adult}
      if (age >= 65)              {streamFrame <- streamFrame_older}
  
  
  # Prepare file
  data <- activAnalyzer::prepare_dataset(agd_file)
  
  if (!is.null(dates)) { 
    start_date <- dates[dates[ID] == id, "start_date"][[1]]
    end_date <- dates[dates[ID] == id, "end_date"][[1]]
    
    if(substr(start_date, 3, 3) == "/") {start_date <- as.Date(format(as.Date(start_date, format = '%d/%m/%Y'),'%Y-%m-%d'))}
    if(substr(start_date, 3, 3) == "-") {start_date <- as.Date(start_date, format = '%Y-%m-%d')}
    if(substr(end_date, 3, 3) == "/") {end_date <- as.Date(format(as.Date(end_date, format = '%d/%m/%Y'),'%Y-%m-%d'))}
    if(substr(end_date, 3, 3) == "-") {end_date <- as.Date(end_date, format = '%Y-%m-%d')}
    
    data <- 
      data |> 
      dplyr::mutate(TimeStamp_short = as.Date(substr(TimeStamp, 1, 10), format = '%Y-%m-%d')) |> 
      dplyr::filter(TimeStamp_short >= start_date & TimeStamp_short <= end_date) |> 
      dplyr::select(-TimeStamp_short)
    }
  
  # Add nonwear time and intensity marks to the dataset
  data_with_intensity_marks <-
    data |> 
    activAnalyzer::mark_wear_time(
      TS = "TimeStamp",
      to_epoch = epoch_target,
      cts = axis,
      frame = frame,
      allowanceFrame = allowanceFrame,
      streamFrame = streamFrame
    ) |> 
    activAnalyzer::mark_intensity(
      col_axis = axis,
      sed_cutpoint = sed_cutpoint,
      mpa_cutpoint = mpa_cutpoint,
      vpa_cutpoint = vpa_cutpoint,
      equation = equation,
      age = age,
      weight = weight,
      sex = sex
      )
  
  # Get results by day
  results_by_day <-
    activAnalyzer::recap_by_day(
      data = data_with_intensity_marks,
      col_axis = axis,
      valid_wear_time_start = valid_wear_time_start,
      valid_wear_time_end = valid_wear_time_end,
      age = age,
      weight = weight,
      sex = sex,
      start_first_bin = 0,
      start_last_bin = 10000,
      bin_width = 500
    )
  
  # Get mean results
  main_metrics <-
    results_by_day$df_all_metrics  |> 
    activAnalyzer::average_results(minimum_wear_time = minimum_wear_time, fun = "mean")
  
  # Compute accumulation metrics related to sedentary behaviour if the measurement
  # is valid
  valid_dates <- 
    results_by_day$df_all_metrics |> 
    dplyr::filter(wear_time >= minimum_wear_time * 60) |> 
    dplyr::pull(date)
  
  if(length(valid_dates) > 0) {
    accum_metrics_sed <- 
      activAnalyzer::compute_accumulation_metrics(
        data = data_with_intensity_marks, 
        behaviour = "sed",
        dates = valid_dates
      )$metrics
  }
  
  if(length(valid_dates) == 0) {
    accum_metrics_sed <- 
      data.frame(
        mean_breaks = NA,
        alpha = NA,
        MBD = NA,
        UBD = NA,
        gini = NA
      )
    
  }
  
  # Get final datasets 
  
  ## data_with_intensity_marks
  data_with_intensity_marks <- 
    data_with_intensity_marks |> 
    dplyr::mutate(id = id) |> 
    dplyr::select(id, dplyr::everything())
  
  ## results_by_day
  results_by_day <- 
    results_by_day$df_all_metrics |> 
    dplyr::mutate(id = id) |> 
    dplyr::select(id, dplyr::everything())
  
  ## all metrics
  all_metrics <- cbind(data.frame(id = id), main_metrics, accum_metrics_sed)
  
  # Create list of results
  if (content == "option_1") {
    list_results <- all_metrics
  }
  
  if (content == "option_2") {
    list_results <- 
      list(
        all_metrics = all_metrics, 
        results_by_day = results_by_day
      )
  }
  
  if (content == "option_3") {
    list_results <- 
      list(
        all_metrics = all_metrics, 
        results_by_day = results_by_day,
        data_with_intensity_marks = data_with_intensity_marks
      )
  }
  
  # Return final results
  return(list_results)

}

