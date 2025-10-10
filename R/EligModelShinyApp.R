#' EligModelShinyApp: Estimate and visualize clinical eligibility probabilities in an RShiny application
#' 
#' This function launches an interactive Shiny application designed to assess the eligibility of real-world 
#' patients for selected clinical trials. It estimates the probability that a real-world patient satisfies key 
#' trial eligibility criteria and visualizes baseline characteristics and time-to-event outcomes based on 
#' eligibility status.  
#' The Shiny application performs the following tasks: 
#' \itemize{
#' \item Summarize patients’ eligibility status for each set of trial criteria, classifying patients as eligible, ineligible, or unknown 
#' \item Visualize baseline characteristics stratified by eligibility status 
#' \item Estimate the probability that patients with missing information satisfies all eligibility criteria, using a multivariate random forest model (from package randomForestSRC) 
#' \item Construct an “augmented” patient cohort, including patients who meet all eligibility criteria as well as those whose estimated probability of meeting all criteria exceeds a user-defined threshold 
#' \item (Optional) Visualize time-to-event outcomes stratified by eligibility status 
#' }
#' 
#' @param elig_dat_list A list of data.frames specifying the eligibility status of each trial criterion for all 
#' patients. This list contains a data.frame for each trial/real-world dataset combination included in the application.
#' In each data.frame, each column corresponds to a trial eligibility criterion, with numeric values indicating the 
#' patient’s status: 1 = eligible; 0 = ineligible; NA = eligibility unknown (missing information). 
#' Each data.frame in elig_dat_list must contain the following columns:
#' \itemize{
#' \item record_id: (type = character) – Patient identifier. record_id must be unique for each combination of trial, cohort, and rwd. 
#' \item trial: (type = character) – Name of the clinical trial of interest. Use "None" if not applicable. Specifies the trial from
#' which the eligibility criteria will be sourced. There should be only one trial value in each item in elig_dat_list 
#' \item cohort: (type = character) – Name of the patient cohort of interest. Use "None" if not applicable. Indicates the specific 
#' subgroup (e.g., treatment arm or trial subpopulation) whose eligibility criteria will be used 
#' \item rwd : (type = character) – Name of the real-world dataset associated with the patients. Use “None” if not applicable
#' There should be only one rwd value in each item in elig_dat_list 
#' \item Eligibility criteria columns:(type = numeric) 
#' }
#' @param char_dat_list A list of data.frames with patient characteristics. This list contains a data.frame for each trial/real-world 
#' dataset combination included in the application. In each data.frame, each characteristic is stored as either a numeric or character
#' column. In the Shiny app, character columns in char_dat_list are treated as categorical variables, while numeric
#' columns are treated as continuous variables. "diag_year” (representing diagnosis year) will be visualized as a binned categorical variable. 
#' Each data.frame in char_dat_list must contain the following columns: 
#' \itemize{
#' \item record_id: (type = character) – Patient identifier. record_id must be unique for each combination of trial, cohort, and rwd. 
#' \item trial: (type = character) – Name of the clinical trial of interest. Use "None" if not applicable. Specifies the trial from
#' which the eligibility criteria will be sourced. There should be only one trial value in each item in char_dat_list 
#' \item cohort: (type = character) – Name of the patient cohort of interest. Use "None" if not applicable. Indicates the specific 
#' subgroup (e.g., treatment arm or trial subpopulation) whose eligibility criteria will be used 
#' \item rwd : (type = character) – Name of the real-world dataset associated with the patients. Use “None” if not applicable
#' There should be only one rwd value in each item in char_dat_list 
#' \item geographic: (type = character) – Patient geographic information, default to be U.S. state abbreviations. If values are 
#' not provided as state abbreviations, they must be mapped to U.S. states in the data_dict_list. 
#' \item Other patient characteristics 
#' }
#' @param surv_dat_list An optional list of data frames containing survival data, with one data frame for each trial/real-world dataset combination
#' included in the application. Each data frame should include a numeric column for survival time in days and a factor column for event status 
#' with two levels: "1" for event and "0" for censoring. In the Shiny app, a Kaplan-Meier (KM) curve will be used to display the 
#' time-to-event outcome, with time converted to months. 
#' surv_dat_list must contain the following columns: 
#' \itemize{
#' \item record_id: (type = character) – Patient identifier. record_id must be unique for each combination of trial, cohort, and rwd. 
#' \item trial: (type = character) – Name of the clinical trial of interest. Use "None" if not applicable. Specifies the trial from
#' which the eligibility criteria will be sourced. There should be only one trial value in each item in surv_dat_list 
#' \item cohort: (type = character) – Name of the patient cohort of interest. Use "None" if not applicable. Indicates the specific 
#' subgroup (e.g., treatment arm or trial subpopulation) whose eligibility criteria will be used 
#' \item rwd : (type = character) – Name of the real-world dataset associated with the patients. Use “None” if not applicable
#' There should be only one rwd value in each item in surv_dat_list 
#' \item time:(type = numeric) Survival time in days 
#' \item status:(type = factor) A factor column for event status with two levels: "1" for event and "0" for censoring 
#' }
#' @param data_dict_list A list of data.frames containing display names for columns from elig_dat_list’s and char_dat_list’s items. 
#' This list contains a data.frame for each trial/real-world dataset combination included in the application. This is used to provide 
#' user-friendly labels for eligibility criteria and patient characteristics within the Shiny app. If geographic information in from an 
#' item char_dat_list is not provided as U.S. state abbreviations, the corresponding data.frame in data_dict_list should also include a 
#' mapping to U.S. states to support geographic visualizations.
#' data_dict_list must contain the following columns: 
#' \itemize{
#' \item trial: (type = character) – Name of the clinical trial of interest. Use "None" if not applicable. Specifies the trial from
#' which the eligibility criteria will be sourced.
#' \item rwd : (type = character) – Name of the real-world dataset associated with the patients. Use “None” if not applicable
#' \item var_type: (type = character) - Specifies the type of variable. Must be one of the following:
#'  1. "Eligibility Criteria" – for columns from items in elig_dat_list, 2. "Patient Characteristics" – for columns from items
#'  in char_dat_list, 3. "Geographic_Alt" – for unique values in the geographic column of items char_dat_list that are not U.S. 
#'  state abbreviations 
#'  \item col_name: (type = character) – Identifies the source column or value. For rows with var_type "Eligibility Criteria" or 
#'  "Patient Characteristics", this is the column name from items in elig_dat_list or char_dat_list, respectively. For rows with 
#'  var_type “Geographic_Alt”, this is the value from the geographic column of items in char_dat_list (e.g., an institution name) 
#'  \item clean_name: (type = character) – The display label shown in the Shiny app UI. For rows with var_type "Eligibility Criteria"
#'  or "Patient Characteristics", this is a user-defined variable name. For rows with var_type "Geographic_Alt", this represents the full 
#'  descriptive identifier corresponding to a value in the geographic column of items in char_dat_list. Each col_name should have a unique 
#'  clean_name in each data.frame in data_dict_list 
#'  \item miscellaneous: (type = character) – Used for labeling in the Shiny app’s patient attrition flow chart and geographic mapping. For
#'   rows with var_type "Eligibility Criteria", this is a rephrased version of clean_name used specifically in the app’s patient attrition 
#'   flow chart. It can be the same as clean_name. For rows with var_type “Geographic_Alt”, this is the U.S. state abbreviation that the 
#'   geographic value maps to. For rows with var_type “Patient Characteristic”, this should be left blank 
#' } 
#' 
#' @param color_elig  A character string specifying the color used to represent the patient cohort who satisfy all trial eligibility criteria
#' This color will be applied consistently across plots and visualizations throughout the RShiny application
#' Accepts any valid R color name (e.g., "blue", "red") or hex code (e.g., "#6b7ad1")
#' 
#' @param color_inelig A character string specifying the color used to represent the patient cohort who fail at least one trial eligibility criterion
#' This color will be applied consistently across plots and visualizations throughout the RShiny application
#' Accepts any valid R color name (e.g., "blue", "red") or hex code (e.g., "#99c418")
#' 
#' @param color_unkn A character string specifying the color used to represent the patient cohort who satisfy all observed trial eligibility criteria
#' but have missing value in at least one criterion
#' This color will be applied consistently across plots and visualizations throughout the RShiny application
#' Accepts any valid R color name (e.g., "blue", "red") or hex code (e.g., "#69B8F7")
#' 
#' @param color_aug A character string specifying the color used to represent the augmented patient cohort
#' This cohort includes patients who satisfy all trial eligibility criteria, as well as those in the unknown cohort 
#' whose estimated probability of meeting all criteria exceeds a user-defined threshold
#' This color will be applied consistently across plots and visualizations throughout the RShiny application
#' Accepts any valid R color name (e.g., "blue", "red") or hex code (e.g., "#0C2340")
#' 
#' @param surv_oucome An optional character string specifying the time-to-event outcome to be visualized in the Shiny app (e.g., "Overall Survival") 
#' This value will be used for labeling and display purposes within the application
#' If not specified and surv_data_list is provided, a default label of "Overall Survival" will be used
#' 
#' @section Example Input Tables:
#'
#' **Example from `elig_dat_list[[1]]`:**
#' \preformatted{
#' record_id  trial    rwd     cohort     IE_1  IE_2  IE_3  IE_4  IE_5
#'        1   Trial 1  Data 1  Cohort 2     1     1     1     1     NA
#'        2   Trial 1  Data 1  Cohort 2    NA     1     1     1      1
#'        3   Trial 1  Data 1  Cohort 2     1     1     1    NA      1
#' }
#'
#' **Example from `char_dat_list[[1]]`:**
#' \preformatted{
#' record_id  trial    rwd     cohort    race  ethnicity                sex      age_dx   diag_year  geographic
#'        1   Trial 1  Data 1  Cohort 2  White  Not Hispanic or Latino  Female     75       2006     CO
#'        2   Trial 1  Data 1  Cohort 2  White  Not Hispanic or Latino  Female     64       2008     GA
#'        3   Trial 1  Data 1  Cohort 2  Black  Hispanic or Latino      Male       52       1999     VT
#' }
#'
#' **Example from `surv_dat_list[[1]]`:**
#' \preformatted{
#' record_id  trial    rwd     cohort     time  status
#'        1   Trial 1  Data 1  Cohort 2   1521     1
#'        2   Trial 1  Data 1  Cohort 2    968     1
#'        3   Trial 1  Data 1  Cohort 2   3087     0
#' }
#'
#' **Example from `data_dict_list[[1]]`:**
#' \preformatted{
#' trial    rwd     var_type              col_name  clean_name               miscellaneous
#' Trial 1  Data 1  Eligibility Criteria  IE_1      Eligibility Criteria 1  Meets Eligibility Criteria 1
#' Trial 1  Data 1  Eligibility Criteria  IE_2      Eligibility Criteria 2  Meets Eligibility Criteria 2
#' Trial 1  Data 1  Eligibility Criteria  IE_3      Eligibility Criteria 3  Meets Eligibility Criteria 3
#' }
#' 
#' @returns Called to launch Shiny app. Returns `NULL` invisibly.
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @import leaflet
#' @import htmltools
#' @import rlang
#' @import shiny
#' @import shinydashboardPlus
#' @import survminer
#' @import visNetwork
#' @import ComplexUpset
#' @import leaflet
#' @importFrom data.table copy
#' @importFrom usmap us_map
#' @importFrom stringr str_wrap
#' 
#' @examples
#'
#' \donttest{
#' library(eligmodel)
#' 
#' # Generate example data #
#' data_list <- app_data_gen()
#' elig_dat_list <- data_list$elig_dat_list
#' char_dat_list <- data_list$char_dat_list
#' surv_dat_list <- data_list$surv_dat_list
#' data_dict_list <- data_list$data_dict_list
#' 
#' # Run application #
#' EligModelShinyApp(elig_dat_list, char_dat_list, data_dict_list, surv_dat_list)
#' }
#' 
EligModelShinyApp <- function(elig_dat_list, char_dat_list, data_dict_list,
                              surv_dat_list = NULL, color_elig = "#6b7ad1", color_inelig = "#99c418", color_unkn = "#69B8F7", color_aug = "#0C2340",
                              surv_outcome = "Overall Survival"){
  
  # Validation Checks -------------------------------------------------------------------------------------------------------------
  
  compile_dat_lists = function(char_dat_list, elig_dat_list, data_dict_list, surv_dat_list = NULL){
    lists = list("char_dat" = char_dat_list,
                 "elig_dat" = elig_dat_list,
                 "data_dict" = data_dict_list, 
                 "surv_dat" = surv_dat_list)
    
    error_messages = c()
    ret_list = list()
    
    for(list_nm in names(lists)){
      dat_list = lists[[list_nm]]
      
      if(!is.null(dat_list)){
        comp_list = compile_dat_list(list_nm, dat_list)
        
        if(comp_list$boolean){
          ret_list[[list_nm]] <- comp_list$value
        } else{
          error_mes = comp_list$value
          error_messages = c(error_messages, error_mes)
        }
      } else{
        ret_list[[list_nm]] <- NULL
      }
    }
    
    if(length(error_messages) > 0){
      stop(paste(error_messages, collapse = "\n"))
    }

  return(ret_list)
  }
  
  compile_dat_list = function(dat_list_name, dat_list){
    if(dat_list_name != "data_dict"){
      #check for record_id
      id_check <- all(unlist(lapply(dat_list, function(df){"record_id" %in% colnames(df)})))
      cohort_check <- all(unlist(lapply(dat_list, function(df){"cohort" %in% colnames(df)})))
    } else{
      id_check <- TRUE
      cohort_check <- TRUE
    }
    
    # check trial
    trial_check <- all(unlist(lapply(dat_list, function(df){"trial" %in% colnames(df)})))
    
    # check rwd
    rwd_check <- all(unlist(lapply(dat_list, function(df){"rwd" %in% colnames(df)})))
    
    
    check_vec <- list("record_id" = id_check,
                      "trial" =  trial_check,
                      "rwd" = rwd_check,
                      "cohort" = cohort_check)
    
    if(!all(unlist(check_vec))){
      missing_cols <- names(check_vec)[which(!unlist(check_vec))]
      error_mes <- paste0("The parameter: ", dat_list_name, 
                          " is missing the following columns from on or more list item: ",
                          paste0(missing_cols, collapse = ", "))
      ret_list <- list("boolean" = FALSE, "value" = error_mes)
      
    } else{
      ret_list <- list("boolean" = TRUE, "value" = bind_rows(dat_list))
    }
    
    return(ret_list)
  }
  
  if(is.null(surv_dat_list)){
    if (!(length(char_dat_list) == length(elig_dat_list) && length(elig_dat_list) == length(data_dict_list))) {
      stop("char_dat_list, elig_dat_list, data_dict_list must be the same length.")
    }
  } else{
    if (!(length(surv_dat_list) == length(char_dat_list) && length(char_dat_list) == length(elig_dat_list) && length(elig_dat_list) == length(data_dict_list))) {
      stop("surv_dat_list, char_dat_list, elig_dat_list, data_dict_list must be the same length.")
    }
  }
  
  input_lists <- compile_dat_lists(char_dat_list, elig_dat_list, data_dict_list, surv_dat_list)
  
  # Shiny App ----------------------------------------------------------------------------------------------------------------------
  
  header <- dashboardHeader(title = "")
  
  sidebar <- dashboardSidebar(
    shinydashboard::sidebarMenuOutput("sidebarTabs")
  )
  
  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "home", homeUI("homeModule")),
      shinydashboard::tabItem(tabName = "methods", methodsUI("methodsModule")),
      shinydashboard::tabItem(tabName = "trial_ec", eligCritUI("EligCritModule")),
      shinydashboard::tabItem(tabName = "patient_char", baselineCharUI("patCharModule")),
      shinydashboard::tabItem(tabName = "surv_out", survOutUI("survOutModule")),
      shinydashboard::tabItem(tabName = "elig_prob", eligProbUI("EligProbModule"))
    ) 
  )   

  ui <- fluidPage(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/style.css"
      )
    ),
    dashboardPage(header, sidebar, body)  
  )
  
  server <- function(input, output, session) {
    
    # Register the custom font 
    regular_font_path <- system.file("www", "SourceSansPro-Regular.otf", package = "eligmodel")
    bold_font_path <- system.file("www", "SourceSansPro-Bold.otf", package = "eligmodel")
    sysfonts::font_add("Source Sans Pro", regular = regular_font_path, bold = bold_font_path)
    
    # Enable showtext rendering
    showtext::showtext_auto()
    
    # instantiate report object
    report_object <- Report$new(input_lists$char_dat, input_lists$elig_dat, input_lists$data_dict, input_lists$surv_dat, color_elig, color_inelig, color_unkn, color_aug)
    
    # Home module
    home <- callModule(homeServer, "homeModule", report_object)
    
    # Trial Eligibility Probability module
    callModule(eligProbServer, "EligProbModule", report_object, home$sectionHeaderText, home$trialEligProbs, home$eligProbsBinBounds, home$eligProbBinWdith, home$eligProbCutoff)
    
    # methods Module
    callModule(methodsServer, "methodsModule", home$sectionHeaderText)
    
    # Trial Eligibility Criteria module
    callModule(eligCritServer, "EligCritModule", report_object, home$sectionHeaderText, home$trialEligVals)
    
    # Patient Characteristic module
    callModule(baselineCharServer, "patCharModule", report_object, home$sectionHeaderText, home$patCharVals, home$bslCharChoices, home$selBslChar, home$selBslStrat, home$selBslPlot, home$eligVars, home$selBslCharECVar)
    
    # Survival Outcome module
    survOutServer("survOutModule", report_object, home$sectionHeaderText, home$survOutcomes, surv_outcome)
    
    # Render sidebar based on OE status
    output$sidebarTabs <- shinydashboard::renderMenu({
      menu_items <- list(shinydashboard::menuItem("Home", tabName = "home", icon = icon("house")))
      
      if(home$reportRendered() && !is.null(report_object$surv_dat)) {
        menu_items <- append(menu_items, 
                             list(
                               shinydashboard::menuItem("Methods", tabName = "methods", icon = icon("calculator")),
                               shinydashboard::menuItem("Trial Eligibility Criteria", tabName = "trial_ec", icon = icon("list-check")),
                               shinydashboard::menuItem("Patient Characteristics", tabName = "patient_char", icon = icon("hospital-user")),
                               shinydashboard::menuItem("Eligibility Probability", tabName = "elig_prob", icon = icon("chart-simple")),
                               shinydashboard::menuItem("Survival Outcomes", tabName = "surv_out", icon = icon("chart-line"))
                             ))
      } else if(home$reportRendered() && is.null(report_object$surv_dat)) {
        menu_items <- append(menu_items, 
                             list(
                               shinydashboard::menuItem("Methods", tabName = "methods", icon = icon("calculator")),
                               shinydashboard::menuItem("Trial Eligibility Criteria", tabName = "trial_ec", icon = icon("list-check")),
                               shinydashboard::menuItem("Patient Characteristics", tabName = "patient_char", icon = icon("hospital-user")),
                               shinydashboard::menuItem("Eligibility Probability", tabName = "elig_prob", icon = icon("chart-simple"))
                             ))
      }
      
      shinydashboard::sidebarMenu(id = "tabs", .list = menu_items)
    })
  }

  shiny::addResourcePath("www", system.file("www", package = "eligmodel"))
  app <- shinyApp(ui, server)
  
  tryCatch({
    suppressWarnings(runApp(app))
  }, error = function(e) {
    # Log for debugging
    message("Error launching application: ", conditionMessage(e))
  })
}
