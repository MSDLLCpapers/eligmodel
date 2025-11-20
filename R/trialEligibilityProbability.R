eligProbUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2(HTML("<strong>Probability of Satisfying All Trial Criteria")),
    uiOutput(ns("section_header")),
    hr(style = "border-top: 3px solid #044d48;"),
    br(),
    
    fluidRow( 
      column(4,
        div(
          class = "centered",
          uiOutput(ns("hist_binwidth_ui"))
        ),
        br(),
        bslib::card(
          max_height = 550,
          class = "sky-bkg",
          div(
            class = "vcentered",
            DT::DTOutput(ns("tbl_cohort_counts"))
          ),
          br(),
          br(),
          div(
            class = "centered",
            uiOutput(ns("unkn_cutoff_ui"))
          ),
          br(),
          div(
            class = "centered",
            downloadButton(ns("download_aug_cohort"), "Download augmented cohort")
          )
        )
      ), 
      column(8, 
        div(
            class = "centered",
             h4(HTML("<strong>Estimated Weights For Unknown Patient Cohort</strong>"))
          ),
        plotOutput(ns("plot_cohort_weights"), height = "600px"),
        div(
          class = "centered",
          p("*The weights for patients in the unknown group range from 0 (ineligible) to 1 (eligible). Higher weights (closer to 1) indicate a greater likelihood of the patient being eligible, contributing to a larger effective sample size and increased statistical power in subsequent analyses.")
        )
      )
    )
  )
}

eligProbServer <- function(input, output, session, report_object, sectionHeader, trialEligProbs, eligProbsBinBounds, eligProbBinWdith, eligProbCutoff){
  
  # -------------------------------------------- UI Elements ------------------------------------
  # slider for bin width of histogram
  output$hist_binwidth_ui <- renderUI({
    req(eligProbsBinBounds())
    ns <- session$ns
    sliderInput(ns("hist_binwidth"), 
                "Select histogram bin width:",
                min = 0.01,
                max = ceiling((eligProbsBinBounds()$max_binwidth - eligProbsBinBounds()$min_binwidth)*100)/100, 
                value = 0.05, 
                step = 0.01)
  })
  
  # slider for cutoff of unknown cohort
  output$unkn_cutoff_ui <- renderUI({
    ns <- session$ns

    sliderInput(ns("unkn_cutoff"), 
                "Select cut-off for the augmented cohort:",
                min = 0,
                max = 1, 
                value = 0.5, 
                step = 0.01)
    
  })  
  
  # text describing selected trial/real-world data set/cohort
  output$section_header <- renderUI({
    sectionHeader()
  })
  
  # table with counts for each eligibility cohort
  output$tbl_cohort_counts <- DT::renderDT({
    trialEligProbs()$tbl_cohort_counts
  })
  
  # histogram of probabilistic weights for unknown cohort
  output$plot_cohort_weights <- renderPlot({
    trialEligProbs()$plot_elig_prob
  })
  
  # download handler for download report
  output$download_aug_cohort <- downloadHandler(
    filename = function() {
      paste0("augmented_cohort_", input$sel_trial, "_", input$sel_rwd, "-", input$sel_cohort, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Ensure the objects are not NULL before proceeding
      if(is.null(sectionHeader()) || is.null(trialEligProbs())) {
        showModal(modalDialog(
          title = "Error",
          "Please update the report before trying to download the augmented cohort.",
          easyClose = TRUE
        ))
        return(NULL)  # Do not proceed if the plot or table is NULL
      }
      
      if (interactive()) {
        write.csv(trialEligProbs()$augmented_cohort, file, row.names = FALSE)
      }
    }
  )
  
  # --------------------------------------------------- Observe Events ----------------------------------------------------
  observeEvent(input$hist_binwidth, {
    req(input$hist_binwidth)

    # Update shared_values with new data from eligProb
    eligProbBinWdith(input$hist_binwidth)
  })
  
  observeEvent(input$unkn_cutoff, {
    req(input$unkn_cutoff)
    req(input$hist_binwidth)
 
    # Update shared_values with new data from eligProb
    eligProbCutoff(input$unkn_cutoff)
  })
  
}
