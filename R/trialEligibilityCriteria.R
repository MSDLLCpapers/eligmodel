eligCritUI <- function(id) {
    ns <- NS(id)

    tagList(
      h2(HTML("<strong>Trial Eligibility Criteria</strong>")),
      uiOutput(ns("section_header")),
      hr(style = "border-top: 3px solid #044d48;"),
      bslib::navset_pill(
        bslib::nav_panel(title = "Summary of Patient Eligibility Status",
                  div(class = "network-container",
                      visNetwork::visNetworkOutput(ns("attr_tbl_elig_stat"), height = "800px", width = "100%")
                  )
        ),

        bslib::nav_panel(title = "Eligibility Status by Trial Criteria",
          div(id = "ec_avail_plot",
              plotOutput(ns("plot_elig_avail"), height = "650px")
          )
        )
      )
  )
}

eligCritServer <- function(input, output, session, report_object, sectionHeader, trialEligVals) {

  # -------------------------------------------- UI Elements ------------------------------------
  # text describing selected trial/real-world data set/cohort
  output$section_header <- renderUI({sectionHeader()
  })
  
  # attrition flow chart
  output$attr_tbl_elig_stat <- visNetwork::renderVisNetwork({
    trialEligVals()$attr_tbl_elig_stat
  })
  
  # bar plot summarizing eligibility status of selected eligibility criteria
  output$plot_elig_avail <- renderPlot({
    trialEligVals()$barplot_elig_stat
  })
}
