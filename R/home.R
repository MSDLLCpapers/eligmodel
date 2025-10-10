# home module
homeUI <- function(id) {
    ns <- NS(id)

    tagList(
      shinyjs::useShinyjs(),
      h2(HTML("<strong>Mapping Key Clinical Trial Eligibility Criteria to Real-World Data</strong>")),
      hr(style = "border-top: 3px solid #044d48;"),
      br(),
      bslib::card(
        bslib::card_body(
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = "30%",
              # clinical trial selection drop down
              uiOutput(ns("sel_trial_ui")),
              # Real-world data selection drop down
              uiOutput(ns("sel_rwd_ui")),
              # cohort selection drop down
              uiOutput(ns("sel_cohort_ui")),
              open = "always"
            ),
            
            # Multiple Select Drop down for eligibility criteria
            uiOutput(ns("sel_elig_crit_pick")),
            # Multiple Select Drop down for baseline characteristics
            uiOutput(ns("sel_base_char_pick"))
          )
        )
      ),
      br(),
      box(
        title = HTML("<strong>Instructions</strong>"),
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        style = "
          background-color: #e8eeed; /* sky background */
          color: #044d48;
          padding-left: 20px; /* Add left padding */
          padding-top: 20px;
          padding-bottom: 40px;
          border-radius: 15px;  /* Make corners rounded */
          box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); ",
        width = 12,
        p('Click the “Update Report” button to update the figures and tables in the application with the above specifications.'),
        p('To download a report, first update the report. Then click the “Download Report” button to export the most recently updated report.'),
        p('To view the selections in the updated report, expand the "Report Selections" panel.'),
        br(),
        div(
          style = "text-align: center;",  # Center the content
          actionButton(ns("update_report"), "Update Report"),
          downloadButton(ns("download_report"), "Download Report")
        )
      ),
      box(
        id = ns("report_sel_box"),
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        title = HTML("<strong>Report Selections</strong>"),
        uiOutput(ns("section_header")),
        br(),
        bslib::layout_columns(
          bslib::card(
            class = "sky-bkg",
            h5(HTML("<strong>Selected Eligibility Criteria</strong>")),
            bslib::card_body(
              # table with eligibility criteria
              uiOutput(ns("tbl_sel_ec"))
            )
          ),
          
          bslib::card(
            class = "sky-bkg",
            h5(HTML("<strong>Selected Patient Characteristics</strong>")),
            bslib::card_body(
              # table with patient characteristics
              uiOutput(ns("tbl_sel_bsl_char"))
            )
          )
        )
      ),
      box(
        id = ns("set_fig_box"),
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        title = HTML("<strong>Combinations of Eligibility Criteria Available Across Patients</strong>"),
        plotOutput(ns("upset_plot"), height = "750px", width = '100%')
      )
    )
}

homeServer <- function(input, output, session, report_object) {

  # ---------------------------------------- Reactive Values ------------------------------------
      # dataframe to control rwd drop downs
      reportDFrwd <- reactiveVal(report_object$master_df)
      # dataframe to control cohort drop downs
      reportDFcohort <- reactiveVal(report_object$master_df)
      # named vector of eligibility criteria available to sel_elig_crit based on selected trial/real-world data set/cohort
      eligVars <- reactiveVal(NULL)
      # named vector of baseline characteristics available to sel_elig_crit based on selected trial/real-world data set/cohort
      bslVars <- reactiveVal(NULL)
      # Upset Plot
      upsetPlot <- reactiveVal(NULL)
      # text describing selected trial/real-world data set/cohort
      sectionHeaderText <- reactiveVal(NULL)  
      # reactive value for objects on the trial eligibility screen
      trialEligVals <- reactiveVal(NULL)
      # reactive value for objects on the eligibility probability screen
      trialEligProbs <- reactiveVal(NULL)
      eligProbsBinBounds <- reactiveVal(NULL)
      # reactive value for objects on the patient characteristics screen
      patCharVals <- reactiveVal(NULL)
      # table with eligibility criteria
      tblSelEligCrit <- reactiveVal(NULL) 
      # table with patient characteristics
      tblSelBSLChar <- reactiveVal(NULL) 
      # reactive val containing patient characteristics currently in updated report 
      # to be used in baseline characteristics screen as a method to force reactivity without having to monitor the update_report button on the patient characteristics screen
      bslCharChoices <- reactiveVal(NULL)
      # reactive val for slider on elig prob page that controls histogram bin width
      eligProbBinWdith <- reactiveVal(NULL)
      # reactive val for slider on elig prob page that controls cutoff for augmented cohort
      eligProbCutoff <- reactiveVal(NULL)
      # reactive val for drop down on patient characteristics page that controls selected variable for plotting
      selBslChar <- reactiveVal(NULL)
      # reactive val for drop down on patient characteristic page that controls stratification variable for plotting continuous pat char vars
      selBslStrat <- reactiveVal(NULL)
      # reactive val for drop down on patient characteristic page that controls plot type for plotting categorical pat char vars
      selBslPlot <- reactiveVal(NULL)
      selBslCharECVar <- reactiveVal(NULL)
      survOutcomes <- reactiveVal(NULL)
      reportRendered <- reactiveVal(FALSE)

      

  
  # -------------------------------------------- UI Elements ------------------------------------
      # clinical trial selection drop down
      output$sel_trial_ui <- renderUI({
        ns <- session$ns
        selectInput(ns("sel_trial"), "Select Clinical Trial", choices = unique(report_object$master_df$trial), selected = unique(report_object$master_df$trial)[1])
      })
      
      # Real-world data selection drop down
      output$sel_rwd_ui <- renderUI({
        ns <- session$ns
        selectInput(ns("sel_rwd"), "Select Real-World Data", choices = unique(reportDFrwd()$rwd), selected = unique(reportDFrwd()$rwd)[1])
      })
      
      # cohort selection drop down
      output$sel_cohort_ui <- renderUI({
        ns <- session$ns
        selectInput(ns("sel_cohort"), "Select Cohort", choices = sort(unique(reportDFcohort()$cohort)), selected = sort(unique(reportDFcohort()$cohort))[1])
      })
      
      # drop down for eligibility criteria
      output$sel_elig_crit_pick <- renderUI({
        ns <- session$ns
        shinyWidgets::pickerInput(
          inputId = ns("sel_elig_crit"),
          label = "Select Eligibility Criteria", 
          choices = eligVars(),
          multiple = TRUE,
          selected = eligVars()[c(1:2)],
          width = "800px")
      })
      
      # drop down for baseline characteristics
      output$sel_base_char_pick <- renderUI({
        ns <- session$ns
        shinyWidgets::pickerInput(
          inputId = ns("sel_bsl_char"),
          label = "Select Baseline Characteristics", 
          choices = bslVars(),
          multiple = TRUE,
          selected = bslVars()[c(1:2)],
          width = "800px")
      })
      
      # table with patient characteristics
      output$section_header <- renderUI({sectionHeaderText()})
      
      # table with eligibility criteria
      output$tbl_sel_ec <- renderUI({
        DT::renderDT({tblSelEligCrit()})
      })
      
      # table with patient characteristics
      output$tbl_sel_bsl_char <- renderUI({
        DT::renderDT({tblSelBSLChar()})
      })
      
      # upset plot
      output$upset_plot <- renderPlot({
          upsetPlot()
      })
      
      # download handler for download report
      output$download_report <- downloadHandler(
        filename = function() {
          paste0("report_", input$sel_trial, "_", input$sel_rwd, "-", input$sel_cohort, "-", Sys.Date(), ".html")
        },
        content = function(file) {
          
          # Ensure the objects are not NULL before proceeding
          if(is.null(sectionHeaderText()) || is.null(trialEligVals()) || is.null(tblSelEligCrit()) || is.null(tblSelBSLChar()) || is.null(trialEligProbs()) || is.null(patCharVals())) {
            showModal(modalDialog(
              title = "Error",
              "Please update the report before trying to download a report.",
              easyClose = TRUE
            ))
            return(NULL)  # Do not proceed if the plot or table is NULL
          }
          withProgress(message = 'Downloading Report', value = 0, {
            # Alert user if current selections don't match updated report
            checks <- report_object$verify_report_sel(input)
            if(!checks$bool) {
              showModal(modalDialog(
                title = "Error",
                checks$message,
                easyClose = TRUE
              ))
              return(NULL)  # Do not proceed if the plot or table is NULL
            }
            incProgress(0.1)
            
            # Temporary output file path for PDF
            report_dir <- tempdir()
            
            www_assets <- c("rmd_style.css", "rmd_header_includes.html", "SourceSansPro-Regular.otf", "SourceSansPro-Bold.otf")
            ext_assets <- c("reportTemplate.Rmd", "methods1.png", "methods2.png")
            for (asset in www_assets) {
              src <- system.file("www", asset, package = "eligmodel")
              dst <- file.path(report_dir, asset)
              if (src == "") stop(paste("Missing asset in www/:", asset))
              ok <- file.copy(from = src, to = dst, overwrite = TRUE)
              if (!ok) stop(paste("Failed to copy", asset, "to", dst))
            }

            for (asset in ext_assets) {
              src <- system.file("extdata", asset, package = "eligmodel")
              dst <- file.path(report_dir, asset)
              if (src == "") stop(paste("Missing asset in extdata/:", asset))
              ok <- file.copy(from = src, to = dst, overwrite = TRUE)
              if (!ok) stop(paste("Failed to copy", asset, "to", dst))
            }

            incProgress(0.2)
            
            # Render the R Markdown file with dynamic title and data
            rmarkdown::render(
              input = file.path(report_dir, "reportTemplate.Rmd"),
              output_dir = report_dir,
              output_file = "report.html",
              params = list(
                sel_trial = input$sel_trial,
                sel_rwd = input$sel_rwd,
                sel_cohort = input$sel_cohort,
                upset_plot = report_object$upset_plot,
                tbl_sel_ec = report_object$tbl_elig_crit,
                tbl_sel_bsl_char = report_object$tbl_bsl_char,
                attr_tbl_ec = report_object$trial_elig_crit$attr_tbl_elig_stat,
                barplot_ec = report_object$trial_elig_crit$barplot_elig_stat,
                tbl_bsl_char = report_object$pat_char$tbl_pat_char,
                tbl_pat_geo = report_object$pat_char$tbl_pat_geo_report,
                plot_pat_geo = report_object$pat_char$plot_pat_geo_report,
                tbl_cohort_counts = report_object$elig_prob$tbl_cohort_counts,
                plot_elig_prob = report_object$elig_prob$plot_elig_prob,
                km_plot_tbl = report_object$surv_outcomes$km_plot_tbl,
                surv_median_tbl = report_object$surv_outcomes$median_tbl,
                show_surv = !is.null(report_object$surv_dat)
              ),
              envir = new.env()
            )
            incProgress(0.7)
            
            if (interactive()) {
              file.copy(file.path(report_dir, "report.html"), file)
            }
            incProgress(0.05)
          }
          )
        }
      )
      
  # ------------------------------------- Observe Events ------------------------------------------
      # Update data for UI drop downs based on selected trial
      observeEvent(input$sel_trial, {
        ret <- report_object$master_df
        ret <- ret[ret$trial == input$sel_trial,]
        reportDFrwd(ret)
        }
      )
      
      # Update data for UI drop downs based on selected trial and real-world data set
      observeEvent(input$sel_rwd, {
        ret <- report_object$master_df
        ret <- ret[ret$trial == input$sel_trial & ret$rwd == input$sel_rwd,]
        reportDFcohort(ret)
        }
      )

      # Update variables available for eligibility criteria and baseline char drop downs based on selected trial, real-world dataset and cohort
      observeEvent(list(input$sel_trial, input$sel_rwd, input$sel_cohort),{
        req(input$sel_trial)
        req(input$sel_rwd)
        req(input$sel_cohort)
        
        subset_elig_dat <- as.data.frame(copy(report_object$elig_dat[report_object$elig_dat$trial == input$sel_trial &
                                                                       report_object$elig_dat$rwd == input$sel_rwd &
                                                                       report_object$elig_dat$cohort == input$sel_cohort,])) %>% 
                        select(-record_id, -trial, -rwd, -cohort)
        
        # remove columns that only have NA values - columns that are not applicable to this trial/cohort/rwd
        subset_elig_dat_clean <- subset_elig_dat[, colSums(is.na(subset_elig_dat)) < nrow(subset_elig_dat)]
        
        elig_vars <- names(subset_elig_dat_clean)
        elig_var_nms <- as.data.frame(report_object$data_dict)[report_object$data_dict$var_type == "Eligibility Criteria",]$clean_name[match(elig_vars, report_object$data_dict[report_object$data_dict$var_type == "Eligibility Criteria",]$col_name)]
        elig_vars <- setNames(elig_vars, elig_var_nms)
        
        subset_bsl_df <- as.data.frame(copy(report_object$char_dat[report_object$char_dat$trial == input$sel_trial &
                                                                     report_object$char_dat$rwd == input$sel_rwd &
                                                                     report_object$char_dat$cohort == input$sel_cohort,]))  %>% 
                        select(-record_id, -trial, -rwd, -cohort)
        
        # remove columns that only have NA values - columns that are not applicable to this trial/cohort/rwd
        subset_bsl_df_clean <- subset_bsl_df[, colSums(is.na(subset_bsl_df)) < nrow(subset_bsl_df)]
        
        bsl_vars <- names(subset_bsl_df_clean)
        bsl_var_nms <- as.data.frame(report_object$data_dict)[report_object$data_dict$var_type == "Patient Characteristic",]$clean_name[match(bsl_vars, report_object$data_dict[report_object$data_dict$var_type == "Patient Characteristic",]$col_name)]
        bsl_vars <- setNames(bsl_vars, bsl_var_nms)

        eligVars(elig_vars)
        bslVars(bsl_vars)
        }
      )
      
      # Update histogram on trial eligibility probability screen based on changes in the bin width slider
      observeEvent(list(eligProbBinWdith(), eligProbCutoff()),
                   ignoreInit = TRUE, {
        if(is.null(eligProbBinWdith()) | is.null(eligProbCutoff())) {                  
          # Prevent further execution of the observeEvent
          return(NULL)
        }

        suppressWarnings(suppressMessages(report_object$update_elig_prob(eligProbBinWdith(), eligProbCutoff())))
        # update plot of trial probability screen
        trialEligProbs(report_object$elig_prob)
        
        if(!is.null(report_object$surv_dat)){
          suppressWarnings(suppressMessages(report_object$update_survival_outcomes(eligProbCutoff())))
          survOutcomes(report_object$surv_outcomes)
        }
      })
      
      # Update individual patient characteristic plots on Patient Characteristics screen based on changes in drop down Patient Characteristics screen
      observeEvent(list(selBslChar(), selBslStrat(), selBslPlot(), selBslCharECVar()), 
                   ignoreInit = TRUE, {
                     req(selBslCharECVar())
                     # if there is no selected baseline characteristic
                     if(is.null(selBslChar()) || selBslChar() == "") {                  
                       # Prevent further execution of the observeEvent
                       return(NULL)
                     }
                    
                     # update plots on patient characteristic screen
                     suppressWarnings(suppressMessages(report_object$update_pat_char(selBslChar(), selBslStrat(), selBslPlot(), selBslCharECVar())))
                     
                     patCharVals(report_object$pat_char)
                   })

      # Update report object
      observeEvent(input$update_report, 
                   ignoreInit = TRUE, {
        if(is.null(input$sel_elig_crit) || length(input$sel_elig_crit) == 0||
           is.null(input$sel_bsl_char) || length(input$sel_bsl_char) == 0) {
          # Show modal if selections are empty
          showModal(modalDialog(
            title = "Missing Selections",
            "No eligibility criteria and/or baseline characteristics have been selected to use in the analysis. Please select eligibility criteria and baseline characteristics to use in the analysis before generating a report'.",
            easyClose = TRUE,
            footer = NULL
          ))
          # Prevent further execution of the observeEvent
          return()
        }
        
        # Wrap only the risky update_report call
        report_success <- tryCatch({
          suppressWarnings(suppressMessages(
            report_object$update_report(input, eligProbBinWdith(), eligProbCutoff())
          ))
          TRUE
        }, error = function(e) {
          # Log for debugging
          message("Error in update_report(): ", conditionMessage(e))
          
          showModal(modalDialog(
            title = "Failure to Update Report",
            "The report could not be updated with the current selections. Please relaunch the app after verifying the input data.",
            easyClose = FALSE,
            footer = NULL
          ))
      
          return(FALSE)
        })
      
        # Stop observer if update failed
        if (!isTRUE(report_success)) return()
        
        # When the report is updated, update baseline char selections
        bsl_char_choices <- report_object$sel_bsl_char
        bsl_char_choices <- bsl_char_choices[!bsl_char_choices %in% c("geographic")]
        bsl_char_choice_nms <- report_object$bsl_vars_dict$clean_name[match(bsl_char_choices, report_object$bsl_vars_dict$col_name)]
        bsl_char_choices <- setNames(bsl_char_choices, bsl_char_choice_nms)
        
        bslCharChoices(bsl_char_choices)
        
        # Update section header reactive value
        sectionHeaderText(report_object$section_header)
        
        # Update trial eligibility screen reactive values
        trialEligVals(report_object$trial_elig_crit)
        
        # update upset plot
        upsetPlot(report_object$upset_plot)
        
        # update table of selected eligibility criteria
        tblSelEligCrit(report_object$tbl_elig_crit)
        
        # update table of selected baseline characteristics
        tblSelBSLChar(report_object$tbl_bsl_char)
        
        # update trial probability screen
        trialEligProbs(report_object$elig_prob)
        eligProbsBinBounds(report_object$elig_prob_bin_bounds)
        
        # update survival outcome screen
        survOutcomes(report_object$surv_outcomes)
        
        # update patient characteristic screen
        patCharVals(report_object$pat_char)
        
        reportRendered(TRUE)
        
        showModal(modalDialog(
          title = "Update Complete",
          "The report has been updated throughout the application.",
          easyClose = TRUE
        ))
        
        if(as.logical(input$set_fig_box$collapsed)){
          shinydashboardPlus::updateBox("set_fig_box", action = "toggle")
        }
    
      })
      
      return(
            list(
              sectionHeaderText = sectionHeaderText,
              trialEligVals = trialEligVals,
              trialEligProbs = trialEligProbs,
              survOutcomes = survOutcomes,
              eligProbsBinBounds = eligProbsBinBounds,
              patCharVals = patCharVals,
              bslCharChoices = bslCharChoices,
              eligProbBinWdith = eligProbBinWdith,
              eligProbCutoff = eligProbCutoff,
              selBslChar = selBslChar,
              selBslStrat = selBslStrat,
              selBslPlot = selBslPlot,
              eligVars = eligVars,
              selBslCharECVar = selBslCharECVar,
              reportRendered = reportRendered
              )
        )
}
