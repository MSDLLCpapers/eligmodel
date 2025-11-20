baselineCharUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2(HTML("<strong>Patient Characteristics</strong>")),
    uiOutput(ns("section_header")),
    hr(style = "border-top: 3px solid #044d48;"),
    br(),
    bslib::navset_pill(
      bslib::nav_panel(title = "Breakdown of Patient Geography",
                fluidRow(
                  column(width = 9,
                         tags$div(
                           uiOutput(ns("plot_pat_geo_static"))
                         )
                  ),
                  column(
                    width = 3,
                    tags$div(
                      style = "display: flex; align-items: center; height: 70%;",
                      bslib::card(
                        class = "sky-bkg-v-center",
                        uiOutput(ns("tbl_pat_geo_static"))
                      )
                    )
                  )
                )
      ),
      
      bslib::nav_panel(title = "Summary of Patient Characteristics",
                br(),
                uiOutput(ns("tbl_bsl_char"))
      ),
      
      bslib::nav_panel(title = "Data Distribution by Patient Characteristic",
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
                    p('Please select a patient characteristic and at least one eligibility criterion. Then click the "Update Plots" button to visualize results.'),
                    fluidRow(column(6,
                                    uiOutput(ns("bsl_char_prim_sel"))
                                    ),
                             column(6,
                                    uiOutput(ns("bsl_char_sec_sel")))),
                    fluidRow(
                      column(9,
                             uiOutput(ns("ec_crit_sel_ui"))
                             ),
                      column(3,
                             br(),
                             div(
                               style = "text-align: center; justify-content: right", 
                               fluidRow(
                                 actionButton(ns("clear_ecs"), "Deselect All Criteria"),
                                 actionButton(ns("select_all_ecs"), "Select All Criteria")
                               )
                             )
                            )           
                    ),
                    div(
                      style = "text-align: center; align-items: center",  # Center the content
                      actionButton(ns("update_bsl_plots"), HTML("<strong>Update Plots</strong>"), class = "big-button")
                    )
                ),
                  fluidRow( 
                    column(4,
                      box(id = ns("elig_pat_box"),
                        solidHeader = TRUE,
                        width = NULL,
                        title = HTML("<strong>Eligible Patients</strong>"),
                        uiOutput(ns("bsl_char_elig_pat"))
                      )
                    ),
                    column(4,
                      box(id = ns("unkn_pat_box"),
                        solidHeader = TRUE,
                        width = NULL,
                        title = HTML("<strong>Unknown Patients</strong>"),
                        uiOutput(ns("bsl_char_unkn_pat"))
                      )
                    ),
                    column(4,
                      box(id = ns("inelig_pat_box"),
                        solidHeader = TRUE,
                        width = NULL,
                        title = HTML("<strong>Ineligible Patients</strong>"),
                        uiOutput(ns("bsl_char_inelig_pat"))
                      )
                    )
                  )
                
      )
    )
  )
}

baselineCharServer <- function(input, output, session, report_object, sectionHeader, patChar, bslCharChoices, selBslChar, selBslStrat, selBslPlot, eligVars, selBslCharECVar) {
  
  # -------------------------------------------- Reactive Vals ------------------------------------
  numVarBool <- reactiveVal(NULL)
  
  # -------------------------------------------- UI Elements ------------------------------------
  # text describing selected trial/real-world data set/cohort
  output$section_header <- renderUI({
    sectionHeader()
  })
  
  # summary table of patient characteristics
  output$tbl_bsl_char <- renderUI({
    HTML(patChar()$tbl_pat_char)
  })

  output$plot_pat_geo_static <- renderUI({
    if("geographic" %in% colnames(report_object$sel_df)){
      div(
        style = "width: 100%;",
        leafletOutput(
          outputId = session$ns("plot_pat_geo"),
          height = 600,  
          width = "100%"
        )
      )
    } else{
      div(
        style = "padding-top: 10%;",
        HTML("No geographic information available. To include geographic information, please review documentation for function EligModelShinyApp().")
      )
    }
  })

  output$plot_pat_geo <- renderLeaflet({
    patChar()$plot_pat_geo
    })
  
  output$tbl_pat_geo_static <- renderUI({
    if("geographic" %in% colnames(report_object$sel_df)){
      DT::renderDT({
        patChar()$tbl_pat_geo
      })
    } 
  })
  
  output$ec_crit_sel_ui <- renderUI({
    ns <- session$ns
    
    choices_vec <- report_object$sel_elig_crit
    names(choices_vec) <- report_object$elig_vars_dict$attr_name[report_object$elig_vars_dict$col_name %in% choices_vec]

    shinyWidgets::pickerInput(
      inputId = ns("sel_elig_crit_bsl"),
      label = "Select eligibility criteria for plotting", 
      choices = choices_vec,
      multiple = TRUE,
      selected = choices_vec)
  })
  
  # drop down of patient characteristics for plotting
  output$bsl_char_prim_sel <- renderUI({
    req(bslCharChoices())
    ns <- session$ns
    selectInput(
      inputId = ns("sel_prim_bsl_char"),
      label = "Select patient characteristic for plotting",
      choices = c("Please select an option" = "", bslCharChoices()),
      selected = NULL,
      multiple = FALSE
    )
  })

  # drop down of categorical variables to stratify continuous variable for plotting
  output$bsl_char_sec_sel <- renderUI({
    req(input$sel_prim_bsl_char)
    req(bslCharChoices())
    
    ns <- session$ns
    # call this so the renderUI will update when bslCharChoices (named vector of selected baseline characteristics) updates
    bslCharChoices()
    
    categ_vars <-  as.data.frame(copy(report_object$sel_df))[report_object$sel_bsl_char] %>%
      select(where(is.character))

    if(mode(report_object$sel_df[[input$sel_prim_bsl_char]]) == "numeric"){
      choices_vec <- c("None", colnames(categ_vars))
      choices_nms <- c("None", report_object$bsl_vars_dict$clean_name[match(colnames(categ_vars), report_object$bsl_vars_dict$col_name)])
      choices_vec <- setNames(choices_vec, choices_nms)
      
      selectInput(
        inputId = ns("sel_strat_bsl_char"),
        label = "Select stratification variable for plotting",
        choices = choices_vec,
        selected = choices_vec[1],
        multiple = FALSE
      )
    } else{
      choices_vec <- c("bar_plot", "pie_chart")
      choices_nms <- c("Bar Plot", "Pie Chart")
      choices_vec <- setNames(choices_vec, choices_nms)
      
      selectInput(
        inputId = ns("sel_plot_type_bsl_char"),
        label = "Select plot type",
        choices = choices_vec,
        selected = choices_vec[1],
        multiple = FALSE
      )
    }
  })

  # plot for eligible patient cohort
  output$bsl_char_elig_ec_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))
    
    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$ec_elig_pat_plot
        })
      }else{
        
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$ec_elig_pat_plot
        })
      }else{
        renderPlot({
          patChar()$ec_elig_pat_plot
        })
      }
    }
  })
  
  # plot for unknown patient cohort
  output$bsl_char_unkn_ec_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))
    
    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$ec_unkn_pat_plot
        })
      }else{
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$ec_unkn_pat_plot
        })
      }else{
        renderPlot({
          patChar()$ec_unkn_pat_plot
        })
      }      
    }
  })
  
  # plot for ineligible patient cohort
  output$bsl_char_inelig_ec_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))
    
    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$ec_inelig_pat_plot
        })
      }else{
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$ec_inelig_pat_plot
        })
      }else{
        renderPlot({
          patChar()$ec_inelig_pat_plot
        })
      }     
    }
    
  })
  
  # plot for eligible patient cohort
  output$bsl_char_elig_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))

    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$elig_pat_plot
        })
      }else{
        
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$elig_pat_plot
        })
      }else{
        renderPlot({
          patChar()$elig_pat_plot
        })
      }
    }
  })
  
  # plot for unknown patient cohort
  output$bsl_char_unkn_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))

    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$unkn_pat_plot
        })
      }else{
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$unkn_pat_plot
        })
      }else{
        renderPlot({
          patChar()$unkn_pat_plot
        })
      }      
    }
  })
  
  # plot for ineligible patient cohort
  output$bsl_char_inelig_pat <- renderUI({
    req(input$sel_prim_bsl_char != "")
    req(!is.null(numVarBool()))

    if(numVarBool() && !is.null(input$sel_strat_bsl_char) && input$sel_strat_bsl_char != "None"){
      # distinct categorical values for stratification variable
      strat_vals <- unique(report_object$sel_df[[input$sel_strat_bsl_char]])
      # if there are more than 8 strat_vals return an error
      if(length(strat_vals) <= 8){
        renderPlot({
          patChar()$inelig_pat_plot
        })
      }else{
        renderUI({HTML("The variable selected for stratification has too many strata (>8) to visualize. Please select another variable for stratification.")})
      }
    }else{
      if("plotly" %in% class(patChar()$elig_pat_plot)){
        plotly::renderPlotly({
          patChar()$inelig_pat_plot
        })
      }else{
        renderPlot({
          patChar()$inelig_pat_plot
        })
      }     
    }

  })
  
  # ------------------------------------- Observe Events ------------------------------------------
  observeEvent(input$clear_ecs, {
    choices_vec <- report_object$sel_elig_crit
    names(choices_vec) <- report_object$elig_vars_dict$attr_name[report_object$elig_vars_dict$col_name %in% choices_vec]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "sel_elig_crit_bsl",
      selected = character(0),
      choices = choices_vec
    )
  })
  
  observeEvent(input$select_all_ecs, {
    choices_vec <- report_object$sel_elig_crit
    names(choices_vec) <- report_object$elig_vars_dict$attr_name[report_object$elig_vars_dict$col_name %in% choices_vec]
    
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "sel_elig_crit_bsl",
      selected = choices_vec,
      choices = choices_vec
    )
  })
  
  observeEvent(input$sel_prim_bsl_char, {
    # Call function to reset report objects
    report_object$reset_pat_char()
    patChar(report_object$pat_char)
    
    numVarBool(mode(report_object$sel_df[[input$sel_prim_bsl_char]]) == "numeric")
  })
  
  observeEvent(list(input$sel_strat_bsl_char, input$sel_plot_type_bsl_char, input$sel_elig_crit_bsl), {
    # Call function to reset report objects
    report_object$reset_pat_char()
    patChar(report_object$pat_char)
  })
  
  # Update individual patient characteristic plots
  observeEvent(input$update_bsl_plots, {

    # Update shared_values, this will trigger OE in home module 
    selBslChar(input$sel_prim_bsl_char)
    selBslCharECVar(input$sel_elig_crit_bsl)

    if(mode(report_object$sel_df[[input$sel_prim_bsl_char]]) != "numeric"){
      selBslStrat(NULL)
      selBslPlot(input$sel_plot_type_bsl_char)
    } else{
      selBslStrat(input$sel_strat_bsl_char)
      selBslPlot(NULL)
    } 

  })

}
