Report <- R6::R6Class("Report",
                    public = list(
                      
                      trial = NULL, # selected trial
                      cohort = NULL, # selected cohort
                      dataset = NULL, # selected data set
                      section_header = NULL, # header text for module pages
                      sel_elig_crit = NULL, # vector of selected eligibility criteria
                      sel_bsl_char = NULL, # vector of selected baseline characteristics
                      tbl_elig_crit = NULL, # table of eligibility criteria selections
                      tbl_bsl_char = NULL, # table of baseline characteristic selections
                      trial_elig_crit = NULL, # figures/tables for Trial Eligibility Criteria Screen
                      pat_char = NULL, # figures/tables for Patient Characteristics Screen
                      elig_prob = NULL, # figures/tables for Eligibility Probability Screen
                      master_df = NULL, # dataframe of all data to be used in the app
                      sel_df = NULL, # dataframe reflecting data for current trial/cohort/real-world data set
                      elig_model_df = NULL, # dataframe with cohort weights returned by eligmodel
                      elig_vars_dict = NULL, # dataframe containing clean names for eligibility criteria
                      bsl_vars_dict = NULL, # dataframe containing clean names for baseline characteristics
                      institution_dict = NULL,
                      upset_plot = NULL, # upset plot of IE Criteria for selected trial/cohort/rwd
                      elig_prob_bin_bounds = NULL,
                      elig_mod_fit = NULL,
                      surv_outcomes = NULL,
                      surv_dat = NULL,
                      char_dat = NULL,
                      elig_dat = NULL,
                      data_dict = NULL,
                      states_sf_area = NULL,
                      color_elig = NULL,
                      color_inelig = NULL,
                      color_unkn = NULL,
                      color_aug = NULL,
                
                      
                      ## -------------------------------- Public Functions ----------------------------------------------
                      
                      initialize = function(char_dat, elig_dat, data_dict, surv_dat = NULL, color_elig = "#6b7ad1", color_inelig = "#99c418", color_unkn = "#69B8F7", color_aug = "#0C2340") {

                        private$get_states_sf_area(
                          system.file(
                            "extdata/us-state-boundaries.zip", 
                            package = "eligmodel"
                          )
                        )
                        
                        self$color_elig <- color_elig
                        self$color_inelig <- color_inelig
                        self$color_unkn <- color_unkn
                        self$color_aug <- color_aug
                        
                        self$char_dat <- char_dat
                        self$elig_dat <- elig_dat
                        self$data_dict <- data_dict
                        self$surv_dat <- surv_dat
                        
                        self$trial <- ""
                        self$cohort <- ""
                        self$dataset <- ""
                        self$section_header <- ""
                        self$trial_elig_crit <- list(
                          attr_tbl_elig_stat = NULL, 
                          barplot_elig_stat = NULL
                        )
                        self$pat_char <- list(
                          tbl_pat_geo_report = NULL, 
                          tbl_pat_geo = NULL, 
                          plot_pat_geo = NULL, 
                          plot_pat_geo_report = NULL, 
                          tbl_pat_char = NULL, 
                          elig_pat_plot = NULL, 
                          unkn_pat_plot = NULL, 
                          inelig_pat_plot = NULL, 
                          ec_elig_pat_plot = NULL, 
                          ec_unkn_pat_plot = NULL, 
                          ec_inelig_pat_plot = NULL
                        )
                        self$elig_prob <- list(
                          plot_elig_prob = NULL, 
                          tbl_cohort_counts = NULL, 
                          augmented_cohort = NULL
                        )
                        self$surv_outcomes <- list(
                          km_plot_tbl = NULL, 
                          median_tbl = NULL
                        )
                        self$elig_prob_bin_bounds <- list(
                          min_bindwith = NULL, 
                          max_binwidth = NULL
                        )
                        private$generate_section_header()
                        private$make_master_df(
                          self$surv_dat, 
                          self$char_dat, 
                          self$elig_dat
                        )
                      },
                      
                      update_report = function(input, elig_prob_binwidth, elig_prob_cutoff) {
                        
                        withProgress(message = 'Updating Report', value = 0, {
                          print("Begin updating report")
                          # update key params
                          self$trial <- input$sel_trial
                          self$cohort <- input$sel_cohort
                          self$dataset <- input$sel_rwd
                          self$sel_elig_crit <- input$sel_elig_crit
                          self$sel_bsl_char <- input$sel_bsl_char
                          
                          self$elig_vars_dict <- self$data_dict[self$data_dict$var_type == "Eligibility Criteria" &
                                                                  self$data_dict$trial == self$trial &
                                                                  self$data_dict$rwd == self$dataset,]
                          self$bsl_vars_dict <- self$data_dict[self$data_dict$var_type == "Patient Characteristic" &
                                                                 self$data_dict$trial == self$trial &
                                                                 self$data_dict$rwd == self$dataset,]
                          self$institution_dict <- self$data_dict[self$data_dict$var_type == "Geographic_Alt" &
                                                                    self$data_dict$trial == self$trial &
                                                                    self$data_dict$rwd == self$dataset,]
                          print("Report attributes updated")
                          incProgress(0.025)
                          
                          # update data frame subset to selections
                          private$update_sel_df()
                          print("Subsetted dataset updated")
                          incProgress(0.025)
                          
                          # update section header
                          private$generate_section_header()
                          print("Section header updated")
                          incProgress(0.05)
                          
                          # update plots and tables
                          private$gen_upset_plot()
                          print("Upset Plot generated")
                          incProgress(0.025)
                          
                          # update table of selected eligibility criteria
                          private$update_tbl_sel_ec(input)
                          print("Table of selected eligibility criteria updated")
                          incProgress(0.1)
                          # update table of selected baseline characteristics
                          private$update_tbl_sel_bsl(input)
                          print("Table of selected baseline characteristics updated")
                          incProgress(0.1)
                          
                          private$update_trial_elig_crit()
                          print("Figures for Trial Eligibility Criteria Screen updated")
                          incProgress(0.2)
                          
                          private$gen_elig_prob(elig_prob_binwidth, elig_prob_cutoff)
                          print("Plot and table for Eligibility Probability screen updated")
                          incProgress(0.15)
                          
                          if(!is.null(self$surv_dat)){
                            self$update_survival_outcomes(elig_prob_cutoff)
                            print("Plot and table for Survival Outconmes screen updated")
                            incProgress(0.15)
                          }
                          
                          private$gen_pat_char()
                          print("Figures for Patient Characteristics screen updated")
                          incProgress(0.15)
                          
                          print("Done updating report")
                        })
                      },
                      
                      update_elig_prob = function(elig_prob_binwidth, elig_prob_cutoff){
                        # update histogram of unknown cohort
                        if(!is.null(self$elig_model_df)){
                          suppressWarnings(
                            suppressMessages(
                              private$update_plot_elig_prob(
                                elig_prob_binwidth, 
                                elig_prob_cutoff
                              )
                            )
                          )
                          print("Eligibility histogram updated")
                          
                          suppressWarnings(
                            suppressMessages(
                              private$update_tbl_elig_prob(elig_prob_cutoff)
                            )
                          )
                          print("Eligibility probability table updated")
                        }
                      },
                      
                      update_pat_char = function(sel_bsl_char, sel_bsl_strat, sel_bsl_plot, sel_ec_vec) {
                        
                        self$pat_char$elig_pat_plot <- private$plot_bsl_char(
                          "Eligible", 
                          sel_bsl_char, 
                          sel_bsl_strat, 
                          sel_bsl_plot, 
                          sel_ec_vec
                        )
                        print("Patient characteristic plot for eligible patients updated")
                        
                        self$pat_char$unkn_pat_plot <- private$plot_bsl_char(
                          "Unknown", 
                          sel_bsl_char, 
                          sel_bsl_strat, 
                          sel_bsl_plot, 
                          sel_ec_vec
                        )
                        print("Patient characteristic plot for unknown patients updated")
                        
                        self$pat_char$inelig_pat_plot <- private$plot_bsl_char(
                          "Ineligible", 
                          sel_bsl_char, 
                          sel_bsl_strat, 
                          sel_bsl_plot, 
                          sel_ec_vec
                        )
                        print("Patient characteristic plot for ineligible patients updated")
                      },
                      
                      reset_pat_char = function() {
                        
                        self$pat_char$elig_pat_plot <- NULL
                        
                        self$pat_char$unkn_pat_plot <- NULL
                        
                        self$pat_char$inelig_pat_plot <- NULL
                        
                        self$pat_char$ec_elig_pat_plot <- NULL
                        
                        self$pat_char$ec_unkn_pat_plot <- NULL
                        
                        self$pat_char$ec_inelig_pat_plot <- NULL
                      },
                      
                      verify_report_sel = function(input){
                        # verify report trial matches input trial
                        trial <- self$trial == input$sel_trial
                        # verify report cohort matches input cohort
                        cohort <- self$cohort == input$sel_cohort
                        # verify report rwd matches input rwd
                        dataset <- self$dataset == input$sel_rwd
                        
                        # verify report EC matches input EC
                        EC <- length(setdiff(self$sel_elig_crit, input$sel_elig_crit)) == 0 & 
                          length(setdiff(input$sel_elig_crit, self$sel_elig_crit)) == 0
                        bsl <- length(setdiff(self$sel_bsl_char, input$sel_bsl_char)) == 0 & 
                          length(setdiff(input$sel_bsl_char, self$sel_bsl_char)) == 0
                        
                        checks_df <- data.frame("trial" = c(trial),
                                                "cohort" = c(cohort),
                                                "dataset" = c(dataset),
                                                "eligibility criteria" = c(EC),
                                                "baseline characteristics" = c(bsl))
                        
                        ret1 <- trial & cohort & dataset & EC & bsl
                        if(ret1){
                          ret2 <- ""
                        }else{
                          ret2 <- paste0("You have made updates to: [",
                                         gsub("\\.", " ", paste(colnames(checks_df)[colSums(checks_df == FALSE) == nrow(checks_df)], collapse = ", ")), 
                                         "]. Please update the report before download.")
                        }
                        
                        ret <- list("bool" = ret1,
                                    "message" = ret2)
                        
                      },
                      
                      
                      update_survival_outcomes = function(elig_prob_cutoff) {
                        if(!is.null(self$surv_dat)){
                          if(is.null(elig_prob_cutoff)){
                            elig_prob_cutoff <- 0.5
                          }
                
                          suppressWarnings(
                            suppressMessages(
                              private$update_survival_figs(elig_prob_cutoff)
                            )
                          )
                        }
                      }
                    ),
                    
                    private = list(
                      ## --------------------------------- Private Functions ---------------------------------------------
                      ## ------------------------------- General -----------------------------
                      get_states_sf_area = function(zip_path){
                        unzip(zip_path, exdir = tempdir())
                        shp_path <- file.path(tempdir(), "us-state-boundaries.shp")
                        
                        self$states_sf_area <- st_read(shp_path, quiet = TRUE)
                      },

                      make_master_df = function(surv_dat, char_dat, elig_dat){
                        # merge data into one data set
                        ret <- merge(char_dat, elig_dat, by = c("record_id", "trial", "cohort", "rwd"))
                        if(!is.null(surv_dat)){
                          ret <- merge(ret, surv_dat, by = c("record_id", "trial", "cohort", "rwd"), all.x = TRUE)
                        }

                        self$master_df <- ret
                      },
                      
                      update_sel_df = function(){
                        df <- as.data.frame(copy(self$master_df))
                        
                        df <- df[df$trial == self$trial & df$cohort == self$cohort & df$rwd == self$dataset,]
                        # columns that don't belong to the current trial/dataset/cohort will only have NA values so remove those
                        valid_cols <- colnames(df)[colSums(is.na(df)) < nrow(df)]
                        df <- df[, valid_cols]
                        
                        # Coerce diagnosis year into character variable for use as categorical variable in eligmodel package and formatting patient characteristics table
                        if("diag_year" %in% colnames(df)){
                          df$diag_year <- as.character(df$diag_year)
                        }
                        
                        # create a variable indicate eligibility status
                        # Creating a string of conditions for Eligible and Ineligible
                        elig_conditions <- paste(self$sel_elig_crit, "== 1", collapse = " & ")
                        inelig_conditions <- paste(self$sel_elig_crit, "== 0", collapse = " | ")
                        
                        df <- df %>%
                          mutate(
                            elig_status = case_when(
                              eval(parse_expr(elig_conditions)) ~ "Eligible",
                              eval(parse_expr(inelig_conditions)) ~ "Ineligible",
                              TRUE ~ "Unknown"
                            )
                          )
                        
                        self$sel_df <- df
                        
                      },
                      
                      generate_section_header = function() {
                        self$section_header <- h4(HTML(paste("<strong>Clinical Trial:</strong> ", self$trial,
                                                             " | <strong>Real-World Dataset:</strong> ", self$dataset,
                                                             " | <strong>Cohort:</strong> ", self$cohort)))
                      },
                      
                      ## ---------------------------- Home Screen -----------------------------

                      make_sel_tbl = function(full_vars, sel_vars, var_type = "Criteria") {
                        criteria_clean <- full_vars$clean_name[full_vars$col_name %in% colnames(self$sel_df)]
                        criteria_col <- full_vars$col_name[full_vars$col_name %in% colnames(self$sel_df)]
                        
                        ret <- data.frame(
                          Criteria = criteria_clean,
                          'Selected for Analysis' = ifelse(criteria_clean %in% criteria_clean[criteria_col %in% sel_vars],
                                                           "<span style='color:green'>+</span>",
                                                           "<span style='color:red'>x</span>")
                        )
                        
                        ret <- DT::datatable(ret, 
                                         colnames = c(var_type, "Selected for Analysis"),
                                         escape = FALSE,
                                         options = list(
                                           dom = 't',
                                           columnDefs = list(list(
                                             targets = 1, 
                                             width = '100px', 
                                             className = 'dt-center'))
                                         ),
                                         rownames = FALSE)
                        
                        return(ret)
                      },

                      update_tbl_sel_ec = function(input){
                        self$tbl_elig_crit <- private$make_sel_tbl(
                          self$elig_vars_dict, 
                          self$sel_elig_crit
                        )
                      },

                      update_tbl_sel_bsl = function(input){
                        self$tbl_bsl_char <- private$make_sel_tbl(
                          full_vars = self$bsl_vars_dict, 
                          sel_vars = self$sel_bsl_char, 
                          var_type = "Patient Characteristic"
                        )
                      },

                      gen_upset_plot = function(){
                        
                        # should user be able to set color_scheme()?
                        # should color_scheme() correspond to the values input in some way?
                        # RColorBrewer may be a good source of colorschemes if we 
                        # do want to offer more options
                        color_scheme <- c("#00857C", "#BFED33", "#6ECEB2", "#0C2340","#724a91","#829e9d","#FFF063",
                                          "#3fa2d4", "#7f7f7f", "#87e091", "#dcf1f2", "#56c24a", "#361975",
                                          "#afa1f0","#f0af0c", "#1f5930", "#5450E4", "#11d978", "#46494f", "#c9bb4d", "#daccdb")
                        
                        # criteria relevant to selected trial/rwd/cohort
                        criteria_clean <- self$elig_vars_dict$miscellaneous[self$elig_vars_dict$col_name %in% colnames(self$sel_df)]
                        criteria_col <- self$elig_vars_dict$col_name[self$elig_vars_dict$col_name %in% colnames(self$sel_df)]
                        
                        df <- as.data.frame(copy(self$sel_df))[, c(criteria_col)]
                        colnames(df) <- criteria_clean[colnames(df) %in% self$elig_vars_dict$col_name]
                        
                        # get logical df indicating non missing values
                        df[!is.na(df)] <- TRUE
                        df[is.na(df)] <- FALSE
                        
                        ret <- ComplexUpset::upset(df, 
                                                   criteria_clean,
                                                   name = "Eligibility Criteria",
                                                   mode = "inclusive_intersection",
                                                   width_ratio = 0.3,
                                                   min_degree = 2,
                                                   intersections = "all",
                                                   n_intersections = 20,
                                                   base_annotations = list(
                                                     'Intersection size' = ComplexUpset::intersection_size(aes(fill = "x"),
                                                                                             mode = "inclusive_intersection",
                                                                                             text = list(size = 3)) +
                                                       scale_fill_manual(values = "#00857C") +
                                                       guides(fill="none") +
                                                       ylab("Patients with Available Data")
                                                   ),
                                                   sort_intersections = 'descending',
                                                   set_sizes = ComplexUpset::upset_set_size(aes(fill = group)) + 
                                                     scale_fill_manual(values = color_scheme[1:length(criteria_clean)]) + 
                                                     guides(fill="none") +
                                                     geom_text(aes(label=..count..), hjust=1.1, stat='count', size = 4) +
                                                     expand_limits(y = nrow(df)*1.3) +
                                                     theme(text = element_text(family = 'Source Sans Pro', size = 14),
                                                           axis.title.x = element_text(family = 'Source Sans Pro', size = 12)) +
                                                     ylab('Patients with Available Data'),
                                                   matrix = ComplexUpset::intersection_matrix(),
                                                   themes = ComplexUpset::upset_default_themes(text = element_text(family = 'Source Sans Pro', size = 16))
                        )
                        
                        self$upset_plot <- ret
                      },
                      
                      
                      ## -------------------------- Trial Eligibility Criteria Screen ---------------------------

                      update_trial_elig_crit = function() {
                        
                        # update table summarizing patient eligibility status
                        private$update_attr_tbl_elig_stat()
                        
                        # update histogram of availability of included trial eligibility criteria
                        private$update_barplot_elig_stat()
                      },

                      update_barplot_elig_stat = function() {
                        # dataframe of eligibility criteria 
                        df <- as.data.frame(copy(self$sel_df))
                        
                        # following preparation steps assume that each patient is
                        # associated with one row in df - ok if we know that the
                        # data is well-behaved, but could pose problems if we
                        # want to apply to other settings
                        df <- df[c(self$sel_elig_crit)]
                        
                        df_long <- df %>%
                          tidyr::pivot_longer(cols = everything(), names_to = "Eligibility_Criteria", values_to = "Status")
                        
                        # Replace NA with a custom label ("Unknown") and then count the occurrences
                        df_long <- df_long %>%
                          mutate(Status = case_when(
                            is.na(Status) ~ "Unknown",  # Replace NA with "Unknown"
                            Status == 1 ~ "Eligible",   # 1 becomes "Eligible"
                            Status == 0 ~ "Ineligible"  # 0 becomes "Ineligible"
                          ))
                        
                        # Summarize the data to count the occurrences of each category for each eligibility criterion
                        df_summary <- df_long %>%
                          group_by(Eligibility_Criteria, Status) %>%
                          summarise(Count = n(), .groups = 'drop') %>% 
                          mutate(
                            # Calculate the total number of patients per eligibility criterion
                            Total = nrow(df),
                            # Calculate the percentage for each category
                            Percentage = (Count / Total) * 100,
                            Eligibility_Criteria = self$elig_vars_dict$clean_name[
                              match(Eligibility_Criteria, self$elig_vars_dict$col_name)
                            ]
                          )
                       
                        ret <- ggplot(df_summary, aes(x = str_wrap(Eligibility_Criteria, width = 30), y = Count, fill = Status)) +
                          geom_bar(stat = "identity") +
                          geom_text(aes(label = paste0(round(Percentage, 1), "%"), color = Status), 
                                    position = position_stack(vjust = 0.5), size = 5, show.legend = FALSE, fontface = "bold") +
                          labs(x = "Eligibility Criteria", y = "Count of Patients", fill = "Eligibility Status") +
                          plot_theme() +
                          scale_fill_manual(values = c("Eligible" = self$color_elig, "Ineligible" = self$color_inelig, "Unknown" = self$color_unkn)) +
                          # theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
                          scale_color_manual(values = c("Eligible" = "white", "Ineligible" = "#0C2340", "Unknown" = "#0C2340"))   # Set custom colors for each status
                        
                        self$trial_elig_crit$barplot_elig_stat <- ret
                      },

                      update_attr_tbl_elig_stat = function(){ 
                        
                        df <- as.data.frame(copy(self$sel_df))
                        df <- df[c(self$sel_elig_crit)]
                        # change column names to clean versions of EC
                        colnames(df) <- self$elig_vars_dict$miscellaneous[match(colnames(df), self$elig_vars_dict$col_name)]
                        
                        # Total number of patients
                        total_patients <- nrow(df)
                        
                        # Initialize a vector to store cumulative counts
                        cumulative_counts <- numeric(length = ncol(df))
                        
                        # Compute cumulative counts for eligible or unknown patients
                        # For each criteria, only count patients who have passed all previous criteria
                        eligible_or_unknown <- function(x) {
                          sum(x == 1 | is.na(x))  # Eligible or unknown (1 or NA)
                        }
                        
                        # Start with the total number of patients
                        cumulative_counts[1] <- eligible_or_unknown(df[[1]])
                        
                        # Now calculate cumulative counts for the subsequent criteria
                        # This is only if there are more than 1 EC selected
                        if(ncol(df) > 1){
                          for (i in 2:ncol(df)) {
                            # Ensure we are using a matrix-like structure (always more than 1 column)
                            previous_criteria <- df[, 1:(i-1), drop = FALSE]  # drop = FALSE ensures it's still a data frame
                            
                            # For each subsequent criterion, we only count those who passed all prior criteria
                            # Why i-1?: At criterion i, we want to check if the patient passed all previous criteria (i.e., the first i-1 criteria).
                            # There should be i-1 rows with TRUE/counted in rowsums
                            # If a patient is eligible or unknown for all previous criteria, their sum of TRUE values will be equal to i-1. 
                            # If it's less, it means they failed at least one of the previous criteria, and they should not be counted as eligible or unknown for the current criterion.
                            passed_previous_criteria <- rowSums(previous_criteria == 1 | is.na(previous_criteria)) == (i-1)
                            cumulative_counts[i] <- sum((df[[i]] == 1 | is.na(df[[i]])) & passed_previous_criteria)
                          }
                          nodes <- data.frame(id = c(1:(ncol(df) + 1)),
                                              shape = rep("box", ncol(df) + 1),
                                              label = c(
                                                paste0("Total Patients: ", total_patients),
                                                paste0(as.character(cumulative_counts[1:(ncol(df) - 1)]), " Patients"),
                                                paste0("Augmented Cohort (Eligible and Unknown Patients): ", cumulative_counts[ncol(df)])),
                                              # value = rep(100, ncol(df) + 1),
                                              fixed = rep(TRUE, ncol(df) + 1)
                          )
                          
                          edges <- data.frame(from = c(1:ncol(df)), to = c(2:(ncol(df)+1)), label = colnames(df))
                          
                        }else{
                          # I have been unable to test with only one EC selected
                          # I receive a "Failure to Update Report" message and
                          # need to relaunch the app
                          
                          # working on identifying the source of this message now
                          # failure seems to occur after output of
                          # "Figures for Trial Eligibility Criteria Screen updated
                          # in update_report()
                          
                          nodes <- data.frame(id = c(1:(ncol(df) + 1)),
                                              shape = rep("box", ncol(df) + 1),
                                              label = c(
                                                paste0("Total Patients: ", total_patients),
                                                paste0("Augmented Cohort (Eligible and Unknown Patients): ", cumulative_counts[ncol(df)])),
                                              # value = rep(100, ncol(df) + 1),
                                              fixed = rep(TRUE, ncol(df) + 1)
                          )
                          
                          edges <- data.frame(from = c(1:ncol(df)), to = c(2:ncol(df)+1), label = colnames(df))
                        }
                        
                        # I found it unclear whether eligible and unknown patients were counted
                        # for all eligibility criteria, or just the final criterion
                        # updated text to clarify
                        ret <- visNetwork::visNetwork(
                          nodes, 
                          edges, 
                          width = "100%",
                          main = list(
                            text = "Patient Eligibility Status by Criterion",
                            style = "font-family:Source Sans Pro; font-size:20px; text-align: center;"
                          ),
                          submain = list(
                            text = "Eligible and Unknown",
                            style = "font-family:Source Sans Pro; font-size:16px; text-align: center;"
                          )
                        ) %>%
                          visNetwork::visNodes(shape = 'box', 
                                   widthConstraint = 150,
                                   font = list(
                                     face = "Source Sans Pro",  # Apply custom font to node labels
                                     size = 16,
                                     color = "#00857C"
                                   ),
                                   color = list(
                                     background = "#e8ebee", border = "#00857C")) %>%
                          visNetwork::visEdges(arrows = 'to', 
                                   color = list(color = '#00857C'),
                                   font = list(
                                     face = "Source Sans Pro",  # Apply custom font to edge labels
                                     size = 16,
                                     color = "#0C2340"
                                   )) %>%
                          visNetwork::visHierarchicalLayout() %>%       
                          visNetwork::visInteraction(dragNodes = FALSE, 
                                         dragView = FALSE, 
                                         zoomView = FALSE) 
                        
                        self$trial_elig_crit$attr_tbl_elig_stat <- ret
                      },
                      
                      ## -------------------------- Eligibility Probability Screen ----------------------------

                      gen_elig_prob = function(elig_prob_binwidth, elig_prob_cutoff){
                        # generate eligibility model
                        private$gen_elig_model()
                        print("eligmodel generated")
                        
                        # update min/max of histogram slider
                        private$get_min_max_binwidth()
                        print("hist binwidth min/max generated")
                        
                        # update histogram of unknown cohort
                        private$update_plot_elig_prob(elig_prob_binwidth, elig_prob_cutoff)
                        print("histogram generated")
                        
                        # update table of patient counts by cohort
                        private$update_tbl_elig_prob(elig_prob_cutoff)
                        print("table of patient counts generated")
                      },

                      get_min_max_binwidth = function(){
                        df <- as.data.frame(copy(self$elig_model_df))
                        
                        df <- df[self$elig_model_df$elig == "Unknown",]
                        self$elig_prob_bin_bounds$min_binwidth <- min(df$w_elig)
                        self$elig_prob_bin_bounds$max_binwidth <- max(df$w_elig)
                      },
 
                      gen_elig_model = function(){
                        sel_df <- as.data.frame(copy(self$sel_df))

                        # parameters for elig_model function
                        X <- sel_df[self$sel_bsl_char]
                        # make character baseline characteristic variables factors
                        categ_vars <- X[self$sel_bsl_char] %>%
                          select(where(is.character))
                        
                        X[colnames(categ_vars)] <- lapply(X[colnames(categ_vars)], factor)
                        
                        elig.dat <- sel_df[self$sel_elig_crit]
                        
                        if(!is.null(self$surv_dat)){
                          # get outcome (death) for survival
                          time <- as.numeric(self$sel_df$time)
                          status <- as.numeric(self$sel_df$status)
                        }else{
                          # if no survival data provided, use sample data
                          sample_surv <- survival::colon
                          time <- fractional_rep(sample_surv$time, nrow(self$sel_df)/nrow(sample_surv))
                          status <- fractional_rep(sample_surv$status, nrow(self$sel_df)/nrow(sample_surv))
                        }
                        
                        Y <- survival::Surv(time, status)
                        
                        mod <- elig_model(Y = Y, X = X, elig.dat = elig.dat, elig.mod = "rfmv")
                        print("model generated")
                        pdat <- data.frame(Y = mod$Y, mod$elig.dat)
                        pdat$elig <- factor(pdat$elig,  levels = c("0", "1", "NA"), labels = c("Ineligible", "Eligible", "Unknown"))
                        
                        self$elig_model_df <- pdat
                        self$elig_mod_fit <- mod
                      },

                      update_tbl_elig_prob = function(elig_prob_cutoff){
                        
                        if(is.null(elig_prob_cutoff)){
                          elig_prob_cutoff <- 0.5
                        }
                        
                        full_df <- as.data.frame(copy(self$elig_model_df))
                        
                        # df for eligible, ineligible and unknown cohort
                        cohort_df = full_df %>%
                          group_by(elig) %>%
                          summarise(Count = n(), .groups = 'drop')
                        
                        augmented_df <- full_df[full_df$w_elig >= elig_prob_cutoff,]
                        augmented_row <- data.frame(elig = "Augmented", Count = nrow(augmented_df))
                        df <- rbind(cohort_df, augmented_row)
                        
                        ret <- DT::datatable(df, 
                                         colnames = c("Eligibility Cohort", "Patient Count"),
                                         escape = FALSE,
                                         options = list(
                                           dom = 't',
                                           columnDefs = list(list(
                                             targets = 1, 
                                             width = '35%', 
                                             className = 'dt-center'),
                                             list(
                                               targets = 0, 
                                               width = '65%'))
                                         ),
                                         rownames = FALSE)  %>%
                          DT::formatCurrency("Count", currency = "", mark = ",", digits = 0) %>%
                          DT::formatStyle(
                            "elig", 
                            target = 'row',
                            fontWeight = 'normal'  # Ensure font weight is normal
                          ) %>%
                          DT::formatStyle(
                            "Count", 
                            target = 'row',
                            fontWeight = 'normal'  # Ensure font weight is normal
                          )
                        
                        self$elig_prob$tbl_cohort_counts <- ret
                        
                        # prepare dataframe for download
                        weights_w_ids <- data.frame(record_id = self$sel_df$record_id, elig_status = self$sel_df$elig_status, weight = self$elig_model_df$w_elig)
                        augmented_w_ids <- weights_w_ids[weights_w_ids$weight >= elig_prob_cutoff,]

                        self$elig_prob$augmented_cohort <- augmented_w_ids
                      },
                      
                      update_plot_elig_prob = function(elig_prob_binwidth, elig_prob_cutoff) {
                        
                        if(is.null(elig_prob_cutoff)){
                          elig_prob_cutoff <- 0.5
                        }
                        
                        if(is.null(elig_prob_binwidth)){
                          elig_prob_binwidth <- 0.05
                        }
                        
                        elig_model_df <- as.data.frame(copy(self$elig_model_df))
                        
                        df <- elig_model_df[self$elig_model_df$elig == "Unknown",]
                        
                        ret <- ggplot(df, aes(x = w_elig, fill = elig)) +
                          geom_histogram(binwidth = elig_prob_binwidth) + 
                          xlab("Estimated Weight") +
                          ylab("Number of Patients") +
                          scale_x_continuous(limits = c(0, 1)) +
                          scale_fill_manual(values = c("#00857C")) + 
                          plot_theme() +
                          theme(legend.position = "none") +
                          geom_vline(xintercept = elig_prob_cutoff, linetype = "dashed", color = "#0C2340", linewidth = 1) +
                          geom_vline(xintercept = 0, linetype = "dashed", color = "#0C2340", linewidth = 1) + 
                          geom_vline(xintercept = 1, linetype = "dashed", color = "#0C2340", linewidth = 1) +
                          annotate("text", x = elig_prob_cutoff, y = 0, label = "Cut-off of Augmented Cohort", 
                                   color = "#0C2340", angle = 90, hjust = -1, vjust = -1, size = 4) +
                          annotate("text", x = 0, y = 0, label = "Weight of Ineligible Patients", 
                                   color = "#0C2340", angle = 90, hjust = -1, vjust = -1, size = 4) +
                          annotate("text", x = 1, y = 0, label = "Weight of Eligible Patients", 
                                   color = "#0C2340", angle = 90, hjust = -1, vjust = -1, size = 4)
                        
                        self$elig_prob$plot_elig_prob <- ret
                        
                      },
                      ## -------------------------- Patient Characteristics Screen  ---------------------------

                      bin_num_var = function(bin_range, df, var, baseline_var){
                        # multiple coercions is likely unnecessary
                        max_val <- max(as.integer(as.character(unlist(df[var]))))
                        min_val <- min(as.integer(as.character(unlist(df[var]))))
                        even_split <- (max_val - min_val) %% bin_range 
                        
                        if(even_split == 0) {
                          age_breaks <- seq(min_val, max_val, by = bin_range)
                        } else{
                          base_seq <- seq(min_val, max_val, by = bin_range)
                          age_breaks <- c(base_seq, base_seq[length(base_seq)] + bin_range)
                        }
                        
                        # Use cut() to create the var_bin column
                        df[paste0(var, '_bin')] <- cut(as.integer(as.character(unlist(df[var]))), breaks = age_breaks, right = TRUE, 
                                                       labels = paste(head(age_breaks, -1), head(age_breaks, -1) + (bin_range - 1), sep = "- "), 
                                                       include.lowest = TRUE)
                        
                        # remove original variable and replace with bin version
                        if(!is.null(baseline_var)){
                          baseline_var <- c(baseline_var[!baseline_var %in% var], paste0(var, '_bin'))
                        }
                        
                        return(list('df' = df, 'baseline_var' = baseline_var))
                      },

                      gen_color_scheme = function(df, sel_var){
                        # should color scheme adjust to user input?
                        color_scheme <- c("#00857C", "#724a91","#BFED33", "#6ECEB2", "#0C2340","#829e9d","#FFF063", "#3fa2d4", 
                                          "#7f7f7f", "#87e091", "#dcf1f2", "#56c24a", "#361975","#f0af0c",
                                          "#afa1f0", "#1f5930", "#5450E4", "#11d978", "#46494f", "#c9bb4d", "#daccdb")

                        text_scheme <- c("#dcf1f2", "#dcf1f2", "#0C2340", "#0C2340", "#dcf1f2", "#0C2340", "#0C2340","#0C2340",
                                         "#dcf1f2", "#0C2340", "#0C2340", "#0C2340", "#dcf1f2", "#0C2340",
                                         "#0C2340", "#dcf1f2", "#dcf1f2", "#0C2340", "#dcf1f2", "#0C2340","#0C2340")
                        
                        # sort strata_vals to ensure that colors are assigned
                        # to same strata regardless of their order in df
                        strata_vals <- unique(df[[sel_var]]) %>% 
                          sort()
                        
                        if(length(strata_vals) > length(color_scheme)){
                          ret_names <- strata_vals[1:length(color_scheme)]
                          fill_vals <- color_scheme
                          text_vals <- text_scheme
                          
                          ret_fill <- setNames(fill_vals, ret_names)
                          ret_text <- setNames(text_vals, ret_names)
                        }else if(length(color_scheme) > length(strata_vals)){
                          ret_names <- strata_vals
                          fill_vals <- color_scheme[1:length(strata_vals)]
                          text_vals <- text_scheme[1:length(strata_vals)]
                          
                          ret_fill <- setNames(fill_vals, ret_names)
                          ret_text <- setNames(text_vals, ret_names)
                        } else {
                          ret_fill <- setNames(color_scheme, strata_vals)
                          ret_text <- setNames(text_scheme, strata_vals)
                        }
                        
                        return(list("fill_color" = ret_fill, "text_color" = ret_text))
                      },

                      gen_pat_char = function() {
                        private$update_tbl_pat_char()
                        
                        if("geographic" %in% colnames(self$sel_df)){
                          private$plot_pat_geo()
                          
                          private$tbl_pat_geo()
                        }
                        
                        self$pat_char$elig_pat_plot <- NULL
                        
                        self$pat_char$unkn_pat_plot <- NULL
                        
                        self$pat_char$inelig_pat_plot <- NULL
                        
                      },
                      gen_sf_plot_data = function(){
                        
                        # load file with plotting data
                        ret <- usmap::us_map() %>% rename(geometry = geom, state_abbr = abbr) %>%
                          mutate(centroid = st_centroid(geometry)) %>% 
                          st_transform(crs = 4326)

                        ret <- ret %>%
                          mutate(centroid = st_centroid(geometry)) 
                        
                        return(ret)
                        
                      },
                      
                      institution_or_state = function(){
                        dict <- copy(self$institution_dict)
                        dict_vals <- dict$col_name
                        
                        ret <- any(dict_vals %in% self$sel_df$geographic)
                        
                        return(ret)
                      },
                      
                      
                      tbl_pat_geo = function(){
                        geo_df <- copy(self$sel_df)[, c("geographic"), drop = FALSE]
                        
                        if(private$institution_or_state()){
                          dict <- copy(self$institution_dict)
                          colnames(dict) <- c("var_type", "geographic", "clean_name", "state")
                          
                          # Merge with dict to get the state and clean name info
                          df_merged <- merge(geo_df, dict, by = "geographic")
                          
                          # Aggregate data to get the count of patients per state, while keeping clean_name
                          
                          df <- df_merged %>%
                            group_by(state, clean_name) %>%
                            summarise(count = n(), .groups = "drop") %>%
                            arrange(state) %>% 
                            rename(State = state, Location = clean_name, `Patient Count` = count)
    
                          
                          ret_report <- DT::datatable(df, 
                                           colnames = c("State", "Location", "Patient Count"),
                                           escape = FALSE,
                                           options = list(
                                             columnDefs = list(list(
                                               targets = 1,  # 0-based index: 0 = first column, 1 = second column
                                               className = 'dt-center'
                                             )),
                                             dom = 't',
                                             paging = FALSE),
                                           rownames = FALSE)  %>%
                            DT::formatStyle(columns = c(2), fontSize = '85%')
                          
                          ret <- DT::datatable(df, 
                                                      colnames = c("State", "Location", "Patient Count"),
                                                      escape = FALSE,
                                                      options = list(
                                                        columnDefs = list(list(
                                                          targets = 1,  # 0-based index: 0 = first column, 1 = second column
                                                          className = 'dt-center'
                                                        )),
                                                        dom = 'tp',
                                                        pageLength = 12),
                                                      rownames = FALSE)  %>%
                            DT::formatStyle(columns = c(2), fontSize = '85%')
                        } else{
                          
                          # Aggregate data to get the count of patients per state, while keeping clean_name
                          df <- geo_df %>%
                            group_by(geographic) %>%
                            summarise(count = n(), .groups = "drop") %>%
                            arrange(geographic) %>% 
                            rename(State = geographic, `Patient Count` = count)
                          
                          ret_report <- DT::datatable(df, 
                                           colnames = c("State", "Patient Count"),
                                           escape = FALSE,
                                           options = list(
                                             columnDefs = list(list(
                                               targets = 1,  # 0-based index: 0 = first column, 1 = second column
                                               className = 'dt-center'
                                             )),
                                             dom = 't',
                                             paging = FALSE),
                                           rownames = FALSE)  
                          ret <- DT::datatable(df, 
                                                      colnames = c("State", "Patient Count"),
                                                      escape = FALSE,
                                                      options = list(
                                                        columnDefs = list(list(
                                                          targets = 1,  # 0-based index: 0 = first column, 1 = second column
                                                          className = 'dt-center'
                                                        )),
                                                        dom = 'tp',
                                                        pageLength = 12),
                                                      rownames = FALSE)
                        }
                        
                        self$pat_char$tbl_pat_geo_report <- ret_report
                        self$pat_char$tbl_pat_geo <- ret
                      },
                      
                      plot_pat_geo = function(){
                        geo_df <- copy(self$sel_df)[, c("geographic"), drop = FALSE]
                        
                        if(private$institution_or_state()){
                          dict <- copy(self$institution_dict)
                          colnames(dict) <- c("var_type", "geographic", "clean_name", "state")
                          
                          # Merge with dict to get the state and clean name info
                          df_merged <- merge(geo_df, dict, by = "geographic")
                          
                          # Aggregate data to get the count of patients per state, while keeping clean_name
                          state_counts <- df_merged %>%
                            group_by(state, clean_name) %>%
                            summarise(count = n(), .groups = "drop")
                          
                        } else{
                          geo_df = geo_df %>%
                            rename(state = geographic)
                          
                          # Aggregate data to get the count of patients per state, while keeping clean_name
                          state_counts <- geo_df %>%
                            group_by(state) %>%
                            summarise(count = n(), .groups = "drop")
                        }
                        
                        # get geo data
                        states_sf <- private$gen_sf_plot_data()
                        # merge counts onto geo data
                        states_sf <- left_join(states_sf, state_counts, by = c("state_abbr" = "state")) 
                        pal <- colorNumeric(palette = "BuGn", domain = states_sf$count)
                        
                        label_data <- states_sf[which(!is.na(states_sf$count)),] %>%
                             st_point_on_surface() %>%
                             bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
                             mutate(label = paste0(state_abbr, " (N = ", count, ")"))
                        
                        app_ret <- leaflet(states_sf) %>%
                          addTiles(urlTemplate = "", attribution = "")%>%
                          addPolygons(
                          fillColor = ~pal(count),
                          fillOpacity = 0.7,
                          color = "#000000",
                          weight = 1,
                          label = ~paste0(state_abbr, ": N = ", count),
                          labelOptions = labelOptions(direction = "auto")
                          ) %>%
                          addLegend(
                          "bottomright", pal = pal, values = ~count[!is.na(count)],
                          title = "Patient Count"
                          )
                        
                        report_ret <- leaflet(states_sf) %>%
                          addTiles(urlTemplate = "", attribution = "")%>%
                          addPolygons(
                            fillColor = ~pal(count),
                            fillOpacity = 0.7,
                            color = "#000000",
                            weight = 1,
                            label = ~paste0(state_abbr, ": N=", count),
                            labelOptions = labelOptions(direction = "auto")
                          ) %>%
                          addLabelOnlyMarkers(
                                   data = label_data,
                                   lng = ~X, lat = ~Y,
                                   label = ~label,
                                   labelOptions = labelOptions(
                                       noHide = TRUE,
                                       direction = "center",
                                       textOnly = TRUE,
                                       style = list(
                                           "font-weight" = "bold",
                                           "font-size" = "9px",
                                           "color" = "#000000",
                                           "background-color" = "rgba(230, 210, 255, 0.6)", 
                                           "padding" = "1px 3px",
                                           "border-radius" = "4px",
                                           "box-shadow" = "0 0 2px #888888"
                                           )
                                      )
                                   )%>%
                          addLegend(
                            "bottomright", pal = pal, values = ~count[!is.na(count)],
                            title = "Patient Count"
                          )
                        
                        self$pat_char$plot_pat_geo_report <- report_ret
                        self$pat_char$plot_pat_geo <- app_ret
                      },
                      
                      plot_bsl_char = function(cohort, sel_bsl_char, sel_bsl_strat, sel_bsl_plot, sel_ec_vec){
                        updated_df <- copy(self$sel_df)
                        
                        # create a variable indicate eligibility status
                        # Creating a string of conditions for Eligible and Ineligible
                        elig_conditions <- paste(sel_ec_vec, "== 1", collapse = " & ")
                        inelig_conditions <- paste(sel_ec_vec, "== 0", collapse = " | ")
                        
                        updated_df <- updated_df %>%
                          mutate(plot_elig_status = case_when(
                            eval(parse_expr(elig_conditions)) ~ "Eligible",
                            eval(parse_expr(inelig_conditions)) ~ "Ineligible",
                            TRUE ~ "Unknown"
                          )
                          )
                        
                        
                        ## No strata selected or selected strata is none
                        if(is.null(sel_bsl_strat) || sel_bsl_strat == "None"){
                          
                          ## Strata is none -> unstratified plot
                          if(!is.null(sel_bsl_strat) && sel_bsl_strat == "None"){
                            subset_df <- updated_df[updated_df$plot_elig_status == cohort, ]
                            # histogram
                            ggplot_ret <- ggplot(subset_df, aes(x = .data[[sel_bsl_char]])) +
                              geom_histogram(bins = 10, fill = "#00857C", color = "white", alpha = 0.7) +
                              labs(x = self$bsl_vars_dict$clean_name[which(self$bsl_vars_dict$col_name == sel_bsl_char)], y = "Frequency") +
                              plot_theme() +
                              theme(plot.margin = ggplot2::unit(
                                c(0.1,
                                  0.1,
                                  0.1,
                                  0.1),
                                "in"
                              ))
                            
                            ret <- plotly::ggplotly(ggplot_ret)
                            
                          } else{
                            # make consistent color scheme
                            scheme <- private$gen_color_scheme(as.data.frame(copy(self$sel_df)), sel_bsl_char)
                            color_scheme <- scheme$fill_color
                            text_scheme <- scheme$text_color
                            
                            if(sel_bsl_char == "diag_year") {
                              updated_df$diag_year <- as.numeric(updated_df$diag_year)
                              updated_df <- private$bin_num_var(bin_range = 5, df = updated_df, var = 'diag_year', NULL)$df
                              updated_df <- updated_df %>% 
                                mutate(diag_year_bin = factor(diag_year_bin))
                              
                              sel_bsl_char <- "diag_year_bin"
                              
                              scheme <- private$gen_color_scheme(updated_df, sel_bsl_char)
                              color_scheme <- scheme$fill_color
                              text_scheme <- scheme$text_color
                            }
                            
                            subset_df <- updated_df[updated_df$plot_elig_status == cohort, ]
                            # pie charts
                            if(sel_bsl_plot == "pie_chart"){
                              
                              subset_df <- dplyr::arrange(
                                subset_df,
                                get(sel_bsl_char)
                              )
                              
                              ret <- plotly::plot_ly(subset_df[sel_bsl_char],
                                             labels = ~get(sel_bsl_char),
                                             type = 'pie',
                                             sort = FALSE,
                                             texttemplate = "%{percent:.1%}",
                                             marker = list(colors = color_scheme[subset_df[[sel_bsl_char]]]))
                              
                              # bar plots
                            } else if(sel_bsl_plot == "bar_plot"){
                              all_vals <- unique(as.data.frame(copy(self$sel_df))[[sel_bsl_char]])
                              
                              # Count the frequency of each variable
                              data_counts <- subset_df %>%
                                count(!!sym(sel_bsl_char)) 
                              
                              if(length(all_vals) > nrow(data_counts)){
                                missing_vals <- all_vals[!all_vals %in% data_counts[[sel_bsl_char]]]
                                
                                for(val in missing_vals){
                                  data_counts <- rbind(data_counts, data.frame(setNames(list(c(val)), sel_bsl_char), "n" = c(0)))
                                }
                              }
                              names(color_scheme) <- str_wrap(names(color_scheme), width = 12)
                              
                              if(sel_bsl_char == "diag_year_bin"){
                                x_wrap_width = 5
                              } else{
                                x_wrap_width = 12
                              }
                              
                              ret <- ggplot(data_counts, aes(x = str_wrap(.data[[sel_bsl_char]], width = x_wrap_width), y = n, fill = str_wrap(.data[[sel_bsl_char]], width = 12))) +
                                geom_bar(stat = "identity", width = 0.75) +
                                geom_text(aes(x = str_wrap(.data[[sel_bsl_char]], width = x_wrap_width), label = n, color = .data[[sel_bsl_char]]),
                                          position = position_stack(vjust = 0.5),
                                          size = 4,
                                          fontface = "bold") + 
                                coord_flip() +
                                scale_color_manual(values = text_scheme) +
                                scale_fill_manual(values = color_scheme) +
                                plot_theme() +
                                scale_y_continuous(limits = c(0, NA)) +
                                ylab("Patient Count") +
                                xlab(self$bsl_vars_dict$clean_name[which(self$bsl_vars_dict$col_name == sel_bsl_char)]) +
                                guides(fill = "none", color = "none") + 
                                theme(plot.margin = ggplot2::unit(
                                  c(0.1,
                                    0.1,
                                    0.1,
                                    0.1),
                                  "in"
                                ))
                            }
                          }
                        } else{
                          
                          scheme <- private$gen_color_scheme(updated_df, sel_bsl_strat)
                          color_scheme <- scheme$fill_color
                          text_scheme <- scheme$text_color
                          
                          subset_df <- updated_df[updated_df$plot_elig_status == cohort, ]
                          
                          # stratified violin plots
                          if(length(unique(subset_df[[sel_bsl_strat]])) <= 8){
                            ret <- ggplot(subset_df, aes(y = str_wrap(.data[[sel_bsl_strat]], width = 12), x = .data[[sel_bsl_char]], fill = (.data[[sel_bsl_strat]]))) +
                              geom_vline(
                                xintercept = seq(0, max(subset_df[[sel_bsl_char]], na.rm = TRUE), by = 10), 
                                color = "grey", 
                                size = 0.2, 
                                linetype = "dashed"
                              ) +
                              geom_violin(color = "black") +
                              geom_boxplot(fill = "white", width = 0.15, outlier.shape = NA, lwd = 1.1) +
                              scale_x_continuous(
                                limits = c(10, max(subset_df[[sel_bsl_char]], na.rm = TRUE)), 
                                breaks = seq(10, max(subset_df[[sel_bsl_char]], na.rm = TRUE), by = 10)
                              ) +
                              labs(y = "", x = self$bsl_vars_dict$clean_name[which(self$bsl_vars_dict$col_name == sel_bsl_char)]) +
                              scale_fill_manual(values = color_scheme) +
                              plot_theme() +
                              guides(fill = guide_legend(ncol = 2)) +
                              theme(legend.text = ggplot2::element_text(
                                family = "Source Sans Pro",
                                face = "plain",
                                size = 12,
                                color = "black"
                              ),
                              plot.margin = ggplot2::unit(
                                c(0.1,
                                  0.1,
                                  0.1,
                                  0.1),
                                "in"
                              ))
                            
                          }else{
                            ret <- NULL
                          }
                        }
                        
                        return(ret)
                      },

                      update_tbl_pat_char = function() {
                        
                        # add a column: Augmented (eligible + unknown)
                        df_tmp <- as.data.frame(copy(self$sel_df))[self$sel_df$elig_status != 'Ineligible',]
                        df_tmp$elig_status <- 'Augmented'
                        
                        df_tb <- rbind(as.data.frame(copy(self$sel_df)), df_tmp)
                        
                        baseline_var <- self$sel_bsl_char
                        
                        if("diag_year" %in% self$sel_bsl_char) {
                          df_tb$diag_year <- as.numeric(df_tb$diag_year)
                          ret_lab <- private$bin_num_var(bin_range = 5, df = df_tb, var = 'diag_year', baseline_var)
                          df_tb <- ret_lab$df
                          baseline_var <- ret_lab$baseline_var
                        }
                        
                        # remove geographic column
                        if("geographic" %in% colnames(self$sel_df)){
                          baseline_var <- baseline_var[!baseline_var %in% "geographic"]
                          df_tb <- df_tb %>% select(-geographic)
                        }

                        # make non-numeric baseline characteristic variables factors
                        categ_vars <- df_tb[baseline_var] %>%
                          select(where(is.character))
                        
                        df_tb[colnames(categ_vars)] <- lapply(df_tb[colnames(categ_vars)], factor)
                        
                        for(x in colnames(df_tb)){
                          # handle bin separately bc it wont be in bsl_vars_dict
                          if(x == "diag_year_bin"){
                            table1::label(df_tb$diag_year_bin) <- "Diagnosis year"
                          }else{
                            table1::label(df_tb[[x]]) <- self$bsl_vars_dict$clean_name[which(self$bsl_vars_dict$col_name == x)] 
                          }
                        }
                        
                        tbl_footnote <- "Abbreviations: SD, standard deviation"
                        form <- paste("~", paste(baseline_var, collapse = " + "), "| elig_status")
                        ret <- table1::table1(formula(form), data = df_tb, overall = F, footnote = tbl_footnote)
                        
                        self$pat_char$tbl_pat_char <- ret
                      },
                      
                      
                      ## ------------------------- Survival Outcome Screen -------------------------
                      
                      update_survival_figs = function(elig_prob_cutoff) {
                        pdat <- data.frame(Y=self$elig_mod_fit$Y, self$elig_mod_fit$elig.dat)
                        
                        pdat_w <- pdat[pdat$w_elig > elig_prob_cutoff,]
                        pdat_w$w_elig <- pdat_w$w_elig / mean(pdat_w$w_elig)
                        pdat_w$elig <- "Augmented"
                        pdat0 <- pdat
                        pdat0$w_elig <- 1
                        
                        pdat_all <- bind_rows(pdat0, pdat_w)
                        
                        # I'm not clear on what this line is doing
                        pdat_all$Y <- pdat_all$Y
                        
                        pdat_all$elig <- factor(
                          pdat_all$elig, 
                          levels = c("0", "1", "NA", "Augmented"), 
                          labels = c("Ineligible", "Eligible", "Unknown", "Augmented")
                        )
                        
                        day_scaler <- 30.45
                        
                        if(max(pdat_all$Y[,1])/365.25 > 5){
                          x_breaks <- 365.25
                        } else{
                          x_breaks <- 182.63
                        }
                        
                        # Fit Survival Plots by Eligibility Status #
                        km.fit1 <- survival::survfit(
                          Surv(Y[,1], Y[,2]) ~ elig, 
                          data = pdat_all, 
                          weights = pdat_all$w_elig
                        )
                        names(km.fit1$strata) <- gsub(
                          "elig=", 
                          "", 
                          names(km.fit1$strata)
                        )
                        
                        # Customized survival curves
                        
                        out_items <- survminer::ggsurvplot(km.fit1, data = pdat_all,
                                                           legend.title = "Eligibility Status",
                                                           xscale = "d_m",
                                                           break.x.by = x_breaks,
                                                           risk.table.y.text.col = FALSE,
                                                           xlab = "Time (months)",
                                                           tables.theme = survminer::theme_cleantable() +
                                                             theme(
                                                               text = element_text(color = "black"),
                                                               axis.text = element_text(color = "black"),
                                                               axis.title = element_text(color = "black"),
                                                               legend.text = element_text(color = "black"),
                                                               legend.title = element_text(color = "black")
                                                             ),
                                                           # receiving warnings for "no shared levels found between `names(values)` 
                                                           # of the manual scale and the data's colour/fill values
                                                           ggtheme = plot_theme() +
                                                             theme(
                                                               legend.text = element_text(size = 12),        # control font size
                                                               legend.key.spacing.x = unit(1.5, 'cm')
                                                             ), 
                                                           risk.table = TRUE,
                                                           palette = c(
                                                             "Eligible" = self$color_elig,
                                                             "Ineligible" = self$color_inelig,
                                                             "Unknown" = self$color_unkn,
                                                             "Augmented" = self$color_aug
                                                           )
                                                         ) 
                        surv_plot <- out_items$plot
                        risk_table <- out_items$table
                        
                        out.plot <- ggpubr::ggarrange(surv_plot, risk_table, ncol = 1, heights = c(3, 1), align = "v")
                        
                        medians <- survminer::surv_median(km.fit1)
                        median_tbl <- data.frame("Status" = medians$strata,
                                                 "med_surv_time" = paste0(round(medians$median/day_scaler, digits = 1), 
                                                                          " (", 
                                                                          round(medians$lower/day_scaler, digits = 1),
                                                                          " - ",
                                                                          round(medians$upper/day_scaler, digits = 1),
                                                                          ")"))
                        
                        median_ret <- DT::datatable(median_tbl,
                                                colnames = c("Status", "Median Survival Time (months)"),
                                                escape = FALSE,
                                                options = list(
                                                  dom = 't',
                                                  ordering = FALSE),
                                                rownames = FALSE) 
                        
                        self$surv_outcomes$km_plot_tbl <- out.plot
                        self$surv_outcomes$median_tbl <- median_ret
                      }
                    )
  )
