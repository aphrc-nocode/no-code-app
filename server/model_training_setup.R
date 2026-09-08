
#### ---- trainControlConfiguration for model training ---------------------  ####

model_training_setup_server = function() {

	observeEvent(input$feature_engineering_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				
				## Evaluation metric
				output$model_training_setup_eval_metric = renderUI({
				
					if (isTRUE(rv_ml_ai$task=="Classification")) {
						temp_choices = get_named_choices(input_choices_file, input$change_language,"model_training_setup_eval_metric_choices_classification")
					} else if (isTRUE(rv_ml_ai$task=="Regression")) {
						temp_choices = get_named_choices(input_choices_file, input$change_language,"model_training_setup_eval_metric_choices_regression")	
					} else if (isTRUE(rv_ml_ai$task=="Unsupervised")) {
						temp_choices = get_named_choices(input_choices_file, input$change_language,"model_training_setup_eval_metric_choices_unsupervised")
					}
					selectInput("model_training_setup_eval_metric"
						, label = get_rv_labels("model_training_setup_eval_metric") 
						, choices = temp_choices 
						, selected = temp_choices[[1]]
						, multiple=FALSE
						, width="100%"
					)
				})

				output$model_training_setup_start_clusters = renderUI({
					prettySwitch("model_training_setup_start_clusters_check"
						, get_rv_labels("model_training_setup_start_clusters_check")
						, value = FALSE
					)
				})
				
				output$model_training_setup_include_ensemble = renderUI({
					prettySwitch("model_training_setup_include_ensemble_check"
						, get_rv_labels("model_training_setup_include_ensemble_check")
						, value = FALSE
					)
				})
				
				## Advanced model control
				output$model_training_setup_customize_train_control = renderUI({
					actionButton("customize_train_control_apply"
						, get_rv_labels("model_training_setup_customize_train_control")
						, icon = icon("cog")
						, width="100%"
					)
				})
						
				output$model_training_setup_presetup = renderUI({
					box(title = get_rv_labels("model_training_setup_presetup")
						, status = "success"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = FALSE
						, width = 12
						, fluidRow(
							column(width=4
								, uiOutput("model_training_setup_eval_metric")
							)
							, column(width=4
								, uiOutput("model_training_setup_start_clusters")
							)
							, column(width=4
								, uiOutput("model_training_setup_include_ensemble")
							)
						)
						, uiOutput("model_training_setup_customize_train_control")
					)		
				})

			} else {

				output$model_training_setup_presetup = NULL
				output$model_training_setup_eval_metric = NULL
				updateSelectInput(session, "model_training_setup_eval_metric", selected="")
				output$model_training_setup_start_clusters = NULL
 				updatePrettySwitch(session, inputId="model_training_setup_start_clusters_check" , value=FALSE)
				output$model_training_setup_include_ensemble = NULL
 				updatePrettySwitch(session, inputId="model_training_setup_include_ensemble_check" , value=FALSE)
			}
		} else {	
			output$model_training_setup_presetup = NULL
			output$model_training_setup_eval_metric = NULL
			updateSelectInput(session, "model_training_setup_eval_metric", selected="")
			output$model_training_setup_start_clusters = NULL
			updatePrettySwitch(session, inputId="model_training_setup_start_clusters_check" , value=FALSE)
			output$model_training_setup_include_ensemble = NULL
			updatePrettySwitch(session, inputId="model_training_setup_include_ensemble_check" , value=FALSE)
		}

	})


	## Train control diaglog box
	observeEvent(input$train_control_method_caret, {
		req(input$train_control_method_caret)
		new_value = if (grepl("cv", input$train_control_method_caret)) 5 else 25
		new_min = if (grepl("cv", input$train_control_method_caret)) 3 else 10
		new_max = if (grepl("cv", input$train_control_method_caret)) 10 else 100
	  	updateNumericInput(
			session,
			"train_control_number_caret",
			value = new_value,
			min = new_min,
			max = new_max
	  	)
		
		output$train_control_repeat_caret = renderUI({
			if(isTRUE(grepl("[d_]cv$", input$train_control_method_caret))) {
				numericInput("train_control_repeat_caret"
					, get_rv_labels("train_control_repeat_caret")
					, value = 5
					, min = 1
					, max = 10
				)
			} else {
				updateNumericInput(session, "train_control_repeat_caret", value = NULL)
				NULL
			}
		})
	})

	observeEvent(input$customize_train_control_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					 showModal(
						 modalDialog(
							title = get_rv_labels("model_training_setup_customize_train_control"),
							size = "m",
							footer = tagList(
							  modalButton(get_rv_labels("customize_train_control_apply_cancel")),
							  actionButton("customize_train_control_apply_save", get_rv_labels("customize_train_control_apply_save"))
							),
							tagList(
							  selectInput("train_control_method_caret"
									, get_rv_labels("train_control_method_caret")
									, choices  = get_named_choices(input_choices_file, input$change_language,"train_control_method_caret_choices")	
								)
								, numericInput("train_control_number_caret"
									, get_rv_labels("train_control_number_caret")
									, value = 5
									, min = 3
									, max = 10
								)
							  , uiOutput("train_control_repeat_caret")
							  , selectInput("train_control_search_caret"
									, get_rv_labels("train_control_search_caret")
									, choices  = get_named_choices(input_choices_file, input$change_language,"train_control_search_caret_choices")	
								)
								, prettySwitch("train_control_verbose_caret"
									, get_rv_labels("train_control_verbose_caret")
									, value = FALSE
								)
								, prettySwitch("train_control_save_predictions_caret"
									, get_rv_labels("train_control_save_predictions_caret")
									, value = FALSE
								)
								, prettySwitch("train_control_class_probabilities_caret",
								               get_rv_labels("train_control_class_probabilities_caret"),
								               value = FALSE
								)
								
								, hr()
								
								, prettySwitch(
								  "enable_transfer_learning_access",
								  "Enable Transfer Learning Access",
								  value = FALSE,
								  status = "success"
								)
								
								, conditionalPanel(
								  condition = "input.enable_transfer_learning_access == true",
								  
								  textAreaInput(
								    "transfer_learning_access_users",
								    "Enter usernames/emails to give access",
								    placeholder = "Example: scygu@aphrc.org, letisha@codata.org",
								    width = "100%"
								  )
								)
							)
						 )
					)
				} else {
					NULL # TODO
				}
			} else {
				NULL # TODO
			}
		} else {
			NULL # TODO
		}
	})

	## Update the advance options
	observeEvent(input$customize_train_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					rv_train_control_caret$method = input$train_control_method_caret
					rv_train_control_caret$number = input$train_control_number_caret
					rv_train_control_caret$repeats = input$train_control_repeat_caret
					if (isTRUE(is.null(rv_train_control_caret$repeats))) {
						rv_train_control_caret$repeats = NA
					}
					rv_train_control_caret$search = input$train_control_search_caret
					rv_train_control_caret$verboseIter = input$train_control_verbose_caret
					rv_train_control_caret$savePredictions = input$train_control_save_predictions_caret
					rv_train_control_caret$classProbs = input$train_control_class_probabilities_caret
				}
			}
		}
	  if (isTRUE(input$enable_transfer_learning_access)) {
	    
	    users_raw <- input$transfer_learning_access_users
	    
	    if (!is.null(users_raw) && nzchar(trimws(users_raw))) {
	      
	      allowed_users <- unlist(strsplit(users_raw, "[,;\\n]+"))
	      allowed_users <- trimws(allowed_users)
	      allowed_users <- allowed_users[nzchar(allowed_users)]
	      
	      for (u in allowed_users) {
	        
	        create_dir(paste0(u, "/transfer_learning_board"))
	        
	        access_file <- file.path(getwd(), u, "transfer_learning_access.csv")
	        
	        access_df <- data.frame(
	          username = app_username,
	          shared_by = app_username,
	          shared_at = as.character(Sys.time()),
	          stringsAsFactors = FALSE
	        )
	        
	        if (file.exists(access_file)) {
	          old <- tryCatch(
	            read.csv(access_file, stringsAsFactors = FALSE),
	            error = function(e) NULL
	          )
	          
	          if (!is.null(old)) {
	            access_df <- unique(rbind(old, access_df))
	          }
	        }
	        
	        write.csv(access_df, access_file, row.names = FALSE)
	      }
	    }
	  }
		removeModal()
	})
}


