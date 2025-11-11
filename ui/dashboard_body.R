## Load UI files
source("ui/sourcedata_ui.R")
source("ui/overview_ui.R")
source("ui/explore_ui.R")
source("ui/transform_ui.R")
source("ui/combinedata_ui.R")
source("ui/summarize_automatic_ui.R")
source("ui/summarize_custom_ui.R", local = TRUE)
source("ui/research_questions_ui.R")
source("ui/setup_models_ui.R")
source("ui/feature_engineering_ui.R")
source("ui/evidence_quality_ui.R")
source("ui/train_all_model_ui.R")
source("ui/validate_deploy_model_ui.R")
source("ui/predict_classify_ui.R")
source("ui/deeplearning_ui.R")
source("ui/cohort_constructor_ui.R")
source("ui/achilles_ui.R")
source("ui/feature_extraction_ui.R")
source("ui/omop_visualizations_ui.R")
source("ui/add_resources_ui.R")
source("ui/deploy_model_ui.R", local=TRUE)
source("ui/predict_pycaret_ui.R", local = TRUE)

#### ---- Change language --------------------------------------------
source("server/change_language.R", local = TRUE)

#### Extracts language specific labels
get_rv_labels = function(var) {
 get_rv_labels_base(rv_lang$labelling_file_df, var)
}


aphrcBody <- dashboardBody(
	headertag,
	useShinyjs(),
	useAttendant(),
	# useWaiter(), #FIXME: Use better one
	theme = appTheme,
	# -- Handler JS pour capter les clics 'Deploy' dans la DataTable du module --
	# dashboard_body (une seule fois, avant tabItems)
	tags$head(
		tags$script(HTML("
		Shiny.addCustomMessageHandler('bindDeployBtn', function(msg) {
			var ns = msg.ns;

			// Nettoie d'abord les handlers (évite doublons)
			$(document).off('click', '.action-deploy');
			$(document).off('click', '.action-stop');

			// ---- Deploy ----
			$(document).on('click', '.action-deploy', function(){
			var $btn = $(this);
			var mid  = $btn.data('model');

			// Feedback visuel immédiat
			$btn.prop('disabled', true).text('Deploying...');

			// Envoi à Shiny
			Shiny.setInputValue(ns + 'deploy_model_id', mid, {priority: 'event'});
			});

			// ---- Stop ----
			$(document).on('click', '.action-stop', function(){
			var $btn = $(this);
			var mid  = $btn.data('model');

			$btn.prop('disabled', true).text('Stopping...');

			Shiny.setInputValue(ns + 'stop_model_id', mid, {priority: 'event'});
			});
		});
		Shiny.addCustomMessageHandler('openSwagger', function(msg) {
		if (msg && msg.url) { window.open(msg.url, '_blank'); }
		});
		"))
	),

	tabItems(
		tabItem(tabName = "homePage"
			, class = "active"
			, fluidRow()
		)

		## Source data
		, sourcedata_ui()

		## Data overview
		, overview_ui()
		
		## Explore
		, explore_ui()

		## Transform data
		, transform_ui()

		## Combine data
		, combinedata_ui()
			
		## Summarize data automatic
		, summarize_automatic_ui()
		
		## Summarize data customize
		, summarize_custom_ui()

		## Research questions
		, research_questions_ui()

		## Setup models
		, setup_models_ui() 

		## Feature engineering
		, feature_engineering_ui()

		## Evidence quality
		, evidence_quality_ui()
		
		## Train models: FIXME: NOW - Remove text in the train_model_ui
		, train_all_model_ui()
		
		## Validate and deploy models
		, validate_deploy_model_ui()
		
		## Prediction UI
		, predict_classify_ui()
				  

		## Deep learning UI
		, deeplearning_ui()
		
		## Cohort constructor
		, cohort_constructor_ui()

		## Achilles
		, achilles_ui()

		## Feature extraction
		, feature_extraction_ui()

		## OMOP visualization
		, omop_visualizations_ui()

		## OMOP resources
		, add_resources_ui()

	)
)
