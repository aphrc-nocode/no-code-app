COMMENTS:

**All**

- Make sure the codes are properly commented (Especially John)
- Good format

```
#### ----- Hearder 1 ------------------------------- ####
##### ----- Hearder 2 ------------------------------- ####
```

- Proper code identation (Mike, Pauline & Letisha). Please make sure you have brackets in the right places
- Good format

```
(
	(
		my code
	)
)
```

- Make sure there conditions for the UI to show only when necessary. E.g.,
	- Plotting options should only show when the data is selected in the Manage Tab (John & Steve & Ouseman). Sam e applies to ML models relying on the data.
	- All the OMOP options should show only when there's a connection to the BD, otherwise, we only need to see connect to DB ..... (Letisha/Mike/Pauline)
	- Every about pycaret/AutoML should on show when the option is selected and refresh when other option is selected
- **NO TEXT** should be hard coded in the R code e.g
	- Letisha: The intructions you have in the UI
	- Pauline: All the text you have in the viz section
	- John: All select options have to be in the labelling file
	- Alvis/Simon: Every text in your section
	- Mike: There are a few areas you need to clear


**Mike, Pauline and Letisha**
- All DB connection for all the OMOP stuff MUST be a single connection. The current setup with every OMOP having a connection box is weired. 
- We agreed that all OMOP stuff should come just before Manage data.

**Ouseman & Mike**
- Fixe the DB options in `server/database_integration.R`

**Ouseman**

- Remove all the ChatGPT text as shown below
- Remove hard coded stuff in the serve.R
- Use the labelling file for all the text in the app
- Provide a documentation on how to start the deep learning API on the README


```
Warning: Closing open result set, pending rows
[1] "New dataset loaded : Reset PyCaret pipeline"
Warning: call dbDisconnect() when finished working with a connection
[1] "✅ Module deploy_model_server lancé"
Warning: Closing open result set, pending rows
[1] "🔍 getwd(): /mnt/d/projects/no-code-app"
[1] "❌ Dossier 'models' non trouvé"
[1] "📋 Modèles affichés dans le selectInput : "
[1] "✅ Module deploy_model_server lancé"
Warning: Closing open result set, pending rows
[1] "🔍 getwd(): /mnt/d/projects/no-code-app"
[1] "❌ Dossier 'models' non trouvé"
[1] "📋 Modèles affichés dans le selectInput : "
[1] "✅ Module deploy_model_server lancé"
Warning: Closing open result set, pending rows
[1] "🔍 getwd(): /mnt/d/projects/no-code-app"
[1] "❌ Dossier 'models' non trouvé"
[1] "📋 Modèles affichés dans le selectInput : "
[1] "✅ Module deploy_model_server lancé"
Warning: Closing open result set, pending rows
[1] "🔍 getwd(): /mnt/d/projects/no-code-app"
[1] "❌ Dossier 'models' non trouvé"
[1] "📋 Modèles affichés dans le selectInput : "
[1] "✅ Module deploy_model_server lancé"
Warning: Closing open result set, pending rows
[1] "🔍 getwd(): /mnt/d/projects/no-code-app"
[1] "❌ Dossier 'models' non trouvé"
[1] "📋 Modèles affichés dans le selectInput : "

```

- No `observe/observerEvent` code should be in `server.R`

```
  observe({
    req(!is.null(rv_ml_ai$modelling_framework))  # Check if value exist

    if (tolower(rv_ml_ai$modelling_framework) == "pycaret") {
      output$automl_module_ui <- renderUI({
        automl_ui("automl_module")
      })
    } else {
      output$automl_module_ui <- renderUI({
        h4("")
      })
    }
  })

  # Deployment
  output$deploy_model_module_ui <- renderUI({
    deploy_model_ui("deploy_model_module")
  })

```

**John***

- AVOID writing hard codes on the server.R
- Properly comment the codes.
- NO hard-coding of text.
- Logout not working.
- Weired splash on login


```
  USER <- login::login_server(
    id = app_login_config$APP_ID,
    db_conn = DBI::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
    emailer = emayili_emailer(
      email_host = app_login_config$email_host,
      email_port = app_login_config $email_port,
      email_username = app_login_config$email_username,
      email_password = app_login_config$email_password,
      from_email = app_login_config$from_email
    ),
    additional_fields = c('first_name' = 'First Name',
                          'last_name' = 'Last Name'),
    cookie_name = "aphrc1",
    cookie_password = "aphrcpass1"
  )

  output$userName <- renderText({ paste0(USER$first_name," ", USER$last_name) })

  observeEvent(input$logoutID, {
    shinyjs::runjs("document.cookie = 'aphrc=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;'")
    session$reload()
  })

  observeEvent(input$show_login, {
    shinyjs::show("login_form")
    shinyjs::hide("signup_form")
    shinyjs::hide("reset_form")
    output$form_title <- renderText("APHRC Nocode Platform")
  })

  observeEvent(input$show_signup, {
    shinyjs::hide("login_form")
    shinyjs::show("signup_form")
    shinyjs::hide("reset_form")
    output$form_title <- renderText("Create an Account")
  })

  observeEvent(input$show_reset, {
    shinyjs::hide("login_form")
    shinyjs::hide("signup_form")
    shinyjs::show("reset_form")
    output$form_title <- renderText("Reset Password")
  })

- All these themes should go the labeling files 
  if(isTRUE(!is.null(rv_current$working_df))){
    output$user_ggthemes <- renderUI({
      selectInput("ggplot_theme", "Choose ggplot2 theme:",
                  choices = theme_choices <- c(
                    "Default (theme_grey)" = "theme_grey",
                    "Black & White (theme_bw)" = "theme_bw",
                    "Classic (theme_classic)" = "theme_classic",
                    "Minimal (theme_minimal)" = "theme_minimal",
                    "Light (theme_light)" = "theme_light",
                    "Dark (theme_dark)" = "theme_dark",
                    "Linedraw (theme_linedraw)" = "theme_linedraw",
                    "Void (theme_void)" = "theme_void",
                    "Test (theme_test)" = "theme_test"
                  ), selected = "Minimal (theme_minimal)")
    })
  }else{
    output$user_ggthemes <- NULL
  }
```


**Mike/Pauline/Letisha**

- The indentation is too bad.
- The sections are all over the palce
- The labeling is hard coded
- Some items SHOULD only show when the connection is sucessful

**Mike**

- What's the empty else in ui/upload_form.R

**Elvis/Samson**

- Hard coded labeling 
- Provide a documentation on how to start the deep learning API on the README

