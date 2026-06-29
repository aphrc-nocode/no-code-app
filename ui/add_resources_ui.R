# ui/add_resources_ui.R

feedback_form_ui <- function() {
  tabItem(
    tabName = "feedbackForm",
    
    div(
      style = "padding: 25px;",
      
      div(
        style = "
             background: #ffffff;
             border-radius: 20px;
             padding: 28px;
             box-shadow: 0 10px 30px rgba(0,0,0,0.08);
             border: 1px solid #e8eef7;
             width: 100%;
             max-width: none;
             ",
        
        uiOutput("feedback_form_title_ui"),
        uiOutput("feedback_form_description_ui"),
        
        tags$iframe(
          src = "https://docs.google.com/forms/d/e/1FAIpQLSccxQ9wY3O0nh0vCT6vaKZsmQnzK_Dm6XZMvYnzyoXxRPM9vQ/viewform?embedded=true",
          width = "100%",
          height = "2200px",
          style = "
               display: block;
               width: 100%;
               min-width: 100%;
               border: none;
               border-radius: 16px;
               background: #ffffff;
               ",
          loading = "lazy"
        ),
        
        uiOutput("feedback_form_button_ui")
      )
    )
  )
}


user_manual_ui <- function() {
  tabItem(
    tabName = "userManual",
    
    div(
      style = "padding: 25px;",
      
      div(
        style = "
          background: #ffffff;
          border-radius: 20px;
          padding: 28px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.08);
          border: 1px solid #e8eef7;
        ",
        
        uiOutput("user_manual_title_ui"),
        uiOutput("user_manual_description_ui"),
        uiOutput("user_manual_button_ui")
      )
    )
  )
}


add_resources_translation <- function() {
  
  output$feedback_form_title_ui <- renderUI({
    h2(
      icon("comment-dots"),
      get_rv_labels("feedback_form_title"),
      style = "font-weight: 800; color: #243447; margin-top: 0;"
    )
  })
  
  output$feedback_form_description_ui <- renderUI({
    p(
      get_rv_labels("feedback_form_description"),
      style = "font-size: 16px; color: #5f6b7a;"
    )
  })
  
  output$feedback_form_button_ui <- renderUI({
    tags$a(
      get_rv_labels("open_feedback_form_new_tab"),
      href = "https://docs.google.com/forms/d/e/1FAIpQLSccxQ9wY3O0nh0vCT6vaKZsmQnzK_Dm6XZMvYnzyoXxRPM9vQ/viewform",
      target = "_blank",
      style = "
        display:inline-block;
        margin-top:18px;
        padding:12px 20px;
        border-radius:12px;
        background:#2c7be5;
        color:white;
        text-decoration:none;
        font-weight:700;
      "
    )
  })
  
  output$user_manual_title_ui <- renderUI({
    h2(
      icon("book-open"),
      get_rv_labels("user_manual_title"),
      style = "font-weight: 800; color: #243447; margin-top: 0;"
    )
  })
  
  output$user_manual_description_ui <- renderUI({
    p(
      get_rv_labels("user_manual_description"),
      style = "font-size: 16px; color: #5f6b7a;"
    )
  })
  
  output$user_manual_button_ui <- renderUI({
    tags$a(
      get_rv_labels("open_user_manual"),
      href = "#",
      target = "_blank",
      style = "
        display:inline-block;
        margin-top:18px;
        padding:12px 20px;
        border-radius:12px;
        background:#2c7be5;
        color:white;
        text-decoration:none;
        font-weight:700;
      "
    )
  })
}

add_resources_translation <- function(output) {
  output$feedback_form_title_ui <- renderUI({
    h2(icon("comment-dots"), get_rv_labels("feedback_form_title"),
       style = "font-weight: 800; color: #243447; margin-top: 0;")
  })
  
  output$feedback_form_description_ui <- renderUI({
    p(get_rv_labels("feedback_form_description"),
      style = "font-size: 16px; color: #5f6b7a;")
  })
  
  output$feedback_form_button_ui <- renderUI({
    tags$a(
      get_rv_labels("open_feedback_form_new_tab"),
      href = "https://docs.google.com/forms/d/e/1FAIpQLSccxQ9wY3O0nh0vCT6vaKZsmQnzK_Dm6XZMvYnzyoXxRPM9vQ/viewform",
      target = "_blank",
      style = "display:inline-block;margin-top:18px;padding:12px 20px;border-radius:12px;background:#2c7be5;color:white;text-decoration:none;font-weight:700;"
    )
  })
  
  output$user_manual_title_ui <- renderUI({
    h2(icon("book-open"), get_rv_labels("user_manual_title"),
       style = "font-weight: 800; color: #243447; margin-top: 0;")
  })
  
  output$user_manual_description_ui <- renderUI({
    p(get_rv_labels("user_manual_description"),
      style = "font-size: 16px; color: #5f6b7a;")
  })
  
  output$user_manual_button_ui <- renderUI({
    tags$a(
      get_rv_labels("open_user_manual"),
      href = "#",
      target = "_blank",
      style = "display:inline-block;margin-top:18px;padding:12px 20px;border-radius:12px;background:#2c7be5;color:white;text-decoration:none;font-weight:700;"
    )
  })
}