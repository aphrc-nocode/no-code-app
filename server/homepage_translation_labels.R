# Server/homepage_translation_labels.R

register_homepage_labels <- function(output, session, get_rv_labels){
  
  output$homepage_title <- renderText(get_rv_labels("homepage_title"))
  output$homepage_intro <- renderText(get_rv_labels("homepage_intro"))
  
  output$homepage_more_details_link <- renderUI({
    txt <- get_rv_labels("homepage_more_details_link_text")
    url <- get_rv_labels("homepage_more_details_url")
    if (is.null(txt) || is.null(url) || !nzchar(txt) || !nzchar(url)) return(NULL)
    
    tags$a(
      href   = url,
      target = "_blank",
      class  = "aphrc-link",
      txt
    )
  })
  
  output$homepage_objectives_kicker <- renderText(get_rv_labels("homepage_objectives_kicker"))
  output$homepage_objectives_title  <- renderText(get_rv_labels("homepage_objectives_title"))
  
  output$homepage_more_details_link <- renderUI({
    txt <- get_rv_labels("homepage_more_details_link_text")
    url <- get_rv_labels("homepage_more_details_url")
    
    # If either is missing, don't render a broken link
    if (is.null(txt) || is.null(url) || !nzchar(txt) || !nzchar(url)) {
      return(NULL)
    }
    
    tags$a(
      href   = url,
      target = "_blank",
      class  = "aphrc-link",
      txt
    )
  })
  
  output$homepage_obj1_title   <- renderText(get_rv_labels("homepage_obj1_title"))
  output$homepage_obj1_bullet1 <- renderText(get_rv_labels("homepage_obj1_bullet1"))
  output$homepage_obj1_bullet2 <- renderText(get_rv_labels("homepage_obj1_bullet2"))
  
  output$homepage_obj2_title   <- renderText(get_rv_labels("homepage_obj2_title"))
  output$homepage_obj2_bullet1 <- renderText(get_rv_labels("homepage_obj2_bullet1"))
  output$homepage_obj2_bullet2 <- renderText(get_rv_labels("homepage_obj2_bullet2"))
  
  output$homepage_obj3_title   <- renderText(get_rv_labels("homepage_obj3_title"))
  output$homepage_obj3_bullet1 <- renderText(get_rv_labels("homepage_obj3_bullet1"))
  output$homepage_obj3_bullet2 <- renderText(get_rv_labels("homepage_obj3_bullet2"))
  
  output$homepage_platform_overview_caption <- renderText(get_rv_labels("homepage_platform_overview_caption"))
  
  output$homepage_components_title <- renderText(get_rv_labels("homepage_components_title"))
  output$homepage_comp1_title      <- renderText(get_rv_labels("homepage_comp1_title"))
  output$homepage_comp1_desc       <- renderText(get_rv_labels("homepage_comp1_desc"))
  output$homepage_comp2_title      <- renderText(get_rv_labels("homepage_comp2_title"))
  output$homepage_comp2_desc       <- renderText(get_rv_labels("homepage_comp2_desc"))
  output$homepage_comp3_title      <- renderText(get_rv_labels("homepage_comp3_title"))
  output$homepage_comp3_desc       <- renderText(get_rv_labels("homepage_comp3_desc"))
  
  output$homepage_end_to_end_workflow_caption <- renderText(get_rv_labels("homepage_end_to_end_workflow_caption"))
}