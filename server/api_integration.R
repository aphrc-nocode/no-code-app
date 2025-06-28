#--- API Integration 

api_integration_server <-function(){
  
  
  
  output$api_data_str <- renderPrint({ 
    if(isTRUE(input$api_type == "DASSA")){
      if(isTRUE(input$dassa_api_type == "Public Data")){
        print(paste("Dataset str : ",input$public_data_list))
        
      }
      
    }
    
  })
  
  
}