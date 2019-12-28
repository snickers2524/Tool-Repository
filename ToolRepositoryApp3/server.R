library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(forcats)
library(shinymanager)

source("Sensitive.R")
source("AppFunctions.R")

#########################################################################################################################################################################

shinyServer(function(input, output,session) {
  

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  table<-loadTable()
  


  output$tool_table<-renderDataTable(
    DT::datatable(loadTable()[-1],
                  selection = "single",
                  colnames = c("Manufacturer","Model","Name","Owner","Current User","Location","Notes"),
                  filter = "top")
  )

  
  
  output$x5 = renderPrint({
    cat('Rows on the current page:\n\n')
    cat(input$tool_table_rows_current, sep = ', ')
    cat('\n\nAll rows:\n\n')
    cat(input$tool_table_rows_all, sep = ', ')
    cat('\n\nSelected rows:\n\n')
    # print(humanTime())
    # cat(input$tool_table_rows_selected, sep = ', ')
    # x<-fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf)
    # print(x)
    mandatoryFilled<-logical(length = length(fieldsMandatory))
    print(mandatoryFilled)

  })
  
  # Tool Edit Stuff
  
  
  observe({toggleElement(id = "tooledit",condition = is.null(input$tool_table_rows_selected)==FALSE)
  })
  
  observeEvent(input$cancel_edit,{
    shinyjs::hide("tooledit")
    shinyjs::show("cancel_msg2")
  })

  observeEvent(is.null(input$tool_table_rows_selected)==FALSE,{
    shinyjs::hide("thankyou_msg1")
    
    shinyjs::hide("cancel_msg2")
    shinyjs::show("thankyou_msg2")
    shinyjs::hide("thankyou_msg2")
    shinyjs::show("tool_create")
    
    updateSelectInput(session,"tool_manufacturer_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_manufacturer"))),selected = table[input$tool_table_rows_selected,]$tool_manufacturer)

    updateTextInput(session,"tool_model_edit",value=table[input$tool_table_rows_selected,]$tool_model)

    updateTextInput(session,"tool_name_edit",value=table[input$tool_table_rows_selected,]$tool_name)

    updateSelectInput(session,"tool_owner_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_owner)

    updateSelectInput(session,"tool_user_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_user")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_user)

    updateSelectInput(session,"tool_location_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_location")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_location)

    output$tool_notes_output<-renderText(table[input$tool_table_rows_selected,]$tool_notes)
  })

  observe({toggleElement(id = "tool_manufacturer_edit_other",condition = input$tool_manufacturer_edit=="Other")
  })

  observe({toggleElement(id = "tool_owner_other_edit",condition = input$tool_owner_edit=="Other")
  })

  observe({toggleElement(id = "tool_user_other_edit",condition = input$tool_user_edit=="Other")
  })

  observe({toggleElement(id = "tool_location_other_edit",condition = input$tool_location_edit=="Other")
  })

  observeEvent(input$submit_edit,{
    
    save_UpdatedtoolData(table2="tools_current",data2 = data_newtool_updatetool2())
    
    output$tool_table<-renderDataTable(
      DT::datatable(loadTable()[-1],
                    selection = "single",
                    colnames = c("Manufacturer","Model","Name","Owner","Current User","Location","Notes"),
                    filter = "top")
    )
    
    shinyjs::hide("tooledit")
    shinyjs::show("thankyou_msg2")
    shinyjs::hide("cancel_msg2")
    shinyjs::show("tool_create")
  })
  
  observeEvent(is.null(input$tool_table_rows_selected)==FALSE,{
    hide("form")
    shinyjs::hide("cancel_msg")
    shinyjs::hide("cancel_msg2")
  })
  
  observe({
    mandatoryFilled<-logical(length = length(fieldsMandatory))
    if (!is.null(input$tool_name_edit)){
      if (!is.null(input$tool_name_edit) & input$tool_name_edit!="")
      {
        mandatoryFilled[[1]]=TRUE
      }
    }
    
    if (!is.null(input$tool_owner_edit)){
      if (input$tool_owner_edit != "Select One" & input$tool_owner_edit != "Other" ){
        mandatoryFilled[[2]]=TRUE
      }
      
      else if (input$tool_owner_edit == "Other" & (!is.null(input$tool_owner_other_edit) & input$tool_owner_other_edit!="")  ){
        mandatoryFilled[[2]]=TRUE
      }
    }
    
    if (!is.null(input$tool_user_edit)){
      if (input$tool_user_edit != "Select One" & input$tool_user_edit != "Other" ){
        mandatoryFilled[[3]]=TRUE
      }
      
      else if (input$tool_user_edit == "Other" & (!is.null(input$tool_user_other_edit) & input$tool_user_other_edit!="")  ){
        mandatoryFilled[[3]]=TRUE
      }
    }
    
    if (!is.null(input$tool_location)){
      if (input$tool_location_edit != "Select One" & input$tool_location_edit != "Other" ){
        mandatoryFilled[[4]]=TRUE
      }
      
      else if (input$tool_location_edit == "Other" & (!is.null(input$tool_location_other_edit) & input$tool_location_other_edit !="")  ){
        mandatoryFilled[[4]]=TRUE
      }
    }
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit_edit", condition = mandatoryFilled)
  })
  
  
  # New Tool Stuff
  observeEvent(input$tool_create,{
    shinyjs::show("form")
    shinyjs::hide("tooledit")
    shinyjs::hide("tool_create")
    shinyjs::hide("cancel_msg")
    shinyjs::hide("cancel_msg2")
    shinyjs::hide("thankyou_msg2")
    
    updateSelectInput(session,"tool_owner",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_user",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_user")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_location",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_location")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_manufacturer",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_manufacturer")),"Other",after=Inf),selected = "Select One")
  })
  
  observeEvent(input$cancel,{
    shinyjs::hide("form")
    shinyjs::show("tool_create")
    shinyjs::show("cancel_msg")
    
  })
  
  
 
  # 
  observe({toggleElement(id = "tool_owner_other",condition = input$tool_owner=="Other")
  })

  observe({toggleElement(id = "tool_user_other",condition = input$tool_user=="Other")
  })

  observe({toggleElement(id = "tool_location_other",condition = input$tool_location=="Other")
  })

  observe({toggleElement(id = "tool_manufacturer_other",condition = input$tool_manufacturer=="Other")
  })
  
 
  
  # Toggle Submit Button on Tool Creation
  
  observe({
    mandatoryFilled<-logical(length = length(fieldsMandatory))
    if (!is.null(input$tool_name)){
      if (!is.null(input$tool_name) & input$tool_name!="")
      {
        mandatoryFilled[[1]]=TRUE
      }
    }
    
    if (!is.null(input$tool_owner)){
      if (input$tool_owner != "Select One" & input$tool_owner != "Other" ){
        mandatoryFilled[[2]]=TRUE
      }
  
      else if (input$tool_owner == "Other" & (!is.null(input$tool_owner_other) & input$tool_owner_other!="")  ){
        mandatoryFilled[[2]]=TRUE
      }
    }

    if (!is.null(input$tool_user)){
      if (input$tool_user != "Select One" & input$tool_user != "Other" ){
        mandatoryFilled[[3]]=TRUE
      }
  
      else if (input$tool_user == "Other" & (!is.null(input$tool_user_other) & input$tool_user_other!="")  ){
        mandatoryFilled[[3]]=TRUE
      }
    }
    
    if (!is.null(input$tool_location)){
      if (input$tool_location != "Select One" & input$tool_location != "Other" ){
        mandatoryFilled[[4]]=TRUE
      }
  
      else if (input$tool_location == "Other" & (!is.null(input$tool_location_other) & input$tool_location_other !="")  ){
        mandatoryFilled[[4]]=TRUE
      }
    }
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  

  

 
  
  observeEvent(input$submit,{
    save_newtool("tools","tools_current",data_newtool(),data_newtool_updatetool())
    
    output$tool_table<-renderDataTable(
      DT::datatable(loadTable()[-1],
                    selection = "single",
                    colnames = c("Manufacturer","Model","Name","Owner","Current User","Location","Notes"),
                    filter = "top")
    )
    shinyjs::hide("submit")
    shinyjs::hide("form")
    shinyjs::reset("form")
    shinyjs::show("thankyou_msg1")
    
    updateSelectInput(session,"tool_manufacturer_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_manufacturer"))),selected = table[input$tool_table_rows_selected,]$tool_manufacturer)
    
    updateTextInput(session,"tool_model_edit",value=table[input$tool_table_rows_selected,]$tool_model)
    
    updateTextInput(session,"tool_name_edit",value=table[input$tool_table_rows_selected,]$tool_name)
    
    updateSelectInput(session,"tool_owner_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_owner)
    
    updateSelectInput(session,"tool_user_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_user")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_user)
    
    updateSelectInput(session,"tool_location_edit",choices = fct_relevel(fct_c(factor("Other"),cols_specific(table,"tool_location")),"Other",after=Inf),selected = table[input$tool_table_rows_selected,]$tool_location)
    
    output$tool_notes_output<-renderText(table[input$tool_table_rows_selected,]$tool_notes)
    
  })

  observeEvent(input$submit_another_tool,{
    shinyjs::hide("thankyou_msg1")
    shinyjs::show("form")

    shinyjs::show("submit")

  })
  
  data_newtool<-reactive({
    data <- sapply(fields_newtool, function(x) input[[x]])
    if (data[[4]]=="Other"){
      data[[4]]=input$tool_owner_other
    }
    if (data[[2]]=="Other"){
      data[[2]]=input$tool_manufacturer_other
    }
    if (data[[2]]=="Select One"){
      data[[2]]="N/A"
    }
    if (data[[4]]=="Select One"){
      data[[4]]="N/A"
    }
    data
  })
  
 
  
  data_newtool_updatetool<-reactive({
    data <- sapply(fields_updatetool, function(x) input[[x]])
    data<-c(data,tool_date = humanTime())
    if (data[[1]]=="Other"){
      data[[1]]=input$tool_location_other
    }
    if (data[[2]]=="Other"){
      data[[2]]=input$tool_user_other
    } 
    if (data[[1]]=="Select One"){
      data[[1]]="N/A"
    }
    if (data[[1]]=="Select One"){
      data[[1]]="N/A"
    }
    
    data
  })
  
  
  data_newtool_updatetool2<-reactive({
    # if (is.null(input$tool_notes_edit)){
    #   fields_updatetool2<-c("tool_location_edit","tool_user_edit")
    # } else {fields_updatetool2<-c("tool_location_edit","tool_user_edit","tool_notes_edit")}
    data <- sapply(fields_updatetool2, function(x) input[[x]])
    data<-c(data,tool_date = humanTime(),tool_ID = table$tool_ID[input$tool_table_rows_selected])
    if (data[[1]]=="Other"){
      data[[1]]=input$tool_location_other_edit
    }
    if (data[[2]]=="Other"){
      data[[2]]=input$tool_user_other_edit
    }
    if (data[[1]]=="Select One"){
      data[[1]]="N/A"
    }
    if (data[[1]]=="Select One"){
      data[[1]]="N/A"
    }
    data
  })
  
  output$make<-renderText({
    input$tool_model
  })
  
  
  
  
  
  
})