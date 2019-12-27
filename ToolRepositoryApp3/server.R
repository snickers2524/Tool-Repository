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

  
  
  # output$x5 = renderPrint({
  #   cat('Rows on the current page:\n\n')
  #   cat(input$tool_table_rows_current, sep = ', ')
  #   cat('\n\nAll rows:\n\n')
  #   cat(input$tool_table_rows_all, sep = ', ')
  #   cat('\n\nSelected rows:\n\n')
  #   # print(humanTime())
  #   # cat(input$tool_table_rows_selected, sep = ', ')
  #   # x<-fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf)
  #   # print(x)
  #   print(mandatoryFilled(input))
  # 
  # })
  
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
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
  
    
    # enable/disable the submit button
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