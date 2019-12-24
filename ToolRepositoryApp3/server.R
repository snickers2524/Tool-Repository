library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(forcats)


create_connection<-function(){
  library(RMySQL)
  
  
  conn = dbConnect(MySQL(), 
                   user='admin', 
                   password='Pumpernickle4054', 
                   dbname='ToolRepository', 
                   host='vickarsfamily.cmppj2geoch9.us-east-1.rds.amazonaws.com')
  
  return(conn)
}

save_newtool<-function(table1,table2,data1,data2){
  print(data1)
  print(data1[[4]])
  if (data1[[4]]=="other"){
    data1[[4]]=input$tool_owner_other
  }
  
  con<-create_connection()
  
  
  
  query1<- sprintf("insert into %s (%s) values ('%s')",
                   table1,
                   paste(names(data1),collapse = ", "),
                   paste(data1,collapse = "','"))
  
  query1<-str_c(query1,";",sep="")
  
  query2<- sprintf("insert into %s (tool_ID, %s) values ((select tool_ID from tools order by tool_ID desc limit 1), '%s')",
                   table2,
                   paste(names(data2),collapse = ", "),
                   paste(data2,collapse = "','"))
  

  dbSendQuery(con, query1)
  dbSendQuery(con, query2)
  dbDisconnect(con)
  
}

humanTime <- function(){x<-ymd_hms(Sys.time());x<-toString(x);return(x)}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

loadTable<-function(){
  conn<-create_connection()
  
  query <- "SELECT 
    t3.tool_ID,
    t3.tool_manufacturer,
    t3.tool_model,
    t3.tool_name,
    t3.tool_owner,
    t4.tool_user,
    t4.tool_location,
    t4.tool_notes
FROM
    tools AS t3
        CROSS JOIN
    (SELECT 
        t1.*, t2.tool_notes
    FROM
        (SELECT 
        tc.tool_location, tc.tool_user, tc.tool_date, tc.tool_ID
    FROM
        tools_current AS tc
    INNER JOIN (SELECT 
        MAX(tool_date) AS tool_date, tool_ID
    FROM
        tools_current
    GROUP BY tool_ID) AS t ON tc.tool_date = t.tool_date
        AND tc.tool_ID = t.tool_ID) AS t1
    INNER JOIN (SELECT 
        GROUP_CONCAT(tool_notes
                SEPARATOR ' - ') AS tool_notes,
            tool_ID
    FROM
        tools_current
    GROUP BY tool_ID) AS t2 ON t1.tool_ID = t2.tool_ID) AS t4 ON t3.tool_ID = t4.tool_ID;"
  data<-dbGetQuery(conn, query)
  data
}

cols_specific <- function(table,col){
  x<-table %>% select(col) %>% distinct()
  x<-c(x[[1:length(x)]])
  x<-factor(x)
  return(x)
}

fieldsMandatory <- c("tool_name","tool_owner","tool_user","tool_location")
form_fields<-c("tool_location","too_model","tool_name","tool_owner","tool_user","tool_location")



fields_newtool<-c("tool_name","tool_manufacturer","tool_model","tool_owner")
fields_updatetool<-c("tool_location","tool_user","tool_notes")

fields_newtool2<-c("tool_name_edit","tool_manufacturer_edit","tool_model_edit","tool_owner_edit")
fields_updatetool2<-c("tool_location_edit","tool_user_edit","tool_notes_edit")




save_UpdatedtoolData<-function(table1 = "tools",table2="tools_current",data2){
  
 
  
  con<-create_connection()
  
  
  
  # query1<- sprintf("insert into %s (%s) values ('%s')",
  #                  table1,
  #                  paste(names(data1),collapse = ", "),
  #                  paste(data1,collapse = "','"))
  # 
  # query1<-str_c(query1,";",sep="")
  update_fieldname<-fields_updatetool
  update_fieldname<-c(update_fieldname,"tool_date","tool_ID")
  print(update_fieldname)
  
  query2<- sprintf("insert into %s (%s) values ('%s')",
                   table2,
                   paste(update_fieldname,collapse = ", "),
                   paste(data2,collapse = "','"))
  
  print(query2)
  
  
  # dbSendQuery(con, query1)
  dbSendQuery(con, query2)
  dbDisconnect(con)

}

#########################################################################################################################################################################

shinyServer(function(input, output,session) {
  table<-loadTable()
  print(fields_updatetool2)
  
  # updateSelectInput(session,"tool_owner",selected = cols_specific(table,"tool_owner"))
  
  output$tool_table<-renderDataTable(
    DT::datatable(table[-1],
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

  })
  
  # Tool Edit Stuff
  
  observe({toggleElement(id = "tooledit",condition = is.null(input$tool_table_rows_selected)==FALSE)
  })
  
  observeEvent(is.null(input$tool_table_rows_selected)==FALSE,{
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
    hide("tooledit")
    show("thankyou_msg2")
  })

  
  # New Tool Stuff
  
  
  observeEvent(input$tool_create, {
    updateSelectInput(session,"tool_owner",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_owner")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_user",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_user")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_location",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_location")),"Other",after=Inf),selected = "Select One")
    updateSelectInput(session,"tool_manufacturer",choices = fct_relevel(fct_c(factor("Select One"),factor("Other"),cols_specific(table,"tool_manufacturer")),"Other",after=Inf),selected = "Select One")
    shinyjs::show("form")
    hide("tool_create")
  })
  
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

  
  # observeEvent(input$cancel,{
  #   hide("form")
  #   show("tool_create")
  # 
  # })
  # 
  # observeEvent(input$cancel,{
  #   show("tool_create")
  # })
  
  observeEvent(input$submit,{
    save_newtool("tools","tools_current",data_newtool(),data_newtool_updatetool())
    hide("form")
    reset("form")
    show("test")
  })

  # observeEvent(input$submit_another_tool,{
  #   hide("thankyou_msg1")
  #   show("form")
  # })
  
  data_newtool<-reactive({
    data <- sapply(fields_newtool, function(x) input[[x]])
    if (data[[4]]=="Other"){
      data[[4]]=input$tool_owner_other
    }
    if (data[[2]]=="Other"){
      data[[2]]=input$tool_manufacturer_other
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
    data
  })
  
  output$make<-renderText({
    input$tool_model
  })
  
  
  
  
  
  
})