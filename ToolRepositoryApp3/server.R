library(shiny)
library(readr)
library(dplyr)
library(lubridate)

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
    t.tool_manufacturer,
    t.tool_model,
    t.tool_name,
    t.tool_owner,
    tc.tool_user,
    tc.tool_location,
    tc.tool_ID,
    group_concat(tc.tool_notes separator ' - ') as tool_notes
FROM
    tools AS t
        INNER JOIN
    tools_current AS tc ON t.tool_ID = tc.tool_ID
group by t.tool_ID
order by tc.tool_date;"
  data<-dbGetQuery(conn, query)
  data
}

cols_specific <- function(table,col){
  x<-table %>% select(col) %>% distinct()
  x<-c(x[[1:length(x)]],"other")
  return(x)
}

fieldsMandatory <- c("tool_name","tool_owner","tool_user","tool_location")
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

#################################################################################################

shinyServer(function(input, output,session) {
  table<-loadTable()
  print(fields_updatetool2)
  
  updateSelectInput(session,"tool_owner",selected = cols_specific(table,"tool_owner"))
  
  output$tool_table<-renderDataTable(
    DT::datatable(table[-7],
                  selection = "single",
                  # colnames = c("Manufacturer","Model","Name","Owner","Current User","Location","Notes"),
                  filter = "top")
                  )
  
  output$x5 = renderPrint({
    cat('Rows on the current page:\n\n')
    cat(input$tool_table_rows_current, sep = ', ')
    cat('\n\nAll rows:\n\n')
    cat(input$tool_table_rows_all, sep = ', ')
    cat('\n\nSelected rows:\n\n')
    print(humanTime())
    cat(input$tool_table_rows_selected, sep = ', ')
  })
  
  # Tool Edit Stuff
  
  observe({toggleElement(id = "tooledit",condition = is.null(input$tool_table_rows_selected)==FALSE)
  })
  
  observeEvent(is.null(input$tool_table_rows_selected)==FALSE,{
    updateTextInput(session,"tool_manufacturer_edit",value=table[input$tool_table_rows_selected,]$tool_manufacturer)
    updateTextInput(session,"tool_model_edit",value=table[input$tool_table_rows_selected,]$tool_model)
    updateTextInput(session,"tool_name_edit",value=table[input$tool_table_rows_selected,]$tool_name)
    updateSelectInput(session,"tool_owner_edit",choices = cols_specific(table,"tool_owner"),selected = table[input$tool_table_rows_selected,]$tool_owner)
    updateSelectInput(session,"tool_user_edit",choices = cols_specific(table,"tool_user"),selected = table[input$tool_table_rows_selected,]$tool_user)
    updateSelectInput(session,"tool_location_edit",choices = cols_specific(table,"tool_location"),selected = table[input$tool_table_rows_selected,]$tool_location)
    output$tool_notes_output<-renderText(table[input$tool_table_rows_selected,]$tool_notes)
  })
  
  observe({toggleElement(id = "tool_owner_other_edit",condition = input$tool_owner_edit=="other")
  })
  
  observe({toggleElement(id = "tool_user_other_edit",condition = input$tool_user_edit=="other")
  })
  
  observe({toggleElement(id = "tool_location_other_edit",condition = input$tool_location_edit=="other")
  })
  
  observeEvent(input$submit_edit,{
    save_UpdatedtoolData(table2="tools_current",data2 = data_newtool_updatetool2())
  })

  
  # New Tool Stuff
  
  
  observeEvent(input$tool_create, {
    updateSelectInput(session,"tool_owner",choices = cols_specific(table,"tool_owner"))
    updateSelectInput(session,"tool_user",choices = cols_specific(table,"tool_user"))
    updateSelectInput(session,"tool_location",choices = cols_specific(table,"tool_location"))
    shinyjs::show("form")
  })
  
  observe({toggleElement(id = "tool_owner_other",condition = input$tool_owner=="other")
  })
  
  observe({toggleElement(id = "tool_user_other",condition = input$tool_user=="other")
  })
  
  observe({toggleElement(id = "tool_location_other",condition = input$tool_location=="other")
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
    reset("form")
    hide("form")
    show("thankyou_msg1")
  })
  
  observeEvent(input$submit_another_tool,{
    hide("thankyou_msg1")
    show("form")
  })
  
  data_newtool<-reactive({
    data <- sapply(fields_newtool, function(x) input[[x]])
    data
  })
  
  data_newtool_updatetool<-reactive({
    data <- sapply(fields_updatetool, function(x) input[[x]])
    data<-c(data,tool_date = humanTime())
    data
  })
  
  data_newtool_updatetool2<-reactive({
    data <- sapply(fields_updatetool2, function(x) input[[x]])
    data<-c(data,tool_date = humanTime(),tool_ID = table$tool_ID[input$tool_table_rows_selected])
    data
  })
  
  output$make<-renderText({
    input$tool_model
  })
  
  
  
  
  
  
})