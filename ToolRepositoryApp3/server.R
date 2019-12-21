library(shiny)

create_connection<-function(){
    library(RODBC)
    
    myServer <- "avickarsmysqlserver1.database.windows.net"
    myUser <- "avickars"
    myPassword <- "Pumpernickle@4054"
    myDatabase <- "toolrepository"
    myDriver <- "SQL Server" # Must correspond to an entry in the Drivers tab of "ODBC Data Sources"
    
    connectionString <- paste0(
        "Driver=", myDriver,
        ";Server=", myServer,
        ";Database=", myDatabase,
        ";Uid=", myUser,
        ";Pwd=", myPassword)
    
    conn <- odbcDriverConnect(connectionString)
    return(conn)
}

save_newtool<-function(table1,table2,data1,data2){
    
    conn<-create_connection()
    
    
    
    query1<- sprintf("insert into %s (%s) values ('%s')",
                     table1,
                     paste(names(data1),collapse = ", "),
                     paste(data1,collapse = "','"))
    
    query1<-str_c(query1,";",sep="")
    print(query1)
    
    query2<- sprintf("insert into %s (tool_ID, %s) values (@tool_id, '%s')",
                     table2,
                     paste(names(data2),collapse = ", "),
                     paste(data2,collapse = "','"))
    
    query2<-str_c(query2,";",sep="")
    
    query<-str_c("begin",
                 query1,
                 "declare @tool_id int;select @tool_id=tool_ID from tools;",
                 query2,
                 "end;",
                 sep = " ")
    
    sqlQuery(conn, query)
    close(conn)
    
}

humanTime <- function() format(Sys.time(), "%Y%m%d %H:%M:%S")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

loadTable<-function(){
  conn<-create_connection()
  
  query <- "select t.tool_manufacturer, t.tool_model,t.tool_name,t.tool_owner,tc.tool_user,tc.tool_location,tc.tool_notes from tools as t inner join tools_current as tc on t.tool_ID=tc.tool_ID;"
  data<-sqlQuery(conn, query)
  close(conn)
  data
}

fieldsMandatory <- c("tool_name","tool_owner","tool_user","tool_location")
fields_newtool<-c("tool_name","tool_manufacturer","tool_model","tool_owner")
fields_updatetool<-c("tool_location","tool_user","tool_notes")

#################################################################################################

shinyServer(function(input, output) {
    
    output$tool_table<-renderDataTable(
      loadTable()
    )
    
    observeEvent(input$tool_create, {
        shinyjs::show("form")
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
    
    output$make<-renderText({
        input$tool_model
    })
    
    
      
    
   

})
