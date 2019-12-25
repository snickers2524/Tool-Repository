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

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

mandatoryFilled<-function(input){
  test<-vector(length = length(fieldsMandatory))
  
  for (i in seq_along(fieldsMandatory)){
    
    if (fieldsMandatory[i]=="tool_name"){
      
      if (is.null(input$tool_name)){
        test[i]=FALSE
      } else {test[i]=TRUE}
    }
    
    if (fieldsMandatory[i]=="tool_owner"){
      if (input$tool_owner=="Select One" & is.null(input$tool_owner_other)){
        test[i]=FALSE
      } else test[i]=TRUE
    }
    
    if (fieldsMandatory[i]=="tool_user"){
      if (input$tool_owner=="Select One" & is.null(input$tool_user_other)){
        test[i]=FALSE
      } else test[i]=TRUE
    }
    
    if (fieldsMandatory[i]=="tool_location"){
      if (input$tool_owner=="Select One" & is.null(input$tool_location_other)){
        test[i]=FALSE
      } else test[i]=TRUE
    }
    
  }
  
  print(test)
  
  return(all(test))
}
