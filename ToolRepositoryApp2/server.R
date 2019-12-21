
fields_newtool<-c("tool_name","tool_manufacturer","tool_model","tool_owner")
fields_updatetool<-c("tool_location","tool_user","tool_notes")

fieldsMandatory <- c("tool_name","tool_owner","tool_user","tool_location")

source("ToolRepositoryApp2/AppFunctions.R")



shinyServer(function(input, output,session) {
    
    # output$tool_table<-renderDataTable(
    #   loadTable()
    # )
    
    # observe({
    #     # check if all mandatory fields have a value
    #     mandatoryFilled <-
    #         vapply(fieldsMandatory,
    #                function(x) {
    #                    !is.null(input[[x]]) && input[[x]] != ""
    #                },
    #                logical(1))
    #     mandatoryFilled <- all(mandatoryFilled)
    #     
    #     # enable/disable the submit button
    #     shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    # })
    # 
    # observeEvent(input$tool_create, {
    #     shinyjs::show("form")
    # })
    
    observeEvent(input$submit,{
        # save_data("tools",data_newtool())
        save_newtool("tools","tools_current",data_newtool(),data_newtool_updatetool())
        # save_data("tools_current",data_newtool_updatetool())
        reset("form")
        hide("form")
        show("thankyou_msg1")
    # })
    # 
    # observeEvent(input$submit_another_tool,{
    #     hide("thankyou_msg1")
    #     show("form")
    # })
    
    # data_newtool<-reactive({
    #     data <- sapply(fields_newtool, function(x) input[[x]])
    #     data
    # })
    # 
    # data_newtool_updatetool<-reactive({
    #     data <- sapply(fields_updatetool, function(x) input[[x]])
    #     data<-c(data,tool_date = humanTime())
    #     data
    # })
    
    output$make<-renderText({
        input$tool_model
    })
    
    
    
    
})

