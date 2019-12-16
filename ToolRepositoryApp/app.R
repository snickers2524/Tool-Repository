fieldsMandatory <- c("name","owner","currentuser","currentlocation")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <- ".mandatory_star { color: red; }"

fieldsAll <- c("name", "make", "model", "currentuser", "currentlocation","owner","notes")
responsesDir <- file.path("Responses")
epochTime <- function() {
    as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

appCSS <-
    ".mandatory_star { color: red; }
   #error { color: red; }"

loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    data <- dplyr::rbind_all(data)
    data
}

shinyApp(
    ui = fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        titlePanel("Tool Repository App"),
        fluidRow(
            column(2,
                div(
                    id = "form",
                    
                    textInput("make", "Make", ""),
                    textInput("model", "Model", ""),
                    textInput("name", labelMandatory("Name"), ""),
                    checkboxGroupInput("owner",labelMandatory("Owner"),c("Leif","Mark","Zlatko","Sam"),inline = TRUE),
                    selectInput("currentuser",labelMandatory("Current User"),c("Aidan","Mark","Zlatko","Sam","Leif") ),
                    selectInput("currentlocation",labelMandatory("Current Location"),c("Nanaimo Str.","Fifth Ave.","Edinburough Str.") ),
                    textAreaInput("notes","Notes",value=""),
                    actionButton("submit", "Submit", class = "btn-primary"),
                    shinyjs::hidden(
                        span(id = "submit_msg", "Submitting..."),
                        div(id = "error",
                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                        )
                    )
                ),
                shinyjs::hidden(
                    div(
                        id = "thankyou_msg",
                        h3("Thanks, your response was submitted successfully!"),
                        actionLink("submit_another", "Submit another response")
                    )
                )
            ),
            column(10,
                   downloadButton("downloadBtn", "Download responses"),
                   DT::dataTableOutput("responsesTable"),
            )
        )
    ),
    server = function(input, output, session) {
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
        
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })
        saveData <- function(data) {
            fileName <- sprintf("%s_%s.csv",
                                humanTime(),
                                digest::digest(data))
            
            write.csv(x = data, file = file.path(responsesDir, fileName),
                      row.names = FALSE, quote = TRUE)
        }
        
        # action to take when submit button is pressed
        observeEvent(input$submit, {
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })

        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
        output$responsesTable <- DT::renderDataTable(
            loadData(),
            rownames = FALSE,
            options = list(searching = FALSE, lengthChange = FALSE)
        ) 
        
        output$downloadBtn <- downloadHandler(
            filename = function() { 
                sprintf("tool_repository_%s.csv", humanTime())
            },
            content = function(file) {
                write.csv(loadData(), file, row.names = FALSE)
            }
        )
        
    }
)

