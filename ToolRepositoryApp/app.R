library(shinyjs)

fieldsMandatory <- c("name", "owner","currentuser","current location")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <-
    ".mandatory_star { color: red; }"

shinyApp(
    ui = fluidPage(
        # shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        titlePanel("Tool Submission"),
        div(
            id = "form",
            
            textInput("make", "Make", ""),
            textInput("model", "Model", ""),
            textInput("name", labelMandatory("Name"), ""),
            checkboxGroupInput("owner",labelMandatory("Owner"),c("Leif","Mark","Zlatko","Sam"),inline = TRUE),
            selectInput("currentuser",labelMandatory("Current User"),c("Aidan","Mark","Zlatko","Sam","Leif") ),
            selectInput("currenlocation",labelMandatory("Current Location"),c("Nanaimo Str.","Fifth Ave.","Edinburough Str.") ),
            actionButton("submit", "Submit", class = "btn-primary")
            
            
        ),
        shinyjs::hidden(
            div(
                id = "thankyou_msg",
                h3("Thanks, your response was submitted successfully!"),
                actionLink("submit_another", "Submit another response")
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
        observeEvent(input$submit, {
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
        })
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        }) 
    }
)