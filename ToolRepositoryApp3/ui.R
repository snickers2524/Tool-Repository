library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(stringr)
library(DT)


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyUI(fluidPage(
  inlineCSS(appCSS),
  useShinyjs(),
  
  titlePanel("Vickars Family Tool Repository"),
  sidebarLayout(
    sidebarPanel(
      
      
      actionButton("tool_create","Create Tool"),
      # actionButton("tool_edit","Edit Tool"),
      
      hidden(
        div(
          id = "form",
          textInput("tool_manufacturer", "Manufacturer", ""),
          textInput("tool_model", "Model", ""),
          textInput("tool_name", labelMandatory("Name"), ""),
          selectInput("tool_owner",labelMandatory("Owner"),choices = c("")),
          textInput("tool_owner_other",value = "",label = NULL),
          selectInput("tool_user",labelMandatory("Current User"),choices = c("") ),
          textInput("tool_user_other",value = "",label = NULL),
          selectInput("tool_location",labelMandatory("Current Location"),choices = c("") ),
          textInput("tool_location_other",value = "",label = NULL),
          textAreaInput("tool_notes","Notes",value=""),
          actionButton("submit", "Submit", class = "btn-primary")
        )
      ),
      
      hidden(
        div(
          id = "tooledit",
          textInput("tool_manufacturer_edit", "Manufacturer", ""),
          textInput("tool_model_edit", "Model", ""),
          textInput("tool_name_edit", labelMandatory("Name"), ""),
          selectInput("tool_owner_edit",labelMandatory("Owner"),choices = c("")),
          textInput("tool_owner_other_edit",value = "",label = NULL),
          selectInput("tool_user_edit",labelMandatory("Current User"),choices = c("") ),
          textInput("tool_user_other_edit",value = "",label = NULL),
          selectInput("tool_location_edit",labelMandatory("Current Location"),choices = c("") ),
          textInput("tool_location_other_edit",value = "",label = NULL),
          h5(strong("Notes")),
          textOutput("tool_notes_output"),
          textAreaInput("tool_notes_edit",label = NULL,value=""),
          actionButton("submit_edit", "Submit", class = "btn-primary")
        )
      ),
      
      hidden(
        div(
          id = "thankyou_msg1",
          h3("Thank you, you have sucessfully submitted your tool.  
                             Click the refresh button and your tool will appear"),
          actionLink("submit_another_tool","Submit Another Tool"),
          
        )
      )
    ),
    mainPanel(
      dataTableOutput("tool_table"),
      verbatimTextOutput('x5')
    )
  )
  
  
  
  
))
