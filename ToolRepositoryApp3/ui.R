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
  
  titlePanel("Family Tool Repository"),
  sidebarLayout(
    sidebarPanel(
      
      
      actionButton("tool_create","Create Tool",class="btn-primary"),
      # actionButton("tool_edit","Edit Tool"),
      
      hidden(
        div(
          id = "form",
          
          h3(strong("Submit New Tool")),
          
          selectInput("tool_manufacturer","Manufacturer",choices = c(""),selected = "Select One"),
          textInput("tool_manufacturer_other",label = NULL,value=NULL),
          
          textInput("tool_model", "Model", value = NULL),
          
          textInput("tool_name", labelMandatory("Name"), value = NULL),
          
          selectInput("tool_owner",labelMandatory("Owner"),choices = c(""),selected = "Select One"),
          textInput("tool_owner_other",label = NULL,value= NULL),
          
          selectInput("tool_user",labelMandatory("Current User"),choices = c(""),selected = "Select One" ),
          textInput("tool_user_other",label = NULL,value = NULL),
          
          selectInput("tool_location",labelMandatory("Current Location"),choices = c(""),selected = "Select One" ),
          textInput("tool_location_other",label = NULL,value = NULL),
          
          textAreaInput("tool_notes","Notes",value=NULL),
          actionButton("submit", "Submit", class = "btn-primary"),
          actionButton("cancel","Cancel",class="btn-primary")
        )
      ),
      
      hidden(
        div(
          id = "tooledit",
          
          h3(strong("Change Tool Status")),
          
          selectInput("tool_manufacturer_edit","Manufacturer",choices = c(""),selected = "Select One"),
          textInput("tool_manufacture_edit_other",label = NULL,value=NULL),
          
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
