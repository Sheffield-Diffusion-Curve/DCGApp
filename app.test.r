library(shiny)
library(shinydashboard)
library(data.table)
library(DT)

source("src/elicitationIO.r")

# See above for the definitions of ui and server
ui <- fluidPage(
  uiOutput("ElicitationBox")
)



server <- function(input, output, session) {
  vals <- reactiveValues()
  
  vals$Data <- getDefaultInputData(n=2)
  vals$LastSelected <- 1
  vals$NextID <- 3
  
  Elcs <- reactive({
    Elcs <- inputELCs(vals$Data)
  }) 

  
  output$ElicitationBox <- renderUI({
    box(
      width = 12, collapsible = TRUE, title = "Elicitation Data", 
      column(
        6,
        offset = 3,
        HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
        actionButton(inputId = "Add_expert", label = "Add a new expert"),
        HTML('</div>')
      ),
      
      column(12, dataTableOutput("ElicitationData")),
      tags$script(
        HTML(
          '$(document).on("click", "input", function () {
          var checkboxes = document.getElementsByName("expert_selected");
          var checkboxesChecked = checkboxes.filter(function(d) {return d.checked;}).map(function(d) {return d.value});
          
          Shiny.onInputChange("checked_rows",checkboxesChecked);
  })'
        )
      ),
      tags$script(
        "$(document).on('click', '#ElicitationTable button', function () {
        Shiny.onInputChange('lastClickId',this.id);
        Shiny.onInputChange('lastClick', Math.random())
});"
      )
      
    )
    })
  
  
  output$ElicitationData <- renderDataTable({
    DT = showInputData(vals$Data)
    DT[["Actions"]] <-
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <button type="button" class="btn btn-secondary modify"id=modify_', 1:nrow(vals$Data), '>Modify</button>
        <button type="button" class="btn btn-warning delete" id=delete_', 1:nrow(vals$Data), '>Delete</button>
        </div>
        '
      )
    datatable(DT, escape = F)
  })
  
  
  modal_modify <- modalDialog(uiOutput("ModifyElicitationData"),
                              footer = tagList(actionButton("save_changes", "Save changes"), modalButton("Dismiss")), 
                              size = "l")
  
  
  output$ModifyElicitationData <- renderUI({
    dat <- data.frame(vals$CurrentExpert)

    fluidRow(
      h3(strong("Set elicitation data"), align = "center"),
      column(12, textInput("new_name", "Expert name", value=dat[, 'Expert'], width="100%")),
      h4("Cap of adoption number", align = "center"),
      distribution_setter(dat[, 1:4+1], "M"),
      h4("Number of adoption in the first period", align = "center"),
      distribution_setter(dat[, 1:4+5], "N1"),
      h4("When the new adoption start to decrease", align = "center"),
      distribution_setter(dat[, 1:4+9], "T")
    )
  })
  
  observeEvent(input$Add_expert, {
    id <- vals$NextID
    vals$NextID <- id + 1
    new_row <- getNewInputData(id)
    vals$Data <- rbind(vals$Data, new_row)
    
    vals$LastSelected <- nrow(vals$Data)
    vals$CurrentExpert <- vals$Data[vals$LastSelected]
    
    showModal(modal_modify)
  })
  
  observeEvent(input$save_changes, {
    changed <- updateDistibution(input)
    names(changed) <- names(vals$Data)

    vals$Data[vals$LastSelected] <- changed
    vals$CurrentExpert <- vals$Data[vals$LastSelected]
    removeModal()
  })
  
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId %like% "delete") {
                   row_to_del = as.numeric(gsub("delete_", "", input$lastClickId))
                   vals$Data = vals$Data[-row_to_del]
                 } else if (input$lastClickId %like% "modify") {
                   vals$LastSelected <- as.numeric(gsub("modify_", "", input$lastClickId))
                   vals$CurrentExpert <- vals$Data[vals$LastSelected]
                   showModal(modal_modify)
                 }
               })
  
  
}

shinyApp(ui, server)