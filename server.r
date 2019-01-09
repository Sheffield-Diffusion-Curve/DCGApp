library(shiny)
library(shinyjs)
library(shinydashboard)
library(data.table)
library(DT)
library(DCGen)

source("src/elicitationIO.r")


server <- function(input, output, session) {
  vals <- reactiveValues()
  
  vals$Data <- getDefaultInputData()
  vals$LastSelected <- 1
  vals$NextID <- 4
  
  vals$Experts <- NULL
  vals$Pars <- NULL
  vals$Curves <- NULL
  
  disable("downloadReport")
  disable("downloadParameters")
  disable("downloadCurves")
  
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
        "$(document).on('click', '#ElicitationBox button', function () {
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
  
  
  observeEvent({input$updateBass; input$dcg}, {
    vals$Experts <- inputELCs(vals$Data) 
    
    nt <- input$num_iter
    # Create a Progress object
    progress <- shiny::Progress$new(session, min=1, max=nt)
    progress$set(message = "Translating Data", value = 0)

    on.exit(progress$close())
    
    updateProgress <- function(value=NULL, detail =NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    vals$Pars <- rand_parameters(vals$Experts, nt, method=input$agg_method, type="continuous")
    
    vals$Curves <- generate_diffusion_curves(vals$Pars, t_max=input$projT, progress=updateProgress, dt=0.5)

    vals$CurrentT <- input$projT
    
    shinyjs::enable("downloadReport")
    shinyjs::enable("downloadParameters")
    shinyjs::enable("downloadCurves")
  })
  
  output$PlotDistNT <- renderPlot({
    if (is.null(vals$Pars)) return ()
    visualise_inputs(vals$Pars)
  })
  
  
  output$PlotDistPQ <- renderPlot({
    if (is.null(vals$Pars)) return ()
    visualise_fitted(vals$Pars)
  })
  
  
  output$PlotBass <- renderPlot({
    curves <- vals$Curves
    if (is.null(curves)) return ()
    
    t_max <- max(ceiling(input$projT), 1)
    
    if (t_max > vals$CurrentT) {
      vals$Curves <- generate_diffusion_curves(vals$Pars, t_max=t_max, progress=updateProgress, dt=0.5)
    }
    
    visualise_curves(curves, dN=input$dnShow,  ci_range=input$centVal, statistics=input$curveType=="stats", average=input$avgType)
    
  })
  
  
  output$SummaryBass <- renderPrint({
    cat("Input parameters\n")
    print(summary(vals$Pars$Parameters[, c("M", "N1", "t")]))
    
    
    cat("\nOutput parameters\n")
    print(summary(vals$Pars$Parameters[, c("M", "p", "q")]))
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$fileName, sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    
    content = function(file) {
      # if (is.null(vals$Curves)) {
      #   vals$Experts <- inputELCs(vals$Data) 
      #   
      #   nt <- input$num_iter
      #   # Create a Progress object
      #   progress <- shiny::Progress$new(session, min=1, max=nt)
      #   progress$set(message = "Translating Data", value = 0)
      #   
      #   on.exit(progress$close())
      #   
      #   updateProgress <- function(value=NULL, detail =NULL) {
      #     if (is.null(value)) {
      #       value <- progress$getValue()
      #       value <- value + (progress$getMax() - value) / 5
      #     }
      #     progress$set(value = value, detail = detail)
      #   }
      #   
      #   vals$Pars <- rand_parameters(vals$Experts, nt, method=input$agg_method, type="continuous")
      #   
      #   vals$Curves <- generate_diffusion_curves(vals$Pars, t_max=input$projT, progress=updateProgress, dt=0.5)
      #   
      #   vals$CurrentT <- input$projT
      # }
      mcvs <- apply(vals$Curves, c(1, 2), ifelse(input$avgType=="mean", mean, median))
      mcvs <- round(mcvs, 2)
      mcvs <- mcvs[mcvs[, 1] == round(mcvs[, 1]),]
      mcvs <- mcvs[seq.int(1, nrow(mcvs), by=max(round(nrow(mcvs)/10), 1)), ]
      rownames(mcvs) <- rep("", nrow(mcvs))
      
      # src <- normalizePath("report_template.Rmd")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, "report.Rmd", overwrite=TRUE)
      
      library(rmarkdown)
      format <-  switch(
        input$format,
        PDF=pdf_document(), HTML=html_document(), Word=word_document())
  
      out <- render(input="report_template.Rmd",
                    output_format=format,
                    output_dir="temp", 
                    envir = list(dat=dat, pars=vals$Pars, curves=vals$Curves, avg_curves=mcvs, input=input))
      
      file.copy(out, file)
    },
    contentType = "text/plain"
  )
  
  output$downloadParameters <- downloadHandler(
    filename = "DCG_Parameters.csv",
    
    content = function(file) {
      write.csv(vals$Pars$Parameters, file)
    }
  )
  
  output$downloadCurves <- downloadHandler(
    filename = "DCG_Curves.csv",
    
    content = function(file) {
      cvs <- data.frame(Time=vals$Curves[,1,1], Nt=vals$Curves[, 2, ])
      write.csv(cvs, file)
    }
  )
}