library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(scales)
library(ggplot2)

source("src/elicitationIO.r")
source("src/bassIO.r")


server <- function(input, output, session) {
  vals <- reactiveValues()
  
  vals$Data <- getDefaultInputData(n=2)
  vals$LastSelected <- 1
  vals$NextID <- 3
  
  Elcs <- reactive({
    Elcs <- inputELCs(vals$Data)
  })
  
  SimuNT <- reactive({
    elcs <- Elcs()
    # print(elcs)
    size <- input$num_iter
    agg <- input$agg_method
    randMNT(elcs, size, agg)
  })  
  
  vals$SimuPQ <- NULL
  vals$SimuCurves <- NULL
  
  
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
  
  
  output$PlotDistNT <- renderPlot({
    mnt <- SimuNT()
    par(mfrow=c(1, 3))
    
    pts <- mnt$M
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(M), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
    
    pts <- mnt$N
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(N[1]), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
    
    pts <- mnt$T
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(t), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
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
  
  
  observeEvent(input$updateBass, {
    mnt <- SimuNT()
    
    # Create a Progress object
    progress <- shiny::Progress$new(session, min=1, max=nrow(mnt))
    progress$set(message = "Translating Data", value = 0)

    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    vals$SimuPQ <- translateNT2PQ(mnt, updateProgress)
    
    vals$SimuCurves <- simulateCurves(vals$SimuPQ, input$projT)
    vals$CurrentT <- input$projT
  })
  
  
  output$PlotDistPQ <- renderPlot({
    if (is.null(vals$SimuPQ)) return ()
    
    par(mfrow=c(1, 3))
    
    pts <- vals$SimuPQ$M
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(M), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
    
    pts <- vals$SimuPQ$P
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(P), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
    
    pts <- vals$SimuPQ$Q
    den <- density(pts, na.rm=T, bw=diff(range(pts))/10)
    hist(pts, main=expression(Q), xlab="Value", col="grey", freq=F)
    polygon(den, col=alpha("grey", 0.2))
  })
  
  
  output$PlotBass <- renderPlot({
    curves <- vals$SimuCurves
    if (is.null(curves)) return ()
    
    t_max <- max(ceiling(input$projT), 1)
    
    if (t_max > vals$CurrentT) {
      vals$SimuCurves <- simulateCurves(vals$SimuPQ, input$projT)
      curves <- vals$SimuCurves
      vals$CurrentT <- input$projT
    }
    
    times <- 0:t_max
    size <- min(500, dim(curves)[3])
    y_max <- max(curves[, 2, ])
    
    Ns <- curves[times+1, 2, ]
    dNs <- curves[times+1, 3, ]
    
    plot(0, 0, 
         xlim=c(0, t_max), ylim=c(0, y_max), 
         xlab="Time", ylab="Number of total adoptions", 
         type="n", las=2)
    
    if (input$linesShow) {
      for (i in 1:size) {
        sel <- Ns[, i]
        alpha <- min(0.01, max(0.2, 50/size))
        lines(times, sel, col=alpha("aquamarine", 0.1))
      }
    }
    
    if (input$centShow) {
      cent = (1 - input$centVal/100)/2
      lines(times, apply(Ns, 1, function(x) quantile(x, cent)), lwd=1.2)
      lines(times, apply(Ns, 1, function(x) quantile(x, 1-cent)), lwd=1.2)
    }
    
    if (input$meanShow) {
      lines(times, rowMeans(Ns), lwd=1.5)
    }
    if (input$medianShow) {
      lines(times, apply(Ns, 1, median), lwd=1.5)
    }
    
    if (input$dnShow) {
      if (input$linesShow) {
        for (i in 1:size) {
          sel <- dNs[, i]
          alpha <- min(0.01, max(0.2, 50/size))
          lines(times, sel, col=alpha("orange", 0.1))
        }
      }
      
      if (input$centShow) {
        cent = (1 - input$centVal/100)/2
        lines(times, apply(dNs, 1, function(x) quantile(x, cent)), lwd=1.2, lty=2)
        lines(times, apply(dNs, 1, function(x) quantile(x, 1-cent)), lwd=1.2, lty=2)
      }
      if (input$meanShow) {
        lines(times, rowMeans(dNs), lwd=1.5)
      }
      if (input$medianShow) {
        lines(times, apply(dNs, 1, median), lwd=1.5)
      }
    }
    
  })
  
  
  output$SummaryBass <- renderPrint({
    cat("Input parameters\n")
    print(summary(SimuNT()))
    
    if (!is.null(vals$SimuPQ)) {
      cat("\nOutput parameters\n")
      print(summary(vals$SimuPQ))
    }
  })
}