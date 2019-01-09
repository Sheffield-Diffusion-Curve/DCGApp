library(data.table)
library(DCGen)

getDefaultInputData <- function() {
  data.table(
    Expert = c("Expert A", "Expert B", "Expert C"),
    M = rep("Triangle", 3),
    M_1 = c(54.2, 158.8, 204.4),    
    M_2 = c(10, 30, 30),
    M_3 = c(150, 230, 410),
    N = rep("Triangle", 3),
    N_1 = c(2.3, 5.7, 7.1),
    N_2 = c(0, 2, 2),
    N_3 = c(5, 15, 10),
    "T" = rep("Normal", 3),
    T_1 = c(5.1, 9.9, 3.5),
    T_2 = c(1.5, 1.5, 1.1),
    T_3 = c(NA, NA, NA)
  )
}


getNewInputData <- function(id) {
  data.frame(
    Expert = paste0("Expert", id),
    M = "Normal", M_1 = 100, M_2 = 1, M_3 = NA,
    N = "Normal", N_1 = 5, N_2 = 1, N_3 = NA,       
    "T" = "Normal", T_1 = 4, T_2 = 1, T_3 = NA
  )
}


showInputData <- function(dat) {
  data.table(
    Expert=dat[["Expert"]],
    M=paste0(dat[[2]], "(", apply(dat[, 3:5], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")"),
    N1=paste0(dat[[6]], "(", apply(dat[, 7:9], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")"),
    "t'"=paste0(dat[[10]], "(", apply(dat[, 11:13], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")")
  )
}


inputELCs <- function(dat) {
  experts <- apply(dat, 1, function(x) new_expert(x[1], x[2], x[3:5], x[6], x[7:9], x[10], x[11:13]))
  experts <- aggregate_experts(experts)
  
  experts
}


distribution_setter <- function(dat, id, dist='Normal', pars=c(0.1, 0.1, NA)) {
  dist <- as.character(dat[,1])
  pars <- unlist(dat[, 2:4])
  
  def <- rbind(Gamma=c(1, 0.1, NA),
               Normal=c(1, 0.1, NA),
               LogNormal=c(0, 0.1, NA),
               StudentT=c(1, 0.1, 3),
               LogStudentT=c(0, 0.1, 3),
               Triangle=c(2, 1, 3))
  
  def[dist, ] <- pars
  
  fluidPage(
    
    selectInput(paste0(id, "_dist"), paste0("Distribution of ", id), 
                c("Gamma", "Normal", "LogNormal", "StudentT", "LogStudentT", "Triangle"), selected = dist, width="50%"),
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'Gamma'"), fluidRow(
      column(1),
      column(5, numericInput(paste0(id, "_shape"), label = "Shape", value = def['Gamma', 1], min=0)), 
      column(5, numericInput(paste0(id, "_rate"), label = "Rate", value = def['Gamma', 2], min=0)))),
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'Normal'"), fluidRow(
      column(1),
      column(5, numericInput(paste0(id, "_mean"), label = "Mean", value = def['Normal', 1], min=0)), 
      column(5, numericInput(paste0(id, "_sd"), label = "SD", value = def['Normal', 2], min=0)))), 
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'LogNormal'"), fluidRow(
      column(1),
      column(5, numericInput(paste0(id, "_meanlog"), label = "log(Mean)", value = def['LogNormal', 1], min=0)), 
      column(5, numericInput(paste0(id, "_sdlog"), label = "log(SD)", value = def['LogNormal', 2], min=0)))), 
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'StudentT'"), fluidRow(
      column(4, numericInput(paste0(id, "_tmean"), label = "Mean", value = def['StudentT', 1], min=0)), 
      column(4, numericInput(paste0(id, "_tsd"), label = "SD", value = def['StudentT', 2], min=0)), 
      column(4, numericInput(paste0(id, "_tdf"), label = "degree of freedom", value = def['StudentT', 3], min=0)))), 
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'LogStudentT'"), fluidRow(
      column(4, numericInput(paste0(id, "_ltmeanlog"), label = "log(Mean)", value = def['LogStudentT', 1], min=0)), 
      column(4, numericInput(paste0(id, "_ltsdlog"), label = "log(SD)", value = def['LogStudentT', 2], min=0)), 
      column(4, numericInput(paste0(id, "_ltdf"), label = "degree of freedom", value = def['LogStudentT', 3], min=0)))), 
    
    conditionalPanel( condition = paste0("input.", id, "_dist == 'Triangle'"), fluidRow(
      column(4, numericInput(paste0(id, "_peak"), label = "Peak", value = def['Triangle', 1], min=0)), 
      column(4, numericInput(paste0(id, "_min"), label = "Min", value = def['Triangle', 2], min=0)), 
      column(4, numericInput(paste0(id, "_max"), label = "Max", value = def['Triangle', 3], min=0))))
  )
}


updateDistibution <- function(input) {
  d <- data.frame(input$new_name)
  
  for (n in c("M", "N1", "T")) {
    dist <- input[[paste0(n, "_dist")]]
    d[n] <- dist
    if (dist == "Gamma") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_shape")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_rate")]]  
      d[paste0(n, "_3")] <- NA  
    } else if (dist == "Normal") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_mean")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_sd")]]  
      d[paste0(n, "_3")] <- NA       
    } else if (dist == "LogNormal") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_meanlog")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_sdlog")]]  
      d[paste0(n, "_3")] <- NA       
    } else if (dist == "StudentT") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_tmean")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_tsd")]]  
      d[paste0(n, "_3")] <- input[[paste0(n, "_tdf")]]        
    } else if (dist == "LogStudentT") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_ltmeanlog")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_ltsdlog")]]  
      d[paste0(n, "_3")] <- input[[paste0(n, "_ltdf")]]        
    }else if (dist == "Triangle") {
      d[paste0(n, "_1")] <- input[[paste0(n, "_peak")]]  
      d[paste0(n, "_2")] <- input[[paste0(n, "_min")]]  
      d[paste0(n, "_3")] <- input[[paste0(n, "_max")]]        
    } else {
      d[paste0(n, "_1")] <- 10
      d[paste0(n, "_2")] <- 1
      d[paste0(n, "_3")] <- NA     
    }
  }
  d
}


