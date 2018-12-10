library(DCGen)

getDefaultData <- function(n=2) {
  data.table(
    Expert = paste0("Expert", 1:n),
    M = rep(100, n),
    M_L = rep(90, n),
    M_U = rep(110, n),
    N0 = rep(10, n),
    N0_L = rep(7, n),
    N0_U = rep(13, n),
    t = rep(8, n),
    t_L = rep(5, n),
    t_U = rep(10, n)
  )
}


getDefaultInputData <- function(n=2) {
  data.table(
    Expert = paste0("Expert", 1:n),
    M = rep("Normal", n),
    M_1 = rep(100, n),
    M_2 = rep(1, n),
    M_3 = rep(0, n),
    N = rep("Normal", n),
    N_1 = rep(10, n),
    N_2 = rep(1, n),
    N_3 = rep(0, n),
    "T" = rep("Gamma", n),
    T_1 = rep(8, n),
    T_2 = rep(1, n),
    T_3 = rep(0, n)
  )
}


getNewInputData <- function(id) {
  data.frame(
    Expert = paste0("Expert", id),
    M = "Normal", M_1 = 100, M_2 = 1, M_3 = 0,
    N = "Normal", N_1 = 10, N_2 = 1, N_3 = 0,       
    "T" = "Gamma", T_1 = 8, T_2 = 1, T_3 = 0
  )
}


showInputData <- function(dat) {
  data.table(
    Expert=dat[["Expert"]],
    M=paste0(dat[[2]], "(", apply(dat[, 3:5], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")"),
    N1=paste0(dat[[6]], "(", apply(dat[, 7:9], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")"),
    "T"=paste0(dat[[10]], "(", apply(dat[, 11:13], 1, function(x) {x <- x[!is.na(x)]; paste(x, collapse = ", ")}),")")
  )
}


inputELCs <- function(dat) {
  res <- list()

  elcs <- apply(dat, 1, function(x) {
    input_elicitation(x[2], x[3:5])
  })
  names(elcs) <- dat$Expert
  res$m <- aggregate_elicitations(elcs) 
  
  elcs <- apply(dat, 1, function(x) {
    input_elicitation(x[6], x[7:9])
  })
  names(elcs) <- dat$Expert
  res$n1 <- aggregate_elicitations(elcs) 
  
  elcs <- apply(dat, 1, function(x) {
    input_elicitation(x[10], x[11:13])
  })
  names(elcs) <- dat$Expert
  res$t <- aggregate_elicitations(elcs) 
  res
}


randMNT <- function(elcs, size, method) {
  data.frame(
    "M" = rand_elicitation(elcs$m, size, method),
    "N" = rand_elicitation(elcs$n1, size, method),
    "T" = rand_elicitation(elcs$t, size, method)
  )
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


