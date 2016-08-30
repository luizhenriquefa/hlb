library(shiny)
library(lattice)
# Inseto
# 0 = nao existe (branco)
# 1 = nao infectivo (preto)
# 2 = infectivo (vermelho)

# Planta
# -1 = infectada (preto)
# 0 = nao existe (branco)
# 1 = nao infectada (verde)

evolvePlanta <- function(x,y,prob){
  ps <- x==y & y==1 # Planta saudavel
  x[which(ps==TRUE)] <- 1
  
  pi <- x==1 & y==2 # Planta infectada
  x[which(pi==TRUE)] <- sample(c(-1,1), 1, prob=c(prob,1-prob))
  
  return(x)
}

evolve <- function(grid, planta){
  size <- nrow(grid)
  new.grid <- grid
  moves_x <- c(0,-1,1,0)
  moves_y <- c(-1,0,0,1)
  sequence <- sample.int(length(new.grid))
  
  for (i in sequence) {
    x <- 1 + (i - 1)%%size
    y <- 1 + (i - 1)%/%size
    ref <- sample.int(4, 1)
    ref.x <- 1 + (x + moves_x[ref] - 1)%%size
    ref.y <- 1 + (y + moves_y[ref] - 1)%%size
    
    if (new.grid[x, y] == 0) {
      new.grid[x, y] <- new.grid[ref.x, ref.y]
      new.grid[ref.x, ref.y] <- 0
    }
    if (new.grid[x, y] != 0) {
      death <- runif(1) <= 1/30
      birth <- runif(1) <= 1/30
      if (death) {new.grid[x, y] <- 0 }
      if (birth) {new.grid[ref.x, ref.y] <- new.grid[x, y]}
    }
    
    if(new.grid[x, y] == 1 && planta[x, y] == -1){
      new.grid[x, y] <- 2
    }
  }
  return(new.grid)
}
# Conta o numero de plantas saudaveis e infectadas ao longo do tempo
funcListaP <- function(z, x, contador){
  if(!is.list(z)){
    z <- list(contador, sum(x == 1), sum(x == -1))
  }
  else{
    z[[1]][length(z[[1]]) + 1] = contador
    z[[2]][length(z[[2]]) + 1] = sum(x == 1) # Planta saudavel
    z[[3]][length(z[[3]]) + 1] = sum(x == -1) # Planta infectada
  }
  return(z)
}

# Conta o numero de insetos infectivos e nao infectivos ao longo do tempo
funcListaI <- function(z, x, contador){
  if(!is.list(z)){
    z <- list(contador, sum(x == 2), sum(x == 1))
  }
  else{
    z[[1]][length(z[[1]]) + 1] = contador
    z[[2]][length(z[[2]]) + 1] = sum(x == 2) # Inseto infectivo
    z[[3]][length(z[[3]]) + 1] = sum(x == 1) # Inseto nao infectivo
  }
  return(z)
}

shinyServer(function(input,output,session){
  # Grafico planta X tempo
  output$plantaPlot <- renderPlot({
    image(gr$planta, main="Plantas", col=c("black","white","green"), axes="false")
  })
  
  # Grafico inseto X tempo
  output$insetoPlot <- renderPlot({
    image(gr$inseto, main="Insetos", col=c("white","black","red"), axes="false")
  })
  
  output$contador <- renderText(gr$counter)
  
  output$contadorPlanta <- renderPlot({
    ylim=range(c(gr$listaP[[2]], gr$listaP[[3]]))
    plot(gr$listaP[[1]], gr$listaP[[2]], t="l", col="green", ylim=ylim, xlab="Tempo (dias)", ylab="Populacoes")
    lines(gr$listaP[[1]], gr$listaP[[3]], t="l", col="black")
    legend("topleft", legend=c("Saudavel", "Infectada"), lwd=2, col=c("green","black"), bty="n")
  })
  
  output$contadorInseto <- renderPlot({
    ylim=range(c(gr$listaI[[2]], gr$listaI[[3]]))
    plot(gr$listaI[[1]], gr$listaI[[2]], t="l", col="red", ylim=ylim, xlab="Tempo (dias)", ylab="Populacoes")
    lines(gr$listaI[[1]], gr$listaI[[3]], t="l", col="black")
    legend("topleft", legend=c("Infectivos", "Nao infectivos"), lwd=2, col=c("red","black"), bty="n")
  })
  
  
  filedata <- function(file){
    if(is.null(file)){
      return(NULL)
    }
    data.matrix(read.table(file$datapath))
  }
  
  filedataP <- reactive({
    filedata(input$fileP)
  })
  
  filedataI <- reactive({
    filedata(input$fileI)
  })
  
  gr <- reactiveValues(counter = 1, stopHelper = 0, planta = matrix(0,2,2), inseto = matrix(0,2,2), listaI = list(0,0,0), listaP = list(0,0,0))
  
  observe({
    if(gr$stopHelper==0){
      return(NULL)
    }
    isolate({
      if (gr$counter==1){
        gr$inseto <- filedataI()
        gr$planta <- filedataP()
      }
      if (gr$counter>1){
        gr$inseto <- evolve(gr$inseto, gr$planta)
        gr$planta <- evolvePlanta(gr$planta, gr$inseto, input$prob)
        
        gr$listaI <- funcListaI(gr$listaI, gr$inseto, gr$counter)
        gr$listaP <- funcListaP(gr$listaP, gr$planta, gr$counter)
      }
      gr$counter=gr$counter+1
      
    })
    
    if (((isolate(gr$stopHelper==1))))
      invalidateLater(200,session)
  })
  
  observe({
    if(input$Start > 0){
      gr$stopHelper <<- 1
    }
  })
  
  observe({
    if(input$Stop > 0){
      gr$counter <<- 1
      gr$stopHelper <<- 0
    }
  })
  
})