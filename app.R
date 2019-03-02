#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

spiral <- function(x, y){
  stopifnot(length(x) == length(y))
  sapply(1:length(x), function(i){
    x <- x[i]
    y <- y[i]
    d <- max(abs(x), abs(y))
    n <- (2*d-1)^2
    if(x == -y & y <= 0){
      a <- 8
      b <- 0
    }else if(x > 0 & abs(x) >= abs(y)){
      a <- 1
      b <- y
    }else if(y > 0 & abs(y) >= abs(x)){
      a <- 3
      b <- -x
    }else if(x < 0 & abs(x) >= abs(y)){
      a <- 5
      b <- -y
    }else{
      a <- 7
      b <- x
    }
    return(n + a*d + b)
  })
}

jumps.knight <- function(x, y, jump = c(2,1)){
  jump_back <- c(jump[2], jump[1])
  spots <- unique(data.frame(x = c(jump, jump, -jump, -jump),
                       y = c(jump_back, -jump_back, jump_back, -jump_back)))
  if(jump[1] != jump[2]){
    spots <- spots %>%
      filter(abs(x) != abs(y))
  }
  
  spots$x <- spots$x + x
  spots$y <- spots$y + y
  return(spots)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("j1",
                     "Jump Dimension 1",
                     min = 1,
                     value = 2,
                     step = 1),
         numericInput("j2",
                      "Jump Dimension 2",
                      min = 0,
                      value = 1,
                      step = 1),
         numericInput("x0",
                      "X Starting Coordinate",
                      min = -Inf,
                      max = Inf,
                      value = 0,
                      step = 1),
         numericInput("y0",
                      "Y Starting Coordinate",
                      min = -Inf,
                      max = Inf,
                      value = 0,
                      step = 1),
         numericInput("n_max",
                      "Maximum Jumps",
                      min = 1,
                      value = 10000,
                      step = 1),
         actionButton("goButton", "Run"), 
         width = 3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        column(width = 10, plotOutput("distPlot", height = "auto"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    j1 <- eventReactive(input$goButton, {
      input$j1
    })
    j2 <- eventReactive(input$goButton, {
      input$j2
    })
    nmax <- eventReactive(input$goButton, {
      as.numeric(input$n_max)
    })
    visited <- data.frame(x=0,y=0,s=1,n=1)
    p <- visited %>% 
      ggplot() +  
      geom_path(aes(x, y, color = n)) + 
      scale_color_continuous(low = "#440000", high = "#44FF00") +
      coord_fixed()+
      guides(color=FALSE)+
      theme_minimal()
    finished <- FALSE
    observeEvent(input$goButton, {
      visited <- data.frame(
        x = as.numeric(input$x0),
        y = as.numeric(input$y0),
        s = spiral(
          as.numeric(input$x0),
          as.numeric(input$y0)
        )
      )
    })
   
   output$distPlot <- renderPlot({
     update_data()
     jump <- as.numeric(c(j1(), j2()))
     N <- nrow(visited)
     invalidateLater(100, session)
     p$data <- visited
     p +
       ggtitle(ifelse(finished, paste0("The knight lasted ", scales::comma(N), " moves!"), ""),
               ifelse(finished, paste0("Using jumps of ", jump[1], " by ", jump[2],
                      ", ending on (", scales::comma(visited$x[N]), ",", scales::comma(visited$y[N]), 
                      "), which is space ", scales::comma(visited$s[N]), " in the spiral."),
               "")) + 
       geom_point(x = visited$x[N],
                  y = visited$y[N],
                  color = "red") 
   }, height = function() {
     session$clientData$output_distPlot_width
   })
   
   update_data <- function(){
     jump <- as.numeric(c(j1(), j2()))
     n_max <- nmax()
     N <- nrow(visited)
     if(N < n_max){
       x_current <- visited$x[nrow(visited)]
       y_current <- visited$y[nrow(visited)]
       jump_next <- jumps.knight(x_current, y_current, jump) %>%
         mutate(s = spiral(x,y)) %>%
         filter(! s %in% visited$s)
       if(nrow(jump_next) == 0){
         finished <<- TRUE
         break
       }
       jump_next <- jump_next %>%
         filter(s == min(s)) %>%
         mutate(n = N+1)
       visited <- bind_rows(visited, jump_next) 
     }else{
       finished <<- TRUE
     }
     visited <<- visited
   }
}

# Run the application 
shinyApp(ui = ui, server = server)

