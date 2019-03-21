#
# This is a Shiny app that solves the trapped knight problem, where a chess 
# knight is placed at the origin of an integer spiral, and keeps making the 
# legal move that places it closest to the center, until it eventually can no 
# longer make a legal move.
#
# This app dynamically draws the path of such a knight, allowing the use to 
# specify the initial conditions and the shape of the knight's move.
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Function that, given a set of x and y integer coordinates, returns the number
# that represents the element in the integer spiral that lands on that space.
spiral <- function(x, y){
  stopifnot(length(x) == length(y))
  stopifnot(is.integer(x) & is.integer(y))
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

# Given x and y coordinates, and a shape of a knights jump, return a dataframe 
# of all spots that the knight can jump to
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

# Define UI for application that draws the knights tour.
ui <- fluidPage(
   
   # No title
   titlePanel(""),
   
   # Sidebar with inputs for the knight's starting location and the shape of its
   # jump, as well as the maximum number of iterations before the simulation 
   # should break (to avoid long server hang or an infinite loop.)
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
      
      # Show a plot of the generated knights tour
      mainPanel(
        column(width = 10, plotOutput("Plot", height = "auto"))
      )
   )
)

# Define server logic required to draw the knights tour
server <- function(input, output, session) {
    
    # Get user inputs as reactives.
    j1 <- eventReactive(input$goButton, {
      input$j1
    })
    j2 <- eventReactive(input$goButton, {
      input$j2
    })
    nmax <- eventReactive(input$goButton, {
      as.numeric(input$n_max)
    })
    # Initialize with the simplest case.
    visited <- data.frame(x=0,y=0,s=1,n=1)
    # Get the plot of the base case ready.
    p <- visited %>% 
      ggplot() +  
      geom_path(aes(x, y, color = n)) + 
      scale_color_continuous(low = "#440000", high = "#44FF00") +
      coord_fixed()+
      guides(color=FALSE)+
      theme_minimal()
    # State of plot
    finished <- FALSE
    # When the go button is pressed, rebuild the dataframe
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
   
   # Update the data and then render the plot using that data.
   output$Plot <- renderPlot({
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
   
   # Update the data, called within the plotting expression
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

