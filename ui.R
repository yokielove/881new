shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(sliderInput("obs", "Choose how many colleges you want to look at",
                 min= 1, max = 1000, value = 25)
    ),
    
    mainPanel(
      img(src="bu.jpg", height = 253, width = 500),
      
      h1("The Economist Colleges Rank"),
      
      p("The rankings are based on whether a college's alumni fare better than expected in the labor market, as measured by", 
        span("earnings 10 years after starting school", style = "color:blue"), "."),
      p(
        span("Mean", style = "color:red"), "and", span("Median", style = "color:red"), "are mean and median of students' earnings 10 years after starting school",
      "and", span("Cost", style = "color:red"), "is the net price for paying tution."),
      
      tableOutput("view"),
      
      plotOutput("fig"),
      p()
    )
  )
))