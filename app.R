#IS 428 Visual Analystics Group Project
#Author: Group 6


## app.R ##
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "StoryBoard",
                    titleWidth = 350),
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            menuItem("Introduction", tabName = "Intro"),
            menuItem("Higher Education Ranking", tabName = "Ranking"),
            menuItem("Enrollment", tabName = "Enrollment"),
            menuItem("Government Expenditure", tabName = "Expenditure"),
            menuItem("Graduate's Prospects", tabName = "Prospects")
        )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            
            tabItem(tabName = "Intro",
                fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50)
                    )
                )
            ),
            tabItem(tabName = "Ranking",
                    h2("Singapore has increased its Higher Education and Training progressively and has maintained its top position since 2015-16.")    
            ),
            tabItem(tabName = "Enrollment",
                h2("Singapore's Higher Education Enrollment")    
            ),
            tabItem(tabName = "Expenditure",
                    h2("Government spending")    
            ),
            tabItem(tabName = "Prospects",
                    h2("How much can University Graduate expect to Earn?")    
            )
        )
            
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)





