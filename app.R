#IS 428 Visual Analystics Group Project
#Author: Group 6


## app.R ##
library(readr)
library(plyr)
library(dplyr)
library(forcats)
library(randomcoloR)
library(plotly)
library(crosstalk)
library(shinydashboard)

# Global Competitiveness Index
gci <- read_csv("data/cleaned/cleaned_GCI.csv", col_types = "ffif")

gci$Series <- fct_relevel(gci$Series, "5th pillar: Higher education and training", after = 0)

gci_top_10 <- gci %>%
    filter(Rank %in% (1:10)) %>%
    arrange(Series, desc(Year), Rank)

# Enrolment
data_enrollment_institute <- read.csv("data/cleaned/cleaned_enrollment-by-institute.csv")
data_enrollment_institute <- filter(data_enrollment_institute, Enrollment > 0)
data_poly_enrollment_institute <- filter(data_enrollment_institute, Institute_Type == "Polytechnic")
data_uni_enrollment_institute <- filter(data_enrollment_institute, Institute_Type == "University")

# Government Expenditure
expenditure_sector <- read_csv("data/cleaned/cleaned_expenditure_by_sector.csv", col_types = "ffn")
expenditure_education <- read_csv("data/cleaned/cleaned_expenditure_by_education.csv", col_types = "ffn")

expenditure_sector <- expenditure_sector %>%
    group_by(Year) %>%
    mutate(Percent = Expenditure / sum(Expenditure) * 100)

ui <- dashboardPage(
    dashboardHeader(title = "StoryBoard",
                    titleWidth = 250),
    dashboardSidebar(
        width = 250,
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
                    h2("Singapore has progressively upped its ranking over the years and clinched the first position for the 5th pillar (Higher Education and Training) of GCI since 2015-16."),
                    selectInput("gci_category",
                                "Category",
                                choices = levels(gci_top_10$Series),
                                width = "400px"),
                    plotlyOutput("gci_ranking")
            ),
            tabItem(tabName = "Enrollment",
                h2("Singapore's Higher Education Enrollment"),
                
                fluidRow(
                    column(1),
                    column(6,
                        sliderInput(inputId = "DateRange",
                            label = "Year Range",
                            min = 1993,
                            max = 2018,
                            value = c(1993, 2018),
                            sep = ""
                        )
                    ),
                    column(5,
                           selectInput(inputId="gender",label="Gender",choices = c("All"="MF",
                                                                                             "Male"="M",
                                                                                             "Female"="F"),
                                       selected = "All",multiple = F),
                    )
                    
                ),
                
                
                fluidRow(
                    
                    
                    box(title = "Polytechnic", width = 6, solidHeader = TRUE, status = "primary", plotOutput("Polyenrollement1", height = 250, click = "plot_click1")),
                    
                    
                    box(title = "University", width = 6, solidHeader = TRUE, status = "primary", plotOutput("Unienrollment1", height = 250, click = "plot_click1"))
                    
                ),
                verbatimTextOutput("clickData")
            ),
            tabItem(tabName = "Expenditure",
                    h2("Government Expenditure"),
                    fluidRow(
                        box(
                            width = 12,
                            title = "Expenditure by Sector",
                            plotlyOutput("expenditure_by_sector")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            title = "Expenditure by Sector",
                            plotlyOutput("expenditure_by_education")
                        )
                    )
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
    
    # Global Competitiveness Index (Bump Chart)
    output$gci_ranking <- renderPlotly({
        gci_top_10_category <- gci_top_10 %>%
            filter(Series == input$gci_category) %>%
            droplevels()
        
        if ("Singapore" %in% gci_top_10_category$Country) {
            gci_top_10_category %>%
                plot_ly(x = ~fct_rev(Year), y = ~Rank,
                        type = "scatter", mode = "lines+markers",
                        line = list(width = 3), marker = list(size = 10),
                        color = I("darkgray"),
                        hoverinfo = "text",
                        text = ~paste("Country:", Country, "<br>Year:", Year, "<br>Rank:", Rank),
                        transforms = list(
                            list(type = "groupby",
                                 groups = ~Country,
                                 styles = list(
                                     list(target = "Singapore",
                                          value = list(line = list(color = "red"),
                                                       marker = list(color = "red")))
                                 )))
                ) %>%
                layout(yaxis = list(autorange = "reversed", tickmode = "linear", showgrid = FALSE),
                       xaxis = list(title = "Year"))
        } else {
            gci_top_10_category %>%
                plot_ly(x = ~fct_rev(Year), y = ~Rank, color = ~Country,
                        type = "scatter", mode = "lines+markers",
                        line = list(width = 3), marker = list(size = 10),
                        colors = distinctColorPalette(nlevels(gci_top_10_category$Country)),
                        hoverinfo = "text",
                        text = ~paste("Country:", Country, "<br>Year:", Year, "<br>Rank:", Rank)) %>%
                layout(yaxis = list(autorange = "reversed", tickmode = "linear", showgrid = FALSE),
                       xaxis = list(title = "Year"))
        }
    })
    
    # Government Expenditure
    output$expenditure_by_sector <- renderPlotly({
        expenditure_sector %>%
            plot_ly(x = ~Year, y = ~Percent, color = ~fct_rev(Sector),
                    hoverinfo = "text",
                    text = ~paste0("The government spent ", round(Percent, 2), "% of the total expenditure on ", tolower(Sector), " in ", Year, ".")) %>%
            add_bars() %>%
            layout(barmode = "stack")
    })
    
    output$expenditure_by_education <- renderPlotly({
        expenditure_education %>%
            plot_ly(x = ~Year, y = ~Expenditure, color = ~`School Type`,
                    hoverinfo = "text",
                    text = ~paste("Type:", `School Type`, "<br>Year:", Year, "<br>Expenditure:", Expenditure)) %>%
            add_lines()
    })
    
    ###
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$Polyenrollement1 <- renderPlot({
        
        poly1 <- data_poly_enrollment_institute  %>%  filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>% ggplot(aes(x=Year, y = Enrollment))
        poly1 <- poly1 + geom_line(col = Institute) 
        
        poly1
    
    })
    
    output$Polyenrollement1 <- renderPlot({
        
        poly1 <- data_poly_enrollment_institute  %>%  filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>% ggplot(aes(x=Year, y = Enrollment, col = Institute))
        poly1 <- poly1 + geom_line()
        poly1
        
    })
    
    output$Unienrollment1 <- renderPlot({

        uni1 <- data_uni_enrollment_institute  %>%  filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>% ggplot(aes(x=Year, y = Enrollment))
        uni1 <- uni1 + geom_line(aes(colour = Institute))
        uni1

    })
    
    output$clickData <- renderPrint(({
        input$plot_click1
    }))
}

shinyApp(ui, server)
