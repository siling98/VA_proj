# IS428 Visual Analytics for Business Intelligence (G1)
# AY2019-20 Term 2
# Author: Team 6

library(readr)
library(plyr)
library(dplyr)
library(forcats)
library(randomcoloR)
library(plotly)
library(crosstalk)
library(packcircles)
library(shinydashboard)
library(ggiraph)

### Start of data import ###

# Global Competitiveness Index
gci <- read_csv("data/cleaned/cleaned_GCI.csv", col_types = "ffif")

gci$Series <- fct_relevel(gci$Series, "5th pillar: Higher education and training", after = 0)

gci_top_10 <- gci %>%
    filter(Rank %in% (1:10)) %>%
    arrange(Series, desc(Year), Rank)

# Enrollment
data_enrollment_institute <- read.csv("data/cleaned/cleaned_enrollment-by-institute.csv")
data_enrollment_institute <- filter(data_enrollment_institute, Enrollment > 0)
data_poly_enrollment_institute <- filter(data_enrollment_institute, Institute_Type == "Polytechnic")
data_uni_enrollment_institute <- filter(data_enrollment_institute, Institute_Type == "University")

# Enrollment by Course
data_enrollment_course <- read_csv("data/cleaned/cleaned_enrollment-by-first-degree.csv")
data_enrollment_course <- filter(data_enrollment_course, Enrollment > 0)
data_enrollment_course <- filter(data_enrollment_course, First_Degree != "Males Total")
data_enrollment_course <- filter(data_enrollment_course, First_Degree != "Females Total")
data_enrollment_course <- filter(data_enrollment_course, First_Degree != "Total")
data_poly_enrollment_course <- filter(data_enrollment_course, Institute_Type == "Polytechnic")
data_uni_enrollment_course <- filter(data_enrollment_course, Institute_Type == "University")

# Government Expenditure
expenditure_sector <- read_csv("data/cleaned/cleaned_expenditure_by_sector.csv", col_types = "ffn")
expenditure_education <- read_csv("data/cleaned/cleaned_expenditure_by_education.csv", col_types = "ffn")

expenditure_sector <- expenditure_sector %>%
    group_by(Year) %>%
    mutate(Percent = Expenditure / sum(Expenditure) * 100)

# Graduate Employment Survey
ges <- read_csv("data/cleaned/cleaned_GES.csv", col_types = "ffffnniiii")
ges2 <- read_csv("data/cleaned/cleaned_GES2.csv", col_types = "fffffnniiii")

### End of data import ###

###

### Start of ui ###

ui <- dashboardPage(
    dashboardHeader(title = "T6 - Growth Signal",
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
            tabItem(
                tabName = "Intro",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Introduction"),
                           h2("Problem"),
                           h4("Despite abundance of data, it is not easy for a post-secondary school student to make an informed decision on their education. This application will help a post-secondary school student gather insights via an interactive dashboard."),
                           h2("Motivation"),
                           h4("Education is the keystone to securing a better future. Many parents are aware of this fact and relentlessly seek information on the best education. However this task is not simple as data is consolidated by different agencies and therefore it is difficult to gain insights especially an overall picture. With so many tools and possibilities available for data visualisation, we have decided to compile the data and create an interactive dashboard for the parents and young adults alike."),
                           h2("Approach"),
                           h4("Consolidate the data from different agencies into a central dashboard for analysis.The dashboard would include infographics and heat maps to allow qualitative data analysis")),
                    column(1)
                )
            ),
            tabItem(
                tabName = "Ranking",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Global Competitiveness Index (GCI)"),
                           h4("The GCI assesses the ability of countries to provide high levels of prosperity of citizens. There are 12 pillars of competitiveness. Here, we analyse the 5th pillar, namely Higher Education and Training, and its subcategories."),
                           h4("The chart below displays the ranking among the top 10 countries for the selected category. Singapore is denoted by the red line. If Singapore is not among the top 10, each country will be represented by a different colour."),
                           h4("Select an option from the filter to view the ranking by category (5th pillar - the overall, or its subcategories).")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           selectInput("gci_category",
                                       "Category",
                                       choices = levels(gci_top_10$Series),
                                       width = "400px"),
                           plotlyOutput("gci_ranking")),
                    column(1)
                )
            ),
            tabItem(
                tabName = "Enrollment",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Enrollment"),
                           h4("The line charts show the number of enrollment for both polytechnic and university institutes over the years."),
                           h4("Use the drop-down and slider to filter by gender and year range.")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(2,
                           selectInput("gender",
                                       "Gender",
                                       choices = c("All" = "MF", "Male" = "M", "Female" = "F"),
                                       selected = "All")),
                    column(5,
                           sliderInput("DateRange",
                                       "Year Range",
                                       min = 1993,
                                       max = 2018,
                                       value = c(1993, 2018),
                                       sep = "")),
                    column(4)
                ),
                fluidRow(
                    column(1),
                    column(5,
                           box(title = "Polytechnic",
                               width = "100%",
                               solidHeader = TRUE,
                               status = "primary",
                               plotlyOutput("Polyenrollement1", height = 250))),
                    column(5,
                           box(title = "University",
                               width = "100%",
                               solidHeader = TRUE,
                               status = "primary",
                               plotlyOutput("Unienrollment1", height = 250))),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           h4("The bubble charts show the breakdown of enrollment by course type for polytechnic and university."),
                           h4("Use the drop-down and slider to filter by gender and year.")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(2,
                           selectInput("gender2",
                                       "Gender",
                                       choices = c("All" = "MF", "Male" = "M", "Female" = "F"),
                                       selected = "All")),
                    column(5,
                           sliderInput("Year",
                                       "Year",
                                       min = 1993,
                                       max = 2018,
                                       value = 2018,
                                       sep = "")),
                    column(4)
                ),
                fluidRow(
                    column(1),
                    column(5,
                           box(title = "Polytechnic (by Course)",
                               width = "100%",
                               solidHeader = TRUE,
                               status = "primary",
                               girafeOutput("Polyenrollement2", height = 250))),
                    column(5,
                           box(title = "University (by Course)",
                               width = "100%",
                               solidHeader = TRUE,
                               status = "primary",
                               girafeOutput("Unienrollment2", height = 250))),
                    column(1)
                )
            ),
            tabItem(
                tabName = "Expenditure",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Government Expenditure"),
                           h4("The 100% stacked bar chart shows the proportion of government expenditure by sector. The education sector is shown at the top of each bar for ease of comparison over the years."),
                           h4("The line chart shows the breakdown of education expenditure by level across year."),
                           br()),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(5,
                           box(title = "Expenditure (by Sector)",
                               width = "100%",
                               height = "625px",
                               solidHeader = TRUE,
                               status = "primary",
                               plotlyOutput("expenditure_by_sector", height = "550px"))),
                    column(5,
                           box(title = "Expenditure of Education (by Level)",
                               width = "100%",
                               height = "625px",
                               solidHeader = TRUE,
                               status = "primary",
                               plotlyOutput("expenditure_by_education", height = "550px"))),
                    column(1)
                )
            ),
            tabItem(
                tabName = "Prospects",
                fluidRow(
                    column(1),
                    column(10,
                           h1("Graduate Employment Survey (GES)"),
                           h4("The GES is an annual survey conducted by the Ministry of Education (MoE) and the 6 universities on the employment conditions after graduation."),
                           h4("The bar chart shows the gross monthly mean salary of different degrees offered under a university and school/faculty in 2018."),
                           h4("The line chart shows the 25th, 50th (median) and 75th percentile of the salary of one of the degrees across year. Click on any bar of a degree to see its salary percentiles."),
                           h4("Use the filters to compare the gross monthly mean salary of the degrees offered by other universities and their schools/faculties.")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(3,
                           selectInput("university", "University",
                                       choices = levels(ges$University),
                                       selected = "National University of Singapore")),
                    column(4,
                           selectInput("school", "School/Faculty",
                                       choices = NULL,
                                       width = "85%")),
                    column(4)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           align = "center",
                           h3(textOutput("ges_mean_salary_by_degree_title")),
                           plotlyOutput("ges_mean_salary_by_degree", height = "350px")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           align = "center",
                           h3(textOutput("ges_salary_percentile_title")),
                           plotlyOutput("ges_salary_percentile", height = "350px")
                    ),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           br(),
                           h4("The chart below shows a heatmap of the employment rate by university and field of study."),
                           h4("Use the drop-down and slider to filter by whether there is honors/cum laude and year.")),
                    column(1)
                ),
                fluidRow(
                    column(1),
                    column(2,
                           selectInput("Hons", "Honors/Cum Laude",
                                       choices = c("Yes" = "Yes", "No" = "No"),
                                       selected = "No")),
                    column(5,
                           sliderInput("Year2", "Year",
                                       min = 2013,
                                       max = 2018,
                                       value = 2018,
                                       sep = "")),
                    column(4)
                ),
                fluidRow(
                    column(1),
                    column(10,
                           align = "center",
                           plotlyOutput("ges_heatmap", height = "500px")
                    ),
                    column(1)
                )
            )
        )
            
    )
)

### End of ui ###

###

### Start of server ###

server <- function(session, input, output) {
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
            add_lines() %>%
            layout(legend = list(orientation = "h"))
    })
    
    # Graduate Employment Survey
    observe({
        ges_university_school <- ges %>%
            filter(Year == 2018,
                   University == input$university) %>%
            droplevels()
        
        updateSelectInput(session, "school", choices = levels(ges_university_school$School))
    })
    
    output$ges_mean_salary_by_degree <- renderPlotly({
        ges_bar <- ges %>%
            filter(Year == 2018,
                   University == input$university,
                   School == input$school) %>%
            droplevels()
        
        ges_bar %>%
            plot_ly(x = ~`Gross Monthly Mean`, y = ~Degree,
                    hoverinfo = "text",
                    text = ~paste0("The gross monthly mean salary of a person with ", Degree, " degree in ", University, " is $", `Gross Monthly Mean`, "."),
                    key = ~Degree,
                    source = "degree") %>%
            add_bars() %>%
            layout(xaxis = list(title = "Gross Monthly Mean"))
    })
    
    observe({
        ges_degree_2018 <- ges %>%
            filter(Year == 2018,
                   University == input$university,
                   School == input$school) %>%
            droplevels() %>%
            filter(Degree == levels(Degree)[1]) %>%
            droplevels()
        
        ges_time <- ges %>%
            filter(University == input$university,
                   School == input$school) %>%
            droplevels() %>%
            filter(Degree == as.character(ges_degree_2018$Degree)) %>%
            droplevels()
        
        output$ges_mean_salary_by_degree_title <- renderText({
            paste("2018 Gross Monthly Mean Salary by Degree(s) under", input$school, "in", input$university)
        })
        
        output$ges_salary_percentile_title <- renderText({
            paste("Salary Percentiles for", levels(ges_time$Degree), "Degree from", input$university)
        })
        
        output$ges_salary_percentile <- renderPlotly({
            ges_time %>%
                plot_ly(x = ~Year) %>%
                add_trace(y = ~`Gross Monthly 25th Percentile`, name = "Gross Monthly 25th Percentile", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ", Year,
                                         "<br>25th Percentile: $", `Gross Monthly 25th Percentile`)) %>%
                add_trace(y = ~`Gross Monthly Median`, name = "Gross Monthly Median", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ", Year,
                                         "<br>Median: $", `Gross Monthly Median`)) %>%
                add_trace(y = ~`Gross Monthly 75th Percentile`, name = "Gross Monthly 75th Percentile", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ", Year,
                                         "<br>75th Percentile: $", `Gross Monthly 75th Percentile`)) %>%
                layout(yaxis = list(title = "Salary"))
        })
    })
    
    ges_event <- reactive({
        event_data(event = "plotly_click", source = "degree")
    })
    
    observeEvent(ges_event(), {
        ges_time <- ges %>%
            filter(University == input$university,
                   School == input$school,
                   Degree == ges_event()$key) %>%
            droplevels()
        
        output$ges_salary_percentile_title <- renderText({
            paste("Salary Percentiles for", ges_event()$key, "Degree from", input$university)
        })
        
        output$ges_salary_percentile <- renderPlotly({
            ges_time %>%
                plot_ly(x = ~Year) %>%
                add_trace(y = ~`Gross Monthly 25th Percentile`, name = "Gross Monthly 25th Percentile", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ", Year,
                                         "<br>25th Percentile: $", `Gross Monthly 25th Percentile`)) %>%
                add_trace(y = ~`Gross Monthly Median`, name = "Gross Monthly Median", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ",Year,
                                         "<br>Median: $", `Gross Monthly Median`)) %>%
                add_trace(y = ~`Gross Monthly 75th Percentile`, name = "Gross Monthly 75th Percentile", type = "scatter", mode = "lines+markers",
                          hoverinfo = "text",
                          text = ~paste0("University: ", University,
                                         "<br>School/Faculty: ", School,
                                         "<br>Degree: ", Degree,
                                         "<br>Year: ", Year,
                                         "<br>75th Percentile: $", `Gross Monthly 75th Percentile`)) %>%
                layout(yaxis = list(title = "Salary"))
        })
    })
    
    # Enrollment
    output$Polyenrollement1 <- renderPlotly({
        data_poly_enrollment_institute %>%
            filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>%
            plot_ly(x = ~Year, y = ~Enrollment, color = ~Institute,
                    hoverinfo = "text",
                    text = ~paste("Institute:", Institute, "<br>Year:", Year, "<br>Enrollment:", Enrollment)) %>%
            add_lines()
    })
    
    output$Unienrollment1 <- renderPlotly({
        data_uni_enrollment_institute %>%
            filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>%
            plot_ly(x = ~Year, y = ~Enrollment, color = ~Institute,
                    hoverinfo = "text",
                    text = ~paste("Institute:", Institute, "<br>Year:", Year, "<br>Enrollment:", Enrollment)) %>%
            add_lines()
    })
    
    output$Polyenrollement2 <- renderGirafe({
        filterdata1 <- filter(data_poly_enrollment_course, Year == input$Year & Sex == input$gender2)
        
        data1 <- data.frame(group=paste(filterdata1$First_Degree), value=filterdata1$Enrollment) 
        if (input$gender2 == "MF"){
            genderpoly <- "ALL"
        }
        else if (input$gender == "F") {
            genderpoly <- "Female"
        }
        else{
            genderpoly <- "Male"
        }
        
        data1$text <- paste("Course: ",data1$group, "\n", "Enrollment:", data1$value, "\n", "Gender:", genderpoly)
        
        packing1 <- circleProgressiveLayout(data1$value, sizetype='area')
        data1 <- cbind(data1, packing1)
        dat1.gg <- circleLayoutVertices(packing1, npoints=50)
        dat1.gg$value <- rep(data1$value, each=51)
        
        bubble1 <- ggplot() + 
            
            # Make the bubbles
            #geom_polygon(data = dat1.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
            geom_polygon_interactive(data = dat1.gg, aes(x, y, group = id, fill=value, tooltip = data1$text[id], data_id = id), colour = "black", alpha = 0.6) +
            scale_fill_distiller(palette = "BuPu", direction = 1 ) +
            
            # Add text in the center of each bubble + control its size
            geom_text(data = data1, aes(x, y, size=2, label = group)) +
            scale_size_continuous(range = c(1,4)) +
            
            # General theme:
            theme_void() + 
            theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
            coord_equal()
        inBubble1 <- ggiraph(ggobj = bubble1, width_svg = 7, height_svg = 7)
    })
    
    output$Unienrollment2 <- renderGirafe({
        filterdata2 <- filter(data_uni_enrollment_course, Year == input$Year & Sex == input$gender2)
        
        if (input$gender2 == "MF"){
            genderuni <- "ALL"
        }
        else if (input$gender2 == "F"){
            genderuni <- "Female"
        }
        else{
            genderuni <- "Male"
        }
        
        data2 <- data.frame(group=paste(filterdata2$First_Degree), value=filterdata2$Enrollment) 
        data2$text <- paste("Course: ",data2$group, "\n", "Enrollment:", data2$value, "\n", "Gender:", genderuni)
        
        packing2 <- circleProgressiveLayout(data2$value, sizetype='area')
        data2 <- cbind(data2, packing2)
        
        dat2.gg <- circleLayoutVertices(packing2, npoints=50)
        dat2.gg$value <- rep(data2$value, each=51)
        
        bubble2 <- ggplot() + 
            
            # Make the bubbles
            #geom_polygon(data = dat2.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
            geom_polygon_interactive(data = dat2.gg, aes(x, y, group = id, fill=value, tooltip = data2$text[id], data_id = id), colour = "black", alpha = 0.6) +
            scale_fill_distiller(palette = "BuPu", direction = 1 ) +
            
            # Add text in the center of each bubble + control its size
            geom_text(data = data2, aes(x, y, size=2, label = group)) +
            scale_size_continuous(range = c(1,4)) +
            
            # General theme:
            theme_void() + 
            theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
            coord_equal()
        inBubble2 <- ggiraph(ggobj = bubble2, width_svg = 7, height_svg = 7)
        
    })
    
    output$ges_heatmap <- renderPlotly({
        ges3 <- filter(ges2, Year == input$Year2 & Hons == input$Hons)
        
        ges3 %>%
            plot_ly(x = ~Degree, y = ~University, z = ~Overall_Employment_Rate,
                    type = "heatmap", colors = "Greens", colorbar = list(title = "Overall Employment Rate"),
                    hoverinfo = "text",
                    text = ~paste("University:", University,
                                  "<br>Field of Study:", Degree,
                                  "<br>Overall Employment Rate:", Overall_Employment_Rate)) %>%
            layout(width = 1000,
                   margin = list(b = 200),
                   xaxis = list(tickangle = 55))
    })
}

### End of server ###

###

shinyApp(ui, server)
