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
library(packcircles)

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

#Enrollment by course
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

ges2 <- read_csv("data/cleaned/cleaned_GES2.csv", col_types = "ffffnniiiiii")

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
                    column(6,
                        sliderInput(inputId = "DateRange",
                            label = "Year Range",
                            min = 1993,
                            max = 2018,
                            value = c(1993, 2018),
                            sep = ""
                        )
                    ),
                    column(6,
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
                fluidRow(
                    column(6,
                           sliderInput(inputId = "Year",
                                       label = "Year",
                                       min = 1993,
                                       max = 2018,
                                       value = 2018,
                                       sep = ""
                           )
                    ),
                    column(6,
                           selectInput(inputId="gender2",label="Gender",choices = c("All"="MF",
                                                                                   "Male"="M",
                                                                                   "Female"="F"),
                                       selected = "All",multiple = F),
                    )
                ),
                
                fluidRow(
                    box(title = "Polytechnic (by course)", width = 6, solidHeader = TRUE, status = "primary", plotOutput("Polyenrollement2", height = 250)),
                    box(title = "University (by course)", width = 6, solidHeader = TRUE, status = "primary", plotOutput("Unienrollment2", height = 250))
                )
                
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
                    h2("How much can University Graduate expect to Earn?"),
                    fluidRow(
                        box(
                            width = 6,
                            # htmlOutput("selectInput_University")
                            selectInput("university", "University",
                                        choices = levels(ges$University),
                                        selected = "National University of Singapore")
                        ),
                        box(
                            width = 6,
                            # htmlOutput("selectInput_School")
                            selectInput("school", "School/Faculty",
                                        choices = NULL)
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            h3(textOutput("ges_mean_salary_by_degree_title")),
                            plotlyOutput("ges_mean_salary_by_degree", width = "85%")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            h3(textOutput("ges_salary_percentile_title")),
                            plotlyOutput("ges_salary_percentile", width = "85%")
                        )
                    ),
                    #heatmap
                    fluidRow(
                        column(4,
                               sliderInput(inputId = "Year2",
                                           label = "Year",
                                           min = 2013,
                                           max = 2018,
                                           value = 2018,
                                           sep = ""
                               )
                        ),
                        column(4,
                               selectInput(inputId="school",label="Faculty/School/Field of Study", choices = c("Accountancy" = "Accountancy",
                                                                                                               "Arts & Social Sciences" = "Arts & Social Sciences", 
                                                                                                               "Business" = "Business", 
                                                                                                               "Computing and Information Systems" = "Computing and Information Systems", 
                                                                                                               "Dentistry" = "Dentistry", 
                                                                                                               "Design & Environment" = "Design & Environment", 
                                                                                                               "DigiPen Institute of Technology" = "DigiPen Institute of Technology" , 
                                                                                                               "Economics" = "Economics", 
                                                                                                               "Engineering" = "Engineering", 
                                                                                                               "Law" = "Law", 
                                                                                                               "Medicine" = "Medicine", 
                                                                                                               "Multidisciplinary" = "Multidisciplinary", 
                                                                                                               "National Institute of Education (NIE)" = "National Institute of Education (NIE)", 
                                                                                                               "Newcastle University" = "Newcastle University", 
                                                                                                               "Sciences" = "Sciences", 
                                                                                                               "Singapore Institute of Technology" = "Singapore Institute of Technology", 
                                                                                                               "Technische Universität München" = "Technische Universität München", 
                                                                                                               "The Culinary Institute of America" = "The Culinary Institute of America", 
                                                                                                               "Trinity College Dublin" = "Trinity College Dublin", 
                                                                                                               "University of Glasgow" = "University of Glasgow", 
                                                                                                               "University of Liverpool" = "University of Liverpool", 
                                                                                                               "University of Manchester" = "University of Manchester", 
                                                                                                               "University of Nevada, Las Vegas" = "University of Nevada, Las Vegas", 
                                                                                                               "Wheelock College" = "Wheelock College"),
                                           selected = "Engineering",multiple = F)
                               
                        ),
                        column(4,
                               selectInput(inputId="Hons",label="Honors/Cum Laude",choices = c("Yes"="Yes",
                                                                                               "No"="No"),
                                           selected = "No",multiple = F)
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            plotlyOutput("ges_heatmap", width = "85%")
                        )
                    ),
            )
        )
            
    )
)

server <- function(session, input, output) {
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
    
    # Graduate Employment Survey
    observe({
        ges_university_school <- ges %>%
            filter(Year == 2018,
                   University == input$university) %>%
            droplevels()
        
        updateSelectInput(session, "school", choices = levels(ges_university_school$School))
    })
    
    # output$selectInput_University <- renderUI({
    #     selectInput("university", "University", choices = levels(ges$University), selected = "National University of Singapore")
    # })
    # 
    # output$selectInput_School <- renderUI({
    #     ges_university_school <- ges %>%
    #         filter(University == input$university) %>%
    #         droplevels()
    #     
    #     selectInput("school", "School/Faculty", choices = levels(ges_university_school$School), selected = "School of Computing")
    # })
    
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
    
    # output$ges_salary_percentile_title <- renderText({
    #     ges_event <- event_data(event = "plotly_click", source = "degree")
    #     
    #     if (is.null(ges_event)) {
    #         "Salary Percentiles for Bachelor of Computing (Computer Science) Degree"
    #     } else {
    #         paste("Salary Percentiles for", ges_event$key, "Degree")
    #     }
    # })
    
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
            paste("Gross Monthly Mean Salary by Degree(s) under", input$school, "in", input$university)
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
    
    # output$ges_salary_percentile <- renderPlotly({
    #     ges_event <- event_data(event = "plotly_click", source = "degree")
    # 
    #     if (is.null(ges_event)) {
    #         ges_time <- ges %>%
    #             filter(University == input$university,
    #                    School == input$school) %>%
    #             droplevels() %>%
    #             filter(Degree == sample(Degree, 1)) %>%
    #             droplevels()
    #     } else {
    #         ges_time <- ges %>%
    #             filter(University == input$university,
    #                    School == input$school,
    #                    Degree == ges_event$key) %>%
    #             droplevels()
    #     }
    #     
    #     ges_time %>%
    #         plot_ly(x = ~Year) %>%
    #         add_trace(y = ~`Gross Monthly 25th Percentile`, name = "Gross Monthly 25th Percentile", type = "scatter", mode = "lines+markers",
    #                   hoverinfo = "text",
    #                   text = ~paste0("Degree: ", Degree, "<br>Year: ", Year, "<br>25th Percentile: $", `Gross Monthly 25th Percentile`)) %>%
    #         add_trace(y = ~`Gross Monthly Median`, name = "Gross Monthly Median", type = "scatter", mode = "lines+markers",
    #                   hoverinfo = "text",
    #                   text = ~paste0("Degree: ", Degree, "<br>Year: ", Year, "<br>Median: $", `Gross Monthly Median`)) %>%
    #         add_trace(y = ~`Gross Monthly 75th Percentile`, name = "Gross Monthly 75th Percentile", type = "scatter", mode = "lines+markers",
    #                   hoverinfo = "text",
    #                   text = ~paste0("Degree: ", Degree, "<br>Year: ", Year, "<br>75th Percentile: $", `Gross Monthly 75th Percentile`)) %>%
    #         layout(title = ~Degree, yaxis = list(title = "Salary"))
    # })
    
    ###
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    ###start of enrollment
    output$Polyenrollement1 <- renderPlot({
        
        poly1 <- data_poly_enrollment_institute  %>%  filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>% ggplot(aes(x=Year, y = Enrollment))
        poly1 <- poly1 + geom_line(aes(colour = Institute))
        poly1
    })
    
    output$Unienrollment1 <- renderPlot({
        
        uni1 <- data_uni_enrollment_institute  %>%  filter(Year >= input$DateRange[1] & Year <= input$DateRange[2] & Sex == input$gender) %>% ggplot(aes(x=Year, y = Enrollment))
        uni1 <- uni1 + geom_line(aes(colour = Institute))
        uni1
        
    })
    
    output$Polyenrollement2 <- renderPlot({
        filterdata1 <- filter(data_poly_enrollment_course, Year == input$Year & Sex == input$gender2)
        
        data1 <- data.frame(group=paste("Group", filterdata1$First_Degree), value=filterdata1$Enrollment) 
        data1$text <- paste("name: ",data1$group, "\n", "value:", data1$value)
        
        packing1 <- circleProgressiveLayout(data1$value, sizetype='area')
        data1 <- cbind(data1, packing1)
        dat1.gg <- circleLayoutVertices(packing1, npoints=50)
        
        ggplot() + 
            
            # Make the bubbles
            geom_polygon(data = dat1.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
            
            # Add text in the center of each bubble + control its size
            geom_text(data = data1, aes(x, y, size=2, label = group)) +
            scale_size_continuous(range = c(1,4)) +
            
            # General theme:
            theme_void() + 
            theme(legend.position="none") +
            coord_equal()
    })
    
    output$Unienrollment2 <- renderPlot({
        filterdata2 <- filter(data_uni_enrollment_course, Year == input$Year & Sex == input$gender2)
        
        data2 <- data.frame(group=paste("Group", filterdata2$First_Degree), value=filterdata2$Enrollment) 
        
        packing2 <- circleProgressiveLayout(data2$value, sizetype='area')
        data2 <- cbind(data2, packing2)
        
        dat2.gg <- circleLayoutVertices(packing2, npoints=50)
        ggplot() + 
            
            # Make the bubbles
            geom_polygon(data = dat2.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
            
            # Add text in the center of each bubble + control its size
            geom_text(data = data2, aes(x, y, size=value, label = group)) +
            scale_size_continuous(range = c(1,4)) +
            
            # General theme:
            theme_void() + 
            theme(legend.position="none") +
            coord_equal()
        
    })
    #end of enrollment
    
    #heat map for dashboard4
    output$ges_heatmap <- renderPlotly({
        ges2 <- filter(ges2, Year == input$Year2 & School == input$school)
        p <- plot_ly(x=ges2$Degree, y=ges2$University,
                     z = ges2$Overall_Employment_Rate,
                     type = "heatmap",
                     colorscale= "Blue")
        #            showscale = F) %>%
        # layout(margin = list(l=1000))
    })
}

shinyApp(ui, server)
