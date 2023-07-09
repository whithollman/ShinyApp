#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

  titlePanel("SAT Student Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("test", "Discipline:",
                  choices = c("Math Total", "Reading Total", "Writing Total"),
                  selected = "Math"),
      sliderInput("score", "Score:",
                  min = 15, max = 100, value = c(20)),
      plotOutput("")

    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("static")
    )
  )
)
 server <- function(input, output, session) {
   output$static <- renderTable(head(result_unique))

   filteredData <- reactive({
     result_unique %>%
      filter(gender_female == input$gender,
             gender_male == input$gender,
             math_score >= input$test,
             reading_score <= input$test,
             writing_score >= input$test,
             total_score >= input$score[1],
             total_score <= input$score[2],
             lunch %in% input$lunch)
     })

   output$plot <- renderPlot({
     filteredData <- input$filteredData
     score <- input$score
     lunch <- input$lunch
     gender <- input$gender
     test <- input$test

     ggplot(result_unique, mapping = aes(x = parental_level_of_education,
                                         y = score,
                                         shape = lunch)) +
       geom_point() +
       scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
       geom_jitter(mapping = aes(color = gender)) +
       theme_ipsum() +
       ggtitle("Student Performance") +
       theme(axis.text.x = element_text(angle = 45, vjust = 0.5,
                                        hjust = 0.5, size = 10)) +
       theme(axis.text.y = element_text(angle = 45, hjust = 0.5, size = 10)) +
       labs(
         x = toupper("Parent Education Level"),
         y = toupper("Student Total Score per Subject"),
         color = toupper("Gender"),
         shape = toupper("Lunch Type"))

   }, res = 95)

 }

# Run the application
shinyApp(ui = ui, server = server)
