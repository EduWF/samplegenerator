#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(dplyr)
library(DT)
library(writexl)

# Define UI for application 
ui <- fluidPage(

  # Application title
  titlePanel("Reproducible random sample generator"),
  
  helpText("Draw a random sample of participants in an event / activity based",
           "on their order on an attendance list.",
           "This app generates a reproducible random",
           "sample list of numbers. You can identify participants by",
           "matching a numbered attendance list of participants to the column",
           "'Participant' below.",
           "You can invite participants to answer a feedback survey individually or in groups.",
           "Samples are reproducible with the function 'set.seed(44343)' using R.",
           "Learn more about reproducible sampling at:", 
           a("https://movimentar.eu", 
                                 href="https://movimentar.eu/reproducible-random-sample-generator-in-shinyapps-for-improved-data-and-learning/", target="_blank")
           ),
  
  # Sidebar with a slider input for number of participants and %
  sidebarLayout(
    sidebarPanel(
      helpText("Please inform the total number of participants in the event / activity and",
               "a sample percentage."),
      sliderInput("participants",
                  "Total number of participants:",
                  min = 1,
                  max = 300,
                  value = 70),
      
      sliderInput("percentage",
                  "Desired sample percentage (%):",
                  min = 1,
                  max = 100,
                  value = 30),
      
      # Button
      downloadButton("download", "Download (.xlsx)")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::dataTableOutput("sample"),
      wellPanel(
        helpText("Author:", 
                 a("Dr. rer. pol. Eduardo W. Ferreira (PhD)", 
                   href="https://movimentar.eu/about/eduardo/", target="_blank"),
                 "- Source code available for download at ",    
          a("Github.",     
            href="https://github.com/movimentar/samplegenerator", target="_blank")
        )
      ) 
    )
  )
)

###################################################
# Define server 
server <- function(input, output) {
  
  output$sample <- renderDataTable({
    set.seed(44343)
    table <- tbl_df(sample(1:input$participants, 
                           size = c(input$participants * (input$percentage / 100))
    )
    )  %>% rename(Participant = value) %>% 
      arrange(Participant)
    table
  })
  
  
  output$download <- downloadHandler(
    filename = function(){paste("Sample ", Sys.Date(),".xlsx")}, 
    content = function(fname){
      set.seed(44343)
      writexl::write_xlsx(
        tbl_df(sample(1:input$participants, 
                      size = c(input$participants * (input$percentage / 100))
        )
        ) %>% 
          rename(Participant = value) %>% 
          arrange(Participant), 
        fname)
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)

