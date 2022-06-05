library(dplyr)
library(shiny)
library(RSQLite)
library(writexl)

# increase the maximum file size to 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)


# Define UI for data upload app

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      h3 {
        color: #48ca3b
      }
      
      .shiny-input-container {
        padding-top: 30px
      }
      
      .col-sm-offset-4 {
        padding-bottom: 5px
      }
      
      label {
        font-weight: 400
      }
    "))
  ),
  
  # App title
  titlePanel(
    h3("Kindle Vocabulary Builder Export For Free", align = "center"),
    windowTitle = "Kindle Vocabulary Builder Export For Free"
  ),
    
  # panel for uploads
  fluidRow(
     column(
       width = 4,
       offset = 4,
       # Input: Select a file
       fileInput("file", "Step 1: Upload your vocab.db file:",
                 buttonLabel = "Upload...",
                 multiple = FALSE,
                 accept = c(".db"))
     ) 
    ),
  
  fluidRow(
    column(
      width = 4,
      offset = 4,
      "Step 2: Export to an Excel file:"
    )
  ),
    
  # panel for downloads
  fluidRow(
    column(
      width = 4,
      offset = 4,
      # Download button
      downloadButton("downloadData", "Download") 
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  
  # input$file will be NULL initially. 
  
  data <- eventReactive(input$file, {
    
    
    
    db <- dbConnect(drv = dbDriver("SQLite"), dbname = input$file$datapath)
    
    
    book_info <- dbReadTable(db, "BOOK_INFO")
    # dict_info <- dbReadTable(db, "DICT_INFO")
    lookups <- dbReadTable(db, "LOOKUPS")
    # metadata <- dbReadTable(db, "METADATA")
    version <- dbReadTable(db, "VERSION")
    words <- dbReadTable(db, "WORDS")
    
    my_lookups <- 
      lookups %>% 
      left_join(words, by = c("word_key" = "id")) %>% 
      left_join(book_info, by = c("book_key" = "id")) %>% 
      select(id, word, usage, book = title, authors, timestamp = timestamp.x)
    
    my_lookups %>% 
      count(word, name = "lookups", sort = TRUE) %>% 
      left_join(my_lookups) %>% 
      select(word, lookups, usage, book, authors) 
    
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("vocab-", Sys.Date(), ".xlsx", sep="")
    },
    
    content = function(file) {
      write_xlsx(data(), file)
    }
  )
  
}
# Run the app ----
shinyApp(ui, server)

