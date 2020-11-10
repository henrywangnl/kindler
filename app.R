library(dplyr)
library(shiny)
library(RSQLite)
library(writexl)



# Define UI for data upload app ----


ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Your vocab.db file"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose vocab.db File",
                buttonLabel = "Upload...",
                multiple = FALSE,
                accept = c(".db"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Download button
      downloadButton("downloadData", "Download")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  data <- eventReactive(input$file1, {
    
    
    
    db <- dbConnect(drv = dbDriver("SQLite"), dbname = input$file1$datapath)
    
    
    book_info <- dbReadTable(db, "BOOK_INFO")
    dict_info <- dbReadTable(db, "DICT_INFO")
    lookups <- dbReadTable(db, "LOOKUPS")
    metadata <- dbReadTable(db, "METADATA")
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

