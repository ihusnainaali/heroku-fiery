library(shiny)
library(rvest)
library(stats)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(shinycssloaders)


# UI Part

##
ui <- fluidPage(
  titlePanel("Web Scrapping"),
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter URL", value = '', width = NULL, placeholder = "URL of website to read data..."),
      textInput("css", "Enter Css", value = '', width = NULL, placeholder = "Css of website to read data..."),
      radioButtons("productInput", "What Kind of product data you want?",
                   choices = c("Name", "Price", "Name and price", "Complete Detail"),
                   selected = "Complete Detail"),
      
    ),
    mainPanel(
      h3("Parsed query string"),
      verbatimTextOutput("queryText"),
      br(),br(),br(),
      tableOutput("results") %>% withSpinner()
    )
  )
)

# Server Part
server = function(input, output, session) {
  
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
  
  result <- reactive({
    if (input$url == "") {
      return()
    }
    
    if (input$css == "") {
      if (input$productInput == "") {
        return()
      }else{
        if (input$productInput == "Name") {
          Css <- c(".BiggerText a")
        }else if (input$productInput == "Price") {
          Css <- c(".PriceFont")
        }else if (input$productInput == "Name and price") {
          Css <- c(".BiggerText a", ".PriceFont")
        }else if (input$productInput == "Complete Detail") {
          Css <- c(".BiggerText a", ".PriceFont")
        }
      }
    }else{
      Css <- c(input$css)
    }
    
    URL <- input$url
    res <- StartScraping(URL, Css)
  })
  
  output$results <- renderTable({
    if (is.null(result())) {
      return()
    }
    
    result()
  })
}

StartScraping  <- function(url = '', css = c()) {
  # Start Scraping
  print(url)
  print(css)
  trails_webpage <- read_html(url)
  
  if (length(css) == 1) {
    print("1")
    
    ouputName <- trails_webpage %>% html_nodes(css[1]) %>% html_text()
    
    index = NULL
    for (value in 1:length(ouputName)) {
      if(ouputName[value] == "\n") {
        index = append(index, value)
      }
    }
    
    Name <- ouputName[ouputName != "\n"]
    datFrame <- data.frame(Name)
    
  }else if (length(css) == 2) {
    
    print("2")
    trail_names_html <- html_nodes(trails_webpage, css[1])
    outputName <- html_text(trail_names_html)
    
    trail_names_html <- html_nodes(trails_webpage, css[2])
    outputPrice <- html_text(trail_names_html)
    
    Prices = NULL
    Names = NULL
    for (value in 1:length(outputName)) {
      if(outputName[value] != "\n") {
        Prices <- append(Prices, outputPrice[value])
        Names <- append(Names, outputName[value])
      }
    }
    
    datFrame <- data.frame(Names, Prices)
  }else if (length(css) == 3) {
    print("3")
    trail_names_html <- html_nodes(trails_webpage, css[1])
    outputName <- html_text(trail_names_html)
    
    trail_names_html <- html_nodes(trails_webpage, css[2])
    outputPrice <- html_text(trail_names_html)
    
    outputLinks <- paste0(url, trails_webpage %>%  html_nodes( paste(css[3], "a", sep = " ")) %>%  html_attr("href"))
    
    Prices = NULL
    Names = NULL
    URLs = NULL
    
    for (value in 1:length(outputName)) {
      print(outputName[value])
      if(outputName[value] != "\n") {
        Prices <- append(Prices, outputPrice[value])
        Names <- append(Names, outputName[value])
        URLs <- append(URLs, outputLinks[value])
      }
    }
    
    datFrame <- data.frame(Names, Prices, URLs)
  }
}

# Start An App
shinyApp(ui = ui, server = server)
