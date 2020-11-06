
library(shiny)


ui <- fluidPage(

  titlePanel("Uploading Files and Display the Descriptives"),
  hr(),

  sidebarLayout(

    sidebarPanel(

      fileInput("file1", "Choose file with the type of .txt or .csv extension",
                multiple = FALSE,
                accept = c(".txt", ".csv")),
      hr(),

      radioButtons("sep", "Separator",
                   choices = c(Tab = "\t", Semicolon = ";", Comma = ",", Space = " "),
                   selected = "\t"),

      radioButtons("quote", "Quote",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                   selected = '"'),

      radioButtons("dec", "Decimal",
                   choices = c(None = "", "Dot" = '.', "Comma" = ","),
                   selected = '.'),

      radioButtons("header", "Header", 
                   choices = c("TRUE", "FALSE"), 
                   selected = "TRUE"),

      hr(),

      radioButtons("disp", "Display Dataset",
                   choices = c(Head = "head", Tail = "tail", All = "all"),
                   selected = "head", inline = TRUE),

    ),

    mainPanel(

      p(strong(em(h4("Dataset", style = "color:blue")))),
      tableOutput("dataset"),
    
      hr(),
      hr(),

      p(strong(em(h4("Descriptive Statistics of the Numerical Variables in the Dataset",
                      style = "color:blue")))),
      tableOutput("table")

    )

  )
)


server <- function(input, output) {

  output$dataset <- renderTable({

    req(input$file1)

    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = ifelse(input$header == "TRUE", TRUE, FALSE),
                       sep = input$sep,
                       quote = input$quote,
                       stringsAsFactors = TRUE,
                       dec = input$dec)
      },
      error = function(e) { 
        stop(safeError(e))
      }
    )

    if(input$disp == "head") { head(df) } 
      else if(input$disp == "tail") { tail(df) }
      else { df }
  })

  
  output$table <- renderTable({
  
    req(input$file1)

    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = ifelse(input$header == "TRUE", TRUE, FALSE),
                       sep = input$sep,
                       quote = input$quote,
                       stringsAsFactors = TRUE,
                       dec = input$dec)
      },
      error = function(e) { 
        stop(safeError(e))
      }
    )

    fdesc <- function(x) {
      n = length(x)
      m = mean(x)
      median = median(x)
      s = sd(x)
      var = var(x)
      skew = sum((x-m)^3/s^3)/n		
      kurt = sum((x-m)^4/s^4)/n - 3
      min = min(x)
      max = max(x)
      q = quantile(x)
      return(c(n = n, mean = m, median = median, sd = s,
               var = var, skewnes = skew, kurtosis = kurt,
               min = min, max = max, q))
    }

    k <- dim(df)[2]
    var1 <- logical()
    for(i in 1:k){ 
      var1[i] <- ifelse(is.numeric(df[,i]), TRUE, FALSE)}
    df2 <- df[, which(var1 == TRUE)]
    des <- sapply(df2, fdesc)
    round(des, 2)

  }, rownames = TRUE)

}

shinyApp(ui, server)