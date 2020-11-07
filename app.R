
library(shiny)


ui <- fluidPage(

  titlePanel(strong("Uploading Files and Display the Descriptives")),
  hr(),

  sidebarLayout(

    sidebarPanel(

      fileInput("file1", h4(strong("Choose file (with the type of .txt or .csv extension)")),
                multiple = FALSE,
                accept = c(".txt", ".csv")),
      hr(),

      radioButtons("sep", "Separator",
                   choices = c(Tab = "\t", Semicolon = ";", Comma = ",", Space = " "),
                   selected = "\t", 
                   inline = TRUE),
      br(),

      radioButtons("quote", "Quote",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                   selected = '"', 
                   inline = TRUE),
      br(),

      radioButtons("dec", "Decimal",
                   choices = c("Dot" = '.', "Comma" = ","),
                   selected = '.', 
                   inline = TRUE),
      br(),

      radioButtons("header", "Header", 
                   choices = c("TRUE", "FALSE"), 
                   selected = "TRUE", 
                   inline = TRUE),
      hr(),

      radioButtons("disp", "Display Dataset",
                   choices = c(Head = "head", Tail = "tail", All = "all"),
                   selected = "head", 
                   inline = TRUE),

    ),

    mainPanel(
   
      tabsetPanel(type = "pills",

        tabPanel(title = h4(strong("Dataset")),
          hr(),
          p(strong(em(h4("Dataset", style = "color:blue")))),
          hr(),
          tableOutput("dataset")),
 
        tabPanel(title = h4(strong("Descriptives")), 
          hr(),
          p(strong(em(h4("Descriptive statistics of the numerical variables in the dataset",
            style = "color:blue")))),
          hr(),
          tableOutput("table")),

        tabPanel(h4(strong("Help")), 
          hr(),
          p(strong(em(h4("How to use this application:", style = "color:blue")))),
          hr(),         
          p(strong("UPLOAD:"), "By clicking 'Browse', you can select the data file in your computer. 
            and then upload the data files with the extention of '.txt' or '.csv'."),
          br(),
          p(strong("SEPARATOR:"), "Select the separator symbol of your dataset. 
            It can be 'tab (\t)', 'column (,)', 'semicolon (;)' or 'space(' ')'. 
            If you select the correct one, you can see your dataset as correctly formed at the right-side area."),
          br(),     
          p(strong("QUOTE:"), "In your dataset, values can be seperated each other with quote symbol, like sigle or double quote. 
            If you select the correct one, you can see your dataset as correctly formed at the right-side area."),
          br(),   
          p(strong("DECIMAL:"), "If there are decimal values in your dataset, you should define the decimal character. 
            It can be 'comma (,)' or 'dot (.)'. 
            If you select the correct one, you can see your decimal values with dot in the right-side area."),
          br(),    
          p(strong("HEADER:"), "If there is a head-line in your dataset, you should select 'TRUE', otherwise 'FALSE'.
            You can test this by considering the display at the right-side area."),
          br(),      
          p(strong("DISPLAY:"), "You can display your dataset with just first rows or last rows. 
            Or you can display all rows of your dataset.")
        )

      )

    )

  )

)


server <- function(input, output, session) {

  values <- reactiveValues()
  values$df <- df

  observe({

    req(input$file1)

    tryCatch(
      {
      values$df <<- read.csv(input$file1$datapath,
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

  })


  output$dataset <- renderTable({

    req(input$file1)

    if(input$disp == "head") { head(values$df, 10) } 
      else if(input$disp == "tail") { tail(values$df, 10) }
      else { values$df }

  }, rownames = TRUE, bordered = TRUE, striped = TRUE, hover = TRUE)

  
  output$table <- renderTable({

    req(input$file1)

    fdesc <- function(x) {
      n = length(x)
      missing = sum(is.na(x))
      m = mean(x)
      s = sd(x)
      var = var(x)
      median = median(x)
      min = quantile(x)[[1]]
      Q1 = quantile(x)[2]
      Q2 = quantile(x)[3]
      Q3 = quantile(x)[4]
      max = quantile(x)[[5]]
      IQR = IQR(x)
      mad = mad(x)
      skew = sum((x-m)^3/s^3)/n		
      kurt = sum((x-m)^4/s^4)/n - 3

      return(c(n = n, missing = missing,
               mean = m, sd = s, var = var, 
               median = median, min = min, Q1 = Q1,
               Q2 = Q2, Q3 = Q3, max = max, 
               IQR = IQR, mad = mad,
               skewness = skew, kurtosis = kurt))
    }

    k1 <- dim(values$df)[2]
    var1 <- logical()
    for(i in 1:k1){ 
      var1[i] <- ifelse(is.numeric(values$df[,i]), TRUE, FALSE)}
    df2 <- as.data.frame(values$df[, which(var1 == TRUE)])
    names(df2) <- names(values$df)[which(var1 == TRUE)]
    
    k2 <- dim(df2)[1]
    k3 <- dim(df2)[2]
    if(k3 > 1 && sum(sort(df2[,1]) == 1:k2) == k2){ 
      df3 <- as.data.frame(df2[,2:k3])
      names(df3) <- names(df2)[2:k3]
    }
    if(k3 > 2  && sum(sort(df2[,2]) == 1:k2) == k2){
      df3 <- as.data.frame(df2[,3:k3])
      names(df3) <- names(df2)[3:k3]
    }
    if(sum(sort(df2[,1]) == 1:k2) < k2) df3 <- df2

    des <- sapply(df3, fdesc)

  }, rownames = TRUE, digits = 2, bordered = TRUE, striped = TRUE, hover = TRUE)

}

shinyApp(ui, server)
