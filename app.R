library(stats)

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

shinyApp(
    ui = tagList(
        navbarPage(
            tabPanel("Navbar 1",
                     sidebarPanel(
                         tags$h5("The purpose of this application is to divide sparse data with a lot of missing values
                                 into multiple clusters, where missing values in each cluster are minimized"),
                         tags$h5(""),
                         tags$h5("You can simply test this tool with the data from the link below:
                                  https://github.com/JohnnyPeng123/SparseDataClustering/blob/master/data.csv"),
                         tags$h5(""),
                         tags$h5("Then set 'Title' as the 'Label column name', and 'NaN' as the 'Missing Value'"),
                         tags$h5(""),
                         tags$h5("Then you can click on the 'Show Table' button first, then click on 'Create Clusters' next"),
                         fileInput("file1", "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c(".csv")),
                         textInput("txt", "Label column name:", "ColumnName"),
                         actionButton("action2", "Show table"),
                         
                         tags$h5(""),
                         numericInput("cluster_n", "Number of Clusters:", 5),
                         
                         tags$h5(""),
                         textInput("missing", "Missing Value", "NaN"),
                         
                         tags$h5(""),
                         actionButton("action", "Create Clusters"),
                         
                         tags$h5("Choose a cluster to zoom in"),
                         conditionalPanel(
                             condition = "input.action > 0",
                             uiOutput("cluster_n"),
                         ),
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Tab 1",
                                      
                                      h4("Table (first 5 rows and 5 columns)"),
                                      tableOutput("table"),
                                      
                                      h4("Dendrogram (All clusters)"),
                                      plotOutput("dendrogram1"),
                                      
                                      h4("Dendrogram (choosen cluster)"),
                                      plotOutput("plotly1"),
                                      
                                      h4("Data from the choose cluster (first 5 rows and 5 columns)"),
                                      tableOutput("table2"),
                             )
                         )
                     )
            )
        )
    ),
    server = function(input, output) {
        
        output$cluster_n <- renderUI({
            sliderInput("slider", "Slider input:", 1, input$cluster_n, input$cluster_n, 1)
        })
        
        objects <- eventReactive(input$action2, {
                                    req(input$file1)
                                    req(input$txt)
                                    tryCatch(
                                        {
                                            data <- read.csv(input$file1$datapath)
                                        },
                                        error = function(e) {
                                            # return a safeError if a parsing error occurs
                                            stop(safeError(e))
                                        }
                                    )
                                    data_X = data[-which(colnames(data)==input$txt)]
                                    rownames(data_X) <- data[[input$txt]]
                                    
                                    data_copy=data_X
                                    for (i in 1:length(data_X)) {
                                        for (j in 1:nrow(data_X[i])){
                                            data_X[[i]][j] <- ifelse(data_X[[i]][j]==input$missing,0,1)
                                        }
                                    }
                                    
                                    hc <- hclust(dist(data_X), "ward.D2")
                                    
                                    list(data_X = data_X, data_copy=data_copy, hc = hc)
                      })
        
        output$table <- renderTable({return(objects()$data_copy[5:10,5:10])}, rownames = TRUE)
        
        observeEvent(input$action, 
        output$dendrogram1 <- renderPlot({
            hc <- objects()$hc
            # colored dendrogram
            op = par(bg = "#EFEFEF")
            return(A2Rplot(hc, k = input$cluster_n, boxes = FALSE, col.up = "gray50"))
            })
        )
        
        observeEvent(input$action, 
        output$plotly1 <- renderPlot({
        data_X <- objects()$data_X
        hc <- objects()$hc
        hc3 <- cutree(hc, k = input$cluster_n)
        x <- subset(data_X, rownames(data_X) %in% names(hc3)[as.numeric(hc3)==input$slider])
        hc_temp <- hclust(dist(x), "ward.D2")
        return(plot(hc_temp))
            })
        )
        
        observeEvent(input$action, 
        output$table2 <- renderTable({
            data_X <- objects()$data_X
            data_copy <- objects()$data_copy
            hc <- objects()$hc
            hc3 <- cutree(hc, k = input$cluster_n)
            y <- subset(data_X, rownames(data_copy) %in% names(hc3)[as.numeric(hc3)==input$slider])
            return(y[1:5,1:5])
         }, rownames = TRUE)
        )
        
        output$txtout <- renderText({
            paste(input$txt, input$slider, format(input$date), sep = ", ")
        })
    }
)