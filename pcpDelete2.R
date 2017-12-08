selDelIntShadePCP = function(pcpDat){
  
  ui <- basicPage(
    plotlyOutput("plot1"),
    verbatimTextOutput("rectdf")
  )
  
  server <- function(input, output) {
    
    colNms <- colnames(pcpDat[, c(2:(ncol(pcpDat)))])
    nVar <- length(colNms)
    
    p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(alpha=0) + xlim(0,(nVar-1)) +ylim(min(pcpDat[,2:(nVar+1)]),max(pcpDat[,2:(nVar+1)])) + xlab("Sample") + ylab("Count")
    gp <- ggplotly(p)
    
    pm <- tidyr::gather(pcpDat, variable, value, -ID)
    
    pm %>% 
      group_by(ID) %>%
      plot_ly(pm, x = ~variable, y = ~value, color = I())
    
    inputRectDf <- reactive({
      req(input$rects)
      # data comes back as a big character vector
      # so we reformat it as a dataframe here
      df <- data.frame(t(matrix(input$rects,nrow=8)))
      names(df) <- names(input$rects)[1:8]
      return(df)
    })
    output$rectdf <- renderPrint({print(inputRectDf())})
    
    output$plot1 <- renderPlotly({
      gp %>% onRender(paste(readLines("javascript.js"), collapse = "")
                      , data = list(pcpDat = pcpDat, nVar = nVar, colNms = colNms))})
    }
  shinyApp(ui, server)
}
