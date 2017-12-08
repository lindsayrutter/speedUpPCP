ui <- basicPage(
  plotlyOutput("plot1"),
  verbatimTextOutput("selection")
)

pm <- tidyr::gather(pcpDat, variable, value, -ID)

server <- function(input, output, session) {
  
  output$plot1 <- toWebGL(renderPlotly({
    pm %>% 
    group_by(ID) %>%
    plot_ly(x = ~variable, y = ~value, color = I("red"), key = ~ID) %>%
    add_trace(type = "scatter", mode = "lines+markers", marker = list(size = 0.01)) %>%
    layout(dragmode = "select")
  }))
  
  rv <- reactiveValues(
    data = pm
  )
  
  observeEvent(event_data("plotly_selected"), {
    
    # dim the "base" layer
    plotlyProxy("plot1", session) %>%
    plotlyProxyInvoke("restyle", list(opacity = 0.1, hoverinfo = "none"))
    
    # determine what IDS need to be shown
    keys <- unlist(event_data("plotly_selected")[["key"]])
    rv$data <- rv$data[rv$data[["ID"]] %in% keys, ]
    d <- group2NA(rv$data, groupNames = "ID", ordered = "variable")
    
    plotlyProxy("plot1", session) %>%
    plotlyProxyInvoke("deleteTraces", list(1))
    
    plotlyProxy("plot1", session) %>%
    plotlyProxyInvoke("addTraces", list(x = d$variable, y = d$value, marker = list(size = 0.01), line = list(color = "red")))
  })
  
  # go back to the original state
  observeEvent(event_data("plotly_relayout"), {
    
    plotlyProxy("plot1", session) %>%
    plotlyProxyInvoke("restyle", list(opacity = 1, hoverinfo = "x+y"))
    
    rv$data <- pm
  })
}

shinyApp(ui, server)
