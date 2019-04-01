library(shiny)
library(shinydashboard)
library(networkD3)
library(ggplot2)
library(dplyr)

my_color <- 'd3.scaleOrdinal() .domain(["_NULL", "_AUTO","_BOTTOM","_COLD","_NEW","_TOP","_LOST","_WARM","my_unique_group"]) .range(["aquamarine","darkorange","darkmagenta","blue","gold","yellow","green","red","grey"])'

#SHINY APP

ui <- dashboardPage(
  dashboardHeader(title = "Assignment 3"),
  dashboardSidebar(),
  
  dashboardBody(
    fluidPage(
      selectInput("year", "Transition Matrix Pedriod:",
                  c("2008-2009" = "2009",
                    "2009-2010" = "2010",
                    "2010-2011" = "2011",
                    "2011-2012" = "2012",
                    "2012-2013" = "2013",
                    "2013-2014" = "2014",
                    "2014-2015" = "2015",
                    "2015-2016" = "2016",
                    "2016-2017" = "2017",
                    "2017-2018" = "2018")),
    selectInput("Segment", "Query:",
                c("New DO" = 2,
                  "New PA" = 3,
                  "Top" = 4,
                  "Bottom" = 5,
                  "Cold" = 6,
                  "Warm" = 7,
                  "Lost" = 8,
                  "Auto" = 9)),
    radioButtons("Variable",label="Number of (/amount):",
                 choices = c("WOMEN"="WOMEN","MEN"="MEN",
                             "NOTSURE"="NOTSURE","ZOMBIE"="ZOMBIE","MAILING"="MAILING","PRELEVEMENT"="PRELEVEMENT","INTERNET"="INTERNET",
                             "CHEQUE"="CHEQUE","CARTE_BANCAIRE"="CARTE_BANCAIRE","Average Amount"="avg","Max Amount"="max"),inline=TRUE)),
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Transition Matrix", sankeyNetworkOutput("plot")),
                  tabPanel("Evolution of Segments", plotOutput("plot2"))
                
      )
      )
    
    
  )
)

server <- function(input, output) {
  
  output$plot <- renderSankeyNetwork({
    datainput = data[which(data$year == input$year),]
    links=data.frame(source = datainput$old_period, target = datainput$new_period, value = datainput$value)
    nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
    nodes$group=as.factor(c("my_unique_group"))
    links$IDsource=match(links$source, nodes$name)-1 
    links$IDtarget=match(links$target, nodes$name)-1
    links$group=as.factor(datainput$old_period)
    sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale=my_color, LinkGroup="group", NodeGroup="group")
  })
  output$plot2 <- renderPlot({
    y=input$Variable
    
    # Render a barplot
    ggplot(data2[which(data2$query == input$Segment),], aes_string(x="period_id", y=y)) + geom_line() + 
      labs(x="Year", y="Variable Selected")+ scale_x_continuous(breaks=c(2008:2018))
  })
}

shinyApp(ui, server)
