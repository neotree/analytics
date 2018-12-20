library(shiny)
library(ggplot2)
library(ggforce)

theme_liam <- function () { 
  theme_bw(base_size=12) %+replace% 
    theme(
      axis.text=element_text(colour="black")
    )
}

# Read in data
source('read-json.R')

# Sepsis by week
require(reshape2)
sepsis.by.week <- data.frame(df.plot %>% group_by(WeekAdmission) %>% summarise(length(Diagnoses[which(Diagnoses=="SEPS")]), nonsepsis=length(Diagnoses[which(Diagnoses!="SEPS")])))
colnames(sepsis.by.week) <- c("WeekAdmission", "Sepsis", "NonSepsis")
sepsis.by.week <- sepsis.by.week[!is.na(sepsis.by.week$WeekAdmission),]
sepsis.by.week <- melt(sepsis.by.week)


ui <- fluidPage(
  # Input functions
  sliderInput(inputId = "threshold", 
              label = "Choose a temperature threshold",
              min = 35, 
              max = 37.5,
              step = 0.1, 
              value = 36.5),
  plotOutput("sepsisPlot"),
  plotOutput("hist"),
  plotOutput("dischargePlot")
)

server <- function(input, output) {
  output$hist <- renderPlot({ 
    p <- ggplot(df.plot[which(!is.na(df.plot$WeekAdmission)),], aes(WeekAdmission, as.numeric(Temperature)))+
      geom_hline(yintercept=input$threshold, linetype="dashed", size=1, colour="red")+
      stat_summary(fun.y="mean", fun.ymin="mean", fun.ymax="mean", size=1, geom="crossbar")+
      ylab("Temperature on admission")+
      xlab("Week of admission")+
      theme_liam()
    p + geom_sina(size=4, maxwidth=1)
  })
  output$sepsisPlot <- renderPlot({
    ggplot(sepsis.by.week, aes(WeekAdmission, value, fill=variable))+
      geom_bar(position="dodge", stat="identity")+
      theme_liam()+labs(fill="Diagnosis")+
      xlab("Week of admission")+
      ylab("Number of patients")+
      theme(legend.position="top")
  })
  output$dischargePlot <- renderPlot({
    
    
  })
}

shinyApp(ui = ui, server = server)