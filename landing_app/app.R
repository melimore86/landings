library("shiny")
library("ggplot2")
library("tidyverse")
library("shinythemes")
library("rsconnect")


#rsconnect::setAccountInfo(name='oysterprojectck',
#                          token='F16D04C1548412A5806A896A22342C97',
#                          secret='QoYg+iiKNQ8wysd6UfA18M4Y3YHortTMsTJT84eo')



data<- read.csv ("data/data.csv", header=T)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
              
              
   # Application title

   titlePanel("Landings Comparison"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("area1", label= h4("Area"), 
                    choices=unique(data$area), selected = "Apalach Landings"),
        
        selectInput("area2", label= h4("Comparison Area"), 
                    choices=unique(data$area), selected= "State Landings")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plotInput <- reactive({
    
      data <- data %>% 
        filter(area == input$area1|area == input$area2)
    
      
    ggplot(data=data, aes(x= Year, y= measurement)) +
      labs(x= "Year", y="Pounds")+
      geom_point( size=2)+
      geom_line(linetype = "dashed")+
      scale_x_continuous(limits=c(1986,2017), breaks=c(1986,1990,1994,1998,2002,2006,2010,2014,2018)) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1, size= 12)) +
      facet_wrap(~area, ncol=1, scales= "free_y")
    
    
   })
  
  output$plot <-renderPlot({
    plotInput()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

