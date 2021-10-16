library(shiny)

ui <- basicPage(

  plotOutput("plot1"),

  inputPanel(
    sliderInput("mean", label = "Media 1:",
                min = -5, max = 5, value = 0, step = 1),

    sliderInput("sd", label = "Desviaci\u00f3n t\u00edpica 1:",
                min = 0.1, max = 5, value = 1, step = 0.1),

    sliderInput("mean2", label = "Media 2:",
                min = -5, max = 5, value = 0, step = 1),

    sliderInput("sd2", label = "Desviaci\u00f3n t\u00edpica 2:",
                min = 0.1, max = 5, value = 1, step = 0.1)
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({

    plot(seq(-10,10,0.01), dnorm(seq(-10,10,0.01),input$mean,input$sd),
         type="l", xlim = c(-8, 8), ylim=c(0, 1), lwd=3, col="deeppink",
         cex.lab=1.5, cex.main=1.5, cex.axis=1.5, axes=FALSE,
         main = "Distribuci\u00f3n Normal. X~N(Media, Desviaci\u00f3n T\u00edpica)",
         xlab ="x", ylab = "f(x)")
    curve(dnorm(x,input$mean2, input$sd2), lwd=3, add=TRUE, col="darkviolet")
    axis(1, at = seq(-8, 8, by = 1), cex.axis=1.5, pos=0)
    axis(2,at = seq(0, 1, by = 0.2), cex.axis=1.5, pos=0)
  })

}

shinyApp(ui, server)
