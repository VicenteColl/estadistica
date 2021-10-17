
ui <- dashboardPage(

  skin = "yellow",

  dashboardHeader(title = "Distribuciones de probabilidad",
                  titleWidth = 300),

  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Binomial", tabName = "Binomial"),
                conditionalPanel(
                  'input.sidebarid == "Binomial"',
                  sliderInput("n", label = "n:",
                              min = 1, max = 10, value = 5, step = 1),

                  sliderInput("p", label = "p:",
                              min = 0.1, max = 0.9, value = 0.5, step = 0.1)),

                menuItem("Poisson", tabName = "Poisson"),
                conditionalPanel(
                  'input.sidebarid == "Poisson"',
                  sliderInput("lambda", label = "lambda:",
                              min = 0.1, max = 10, value = 2, step = 0.1)),

                menuItem("Uniforme", tabName = "Uniforme"),
                conditionalPanel(
                  'input.sidebarid == "Uniforme"',
                  sliderInput("min", label = "min:",
                              min = 0, max = 10, value = 2, step = 1),

                  sliderInput("max", label = "max:",
                              min = 0, max = 10, value = 6, step = 1)),

                menuItem("Exponencial", tabName = "Exponencial"),
                conditionalPanel(
                  'input.sidebarid == "Exponencial"',
                  sliderInput("rate", label = "lambda:",
                              min = 0.1, max = 8, value = 2, step = 0.1)),

                menuItem("Normal", tabName = "Normal"),
                conditionalPanel(
                  'input.sidebarid == "Normal"',
                  sliderInput("mean", label = "media:",
                              min = -5, max = 5, value = 0, step = 1),

                  sliderInput("sd", label = "desviacion típica:",
                              min = 0.1, max = 5, value = 1, step = 0.1))
    )
  ),

  # body ----
  dashboardBody(
    tabItems(
      # pagina 1 ----
      tabItem(tabName = "Binomial",
              #br(),
              plotOutput("plot.binomial")),
      # pagina 2 ----
      tabItem(tabName = "Poisson",
              #br(),
              plotOutput("plot.poisson")),
      # pagina 3 ----
      tabItem(tabName = "Uniforme",
              #br(),
              plotOutput("plot.uniforme")),
      # pagina 4 ----
      tabItem(tabName = "Exponencial",
              #br(),
              plotOutput("plot.exponencial")),
      # pagina 5 ----
      tabItem(tabName = "Normal",
              #br(),
              plotOutput("plot.normal"))
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {

  output$plot.binomial <- renderPlot({
    barplot(dbinom(0:10,input$n,input$p), names.arg = c(0:10),
            xlim = c(0, 13), ylim=c(0, 1),
            main = "Distribuci\u00f3n Binomial. X~Bi(n, p)",
            xlab ="x", ylab = "P(x)",
            col = "orange",
            border = "white")
  })

  output$plot.poisson <- renderPlot({
    barplot(dpois(0:10,input$lambda), names.arg = c(0:10),
            xlim = c(0, 13), ylim=c(0, 1),
            main = "Distribuci\u00f2n de Poisson. X~P(lambda)",
            xlab ="x", ylab = "P(x)",
            col = "darkred",
            border = "white")
  })

  output$plot.uniforme <- renderPlot({
    plot(seq(0,10,0.01),dunif(seq(0,10,0.01),input$min,input$max),
         type="l", xlim = c(0, 10), ylim=c(0, 1), lwd=3,
         main = "Distribuci\u00f3n Uniforme. X~U(min, max)",
         xlab ="x", ylab = "f(x)",
         col = "darkblue")
  })

  output$plot.exponencial <- renderPlot({
    plot(seq(0,15,0.1), dexp(seq(0,15,0.1),input$rate),
         type="l", xlim = c(0, 15), ylim=c(0, 1), lwd=3,
         main = "Distribuci\u00f3n Exponencial. X~Exp(lambda)",
         xlab ="x", ylab = "f(x)",
         col = "darkgreen")
  })

  output$plot.normal <- renderPlot({
    plot(seq(-10,10,0.01), dnorm(seq(-10,10,0.01),input$mean,input$sd),
         type="l", xlim = c(-10, 10), ylim=c(0, 1), lwd=3,
         main = "Distribuci\u00f3n Normal. X~N(media, desviacion típica)",
         xlab ="x", ylab = "f(x)",
         col = "red")
  })



}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)
