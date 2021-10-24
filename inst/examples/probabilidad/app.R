library(shiny)
library(shinydashboard)

#jscode <- "shinyjs.closeWindow = function() { window.close(); }"


ui <- dashboardPage(

  skin = "yellow",

  dashboardHeader(title = "Distribuciones de probabilidad",

                  titleWidth = 300,

  tags$li(a(href = 'https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ',
            target = '_blank',
            img(src = 'e_R_logoB_web.jpg',
                title = 'Canal youtube:\nLa magia de estadistica', height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),

  tags$li(a(href = "window.close()",
            title = "Cerrar",
            icon("power-off")),
          class = "dropdown"),

  dropdownMenuOutput("messageMenu")
),

  # sidebar ----
  dashboardSidebar(
    sidebarMenu(#id = "sidebarid",
                id = "tabName",
                menuItem("Binomial",
                         tabName = "Binomial"),
                startExpanded = FALSE,
                conditionalPanel(
                  'input.tabName == "Binomial"',
                  sliderInput("n", label = "n:",
                              min = 1, max = 10, value = 5, step = 1),

                  sliderInput("p", label = "p:",
                              min = 0.1, max = 0.9, value = 0.5, step = 0.1)),

                menuItem("Poisson", tabName = "Poisson"),
                startExpanded = FALSE,
                conditionalPanel(
                  'input.tabName == "Poisson"',
                  sliderInput("lambda", label = "lambda:",
                              min = 0.1, max = 10, value = 2, step = 0.1)),

                menuItem("Uniforme", tabName = "Uniforme"),
                startExpanded = TRUE,
                conditionalPanel(
                  'input.tabName == "Uniforme"',
                  sliderInput("min", label = "min:",
                              min = 0, max = 10, value = 2, step = 1),

                  sliderInput("max", label = "max:",
                              min = 0, max = 10, value = 6, step = 1)),

                menuItem("Exponencial", tabName = "Exponencial"),
                startExpanded = TRUE,
                conditionalPanel(
                  'input.tabName == "Exponencial"',
                  sliderInput("rate", label = "lambda:",
                              min = 0.1, max = 8, value = 2, step = 0.1)),

                menuItem("Normal", tabName = "Normal"),
                startExpanded = TRUE,
                conditionalPanel(
                  'input.tabName == "Normal"',
                  sliderInput("mean", label = "media:",
                              min = -5, max = 5, value = 0, step = 1),

                  sliderInput("sd", label = "desviacion típica:",
                              min = 0.1, max = 5, value = 1, step = 0.1)),

                menuItem("Acerca de...",
                         tabName = "Acerca de",
                         icon = icon("users"),
                         startExpanded = TRUE,
                         tags$footer(
                           tags$p(strong("Autores:"),br(),
                                  "Vicente Coll-Serrano",br(),
                                  "Cristina Pardo García",br(),
                                  "Rosario Martínez Verdú",br(),
                                  "email: estadisTIC@uv.es"),

                           style = "
      * {
    margin: 0;
  }
  html, body {
    height: 50%;
  }
  .wrapper {
    min-height: 100%;
    height: auto !important; /* This line and the next line are not necessary unless you need IE6 support */
    height: 100%;
    margin: 0 auto -155px; /* the bottom margin is the negative value of the footer's height */
  }
  .footer, .push {
    height: -155px; /* .push must be the same height as .footer */
  }

  /*

  Sticky Footer by Ryan Fait
  http://ryanfait.com/

  */"
                         )
                )

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
            xlim = c(0, 15), ylim=c(0, 1),
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
