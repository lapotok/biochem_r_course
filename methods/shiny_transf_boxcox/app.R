library(shiny)
library(tidyverse)
library(ggpubr)
library(broom)
library(patchwork)
library(glue)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Трансформация данных методом Box-Cox"),
  
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      helpText(withMathJax("Формула трансформации: $$y_i^{(\\lambda)} = \\begin{cases}
\\dfrac{y_i^\\lambda - 1}{\\lambda} & \\text{if } \\lambda \\neq 0, \\\\
\\ln y_i & \\text{if } \\lambda = 0
\\end{cases}$$")),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Lambda",
                  label = withMathJax("Значение \\(\\lambda\\)"),
                  min = -2,
                  max = 2,
                  value = 1,
                  step=.1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "Scatter", width = "600px", height = "800px")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$Scatter <- renderPlot({
    set.seed(1234)
    bc_transf = function(x, lambda){
      if (lambda == 1) return(x)
      if (lambda == 0) return(log(x))
      return((x^lambda-1)/lambda)
    }
    
    df = 
      data.frame(x = 1:40) %>%
      mutate(y = 1.13^(x*.6 + 6 + rnorm(40, 0, 1))+ rnorm(40, 0, .5)) %>%
      mutate(yt = bc_transf(y, input$Lambda))
    
    m = lm(yt ~ x, df)
    ma = m %>% augment()
    
    g1 = ggscatter(ma, "x", "yt", size=5, alpha=.6, title = ifelse(input$Lambda == 1, "Исходные данные", bquote("Трансформированные данные ("~.(input$Lambda)~", R2={m %>% summary() %>% .$r.squared %>% round(2)})")), xlab = "Переменная X", ylab = "Переменная Y", ggtheme = theme_classic(base_size = 20)) +
      geom_line(aes(x=x, y=.fitted), col="dodgerblue", size=1)
    g2 = ggscatter(ma, "x", ".resid", size=3, alpha=.6, title = "График остатков", xlab = "Переменная X", ylab = "Остатки", ggtheme = theme_classic(base_size = 20)) + 
      geom_hline(yintercept = 0) +
      geom_segment(data=ma, aes(x=x, y=0, xend=x, yend=.resid), col="red", alpha=.6)
    g3 = g1 / plot_spacer() / g2 + plot_layout(heights = c(.7, .1, .2))
    g3
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

# runApp("/Users/lapotok/Dropbox/study/biochem_statistics/tests/shiny_transf_boxcox")