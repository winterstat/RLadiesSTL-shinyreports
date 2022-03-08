## Shiny App: User Interface

# Required packages
#install.packages("palmerpenguins")
library(palmerpenguins)
library(shiny)
library(bslib)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(officedown)
library(flextable)

# Set default ggplot theme
theme_set(theme_minimal())

# Define UI
shinyUI(fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  # Application title with image
  headerPanel(
    title = h1(tags$img(src='palmerpenguins.png', height = 50, width = 50), "Penguin Report"),
    windowTitle="Penguin Report"
  ),
  
  p("Explore the Palmer Penguins with this Shiny App!"),
  p("This App was created to accompany the R-Ladies St. Louis talk ", a(href="https://github.com/winterstat/RLadiesSTL-shinyreports", "'Creating Dynamic Reports from your Shiny App'", target = "_blank", .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")), 
  
  # Sidebar with Input options for user
  sidebarLayout(
    sidebarPanel(
      selectInput("xaxis", label = tags$b("Pick a characteristic:"),
                  choices = list("Bill length" = 'bill_length_mm',
                                 "Bill depth" = 'bill_depth_mm',
                                 "Flipper length" = 'flipper_length_mm',
                                 "Body mass" = 'body_mass_g')),
      selectInput("yaxis", label = tags$b("Pick a second characteristic:"),
                  choices = list("Bill length" = 'bill_length_mm',
                                 "Bill depth" = 'bill_depth_mm',
                                 "Flipper length" = 'flipper_length_mm',
                                 "Body mass" = 'body_mass_g'),
                  selected = "bill_depth_mm"),
      radioButtons("groupvar", label = tags$b("Pick a grouping variable:"),
                   choices = list("Island" = 'island', "Species" = 'species',
                                  "Sex" = 'sex'))
    ),
    
    # Main panel with tabs to show plot, table, and quiz
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot")
        ), 
        
        tabPanel("Table", 
                 DT::dataTableOutput("table")
        ),
        tabPanel("Quiz!", 
                 p("Once you feel like you explored the penguin data fully, 
                           answer the quiz questions below and download a personal report that also includes the last plot and table you created!"),
                 br(),
                 radioButtons("q1", label = tags$b(q1_text),
                              choices = unique(penguins$island)),
                 sliderInput("q2", label = tags$b(q2_text), min = 0, 
                             max = nrow(penguins), value = 0, step = 1),
                 radioButtons("q3", label = tags$b(q3_text),
                              choices = unique(penguins$species)),
                 br(),
                 hr(),
                 # h4("Download your custom report!"),
                 # radioButtons('format', tags$b('Pick a Document format'), 
                 #              c('PDF', 'HTML', 'Word'),
                 #              inline = TRUE),
                 # downloadButton("report", "Generate report"), 
                 # downloadButton("officerreport", "Generate officer report")
        )
      ) # Close tabsetPanel
    ) # Close mainPanel
  ), # Close SidebarLayout 
  hr(), # Include footer
  p("Artwork by ", a(href="https://allisonhorst.github.io/palmerpenguins/articles/art.html", "@allison_horst", 
                     target = "_blank", .noWS = "outside"), '.', 
    .noWS = c("after-begin", "before-end"),
    style = 'color: #D3D3D3; font-size: 0.75em;')
) # Close fluidPage
) # Close ui
