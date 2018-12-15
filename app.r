#load packages
library(shiny)
library(sunburstR)
library(RColorBrewer)
library(shinythemes)

#load data
Both.mother <- readRDS("Both.mother.rds")
Animals.mother <- readRDS("Animals.mother.rds")
Humans.mother <- readRDS("Humans.mother.rds")
Both.offspring <- readRDS("Both.offspring.rds")
Animals.offspring <- readRDS("Animals.offspring.rds")
Humans.offspring <- readRDS("Humans.offspring.rds")

#server
server <- function(input, output, session) {
  datasetInput <- reactive({
    switch(paste(input$species,input$pregnancy.birth),
           "Humans Mothers" = Humans.mother,
           "Animals Mothers" = Animals.mother,
           "Both Mothers" = Both.mother,
           "Humans Offspring" = Humans.offspring,
           "Animals Offspring" = Animals.offspring,
           "Both Offspring" = Both.offspring
    )
  })
  output$sunburst <- renderSunburst({
    add_shiny(datasetInput())
  })
  selection <- reactive({
    input$sunburst_mouseover
  })
  output$downloadData <- downloadHandler(
    filename ="JDOHaD_exposure_level_data_Sharp2019.csv", 
    content = function(file){file.copy("dat.csv",file)}
  )
#  output$githubLink <- a(href="https://github.com/ammegandchips/jdohad_sunburst")
}

#ui
ui <- fluidPage(
  theme=shinytheme("sandstone"),
  headerPanel('Exposures studied in the Journal of Developmental Origins of Health and Disease'),
  sidebarPanel(
    includeHTML("sunburst_text.html"),
    radioButtons("species", 'Select human studies, animal studies, or both:', c("Humans","Animals","Both"),selected="Both"),
    radioButtons("pregnancy.birth",'Select whether to classify exposures like birthweight and gestational age as pertaining to mothers or offspring:',c("Mothers","Offspring"),selected="Mothers"),
    downloadButton('downloadData',"Download the raw data"),
    br(),br(),
    actionButton('githubLink',"See the R code",icon=icon("github"),onclick="window.open('https://github.com/ammegandchips/jdohad_sunburst')"),
    includeHTML("author_details.html"),
    width =5),
  mainPanel(
    sunburstOutput("sunburst", height="800px"),
    width=7
  )
)

#combine server and ui
shinyApp(ui = ui, server = server)
