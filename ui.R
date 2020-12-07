library(shiny)
val = c("ago 2018", "nov 2018", 
           "fev 2019", "mai 2019",
           "ago 2019", "nov 2019",
           "fev 2020", "mai 2020")
 
nomes = paste0(1:4, "º ", "Trimestre de ", rep(2018:2020, each = 4))[3:10]

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Índice de Recuperação Econômica do 3º trimestre de 2018 ao 3º trimestre de 2020"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "data",
                         #choiceNames = list("Data"),
                         icon("calendar"),
                         choiceNames = nomes,#val %>% str_to_title,
                         choiceValues = val)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("myplotly"),
            plotlyOutput("linhas")
        )
    )
)
