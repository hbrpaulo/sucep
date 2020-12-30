

#
# https://blog.indicium.tech/como-criar-mapas-interativos-com-r-e-leaflet-com-dados-do-covid-19/
# https://rpubs.com/wesleysilva88/mapa_leaflet
# https://learn.r-journalism.com/en/mapping/leaflet_maps/leaflet/     'Estilizar'
# https://www.tiagoms.com/post/mapa/

rm(list = ls())

#setwd("~/")

library(RColorBrewer)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(plotly)
library(readxl)
library(DT)

link  = "https://raw.githubusercontent.com/hbrpaulo/sucep/master/dadospnad.csv"
xyo <- read_csv(link) %>%
  data.frame %>% melt(id.vars = c("Indices", "Região"),
                      value.name = "Valor",
                      variable.name = "Data") %>%
  arrange(Região, Data)
# 
# library(readxl)
# xyo <- read_excel("Documents/pnad/2020/2019 a 2020 separado por região.xlsx", 
#                                                sheet = "Dados") %>%
#     data.frame %>% melt(id.vars = c("Indices", "Região"),
#                         value.name = "Valor", 
#                         variable.name = "Data") %>% 
#     arrange(Região, Data)


xyo = xyo %>%
  mutate(
    Data = ifelse(Data=="jul.ago.set.2018", "2018 08",
                  ifelse(Data=="out.nov.dez.2018", "2018 11",
                         ifelse(Data=="jan.fev.mar.2019", "2019 02",
                                ifelse(Data=="abr.mai.jun.2019", "2019 05",
                                       ifelse(Data=="jul.ago.set.2019", "2019 08",
                                              ifelse(Data=="out.nov.dez.2019", "2019 11",
                                                     ifelse(Data=="jan.fev.mar.2020", "2020 02",
                                                            ifelse(Data=="abr.mai.jun.2020", "2020 05", Data)
                                                     )
                                              )
                                       )
                                )
                         )
                  )
    )
  ) %>% mutate(Data = zoo::as.yearmon(Data, format = "%Y %m"))

xyo$Valor = as.numeric(xyo$Valor)

ldf = ldfscale = xyo %>% group_split(Região, Data)


for(i in 1:length(ldf)){
  ldfscale[[i]][4] = scale(ldf[[i]][4])
}

scales<-function(df){
  df[,4] = df[,4] %>% scale
  return(df)
}  

ldfscale = lapply(ldf, scales)

df = plyr::ldply(ldf, data.frame)
dfscale = plyr::ldply(ldfscale, data.frame)

# ggplotly(
# ggplot(dfscale,
#        mapping = aes(y = Valor, x = Data, group = Indices,
#                      colour = Indices)) + facet_wrap(~Região) +
#   geom_smooth(method = "auto", se = FALSE) + 
#     theme(legend.position = "none", 
#   axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
# )


#df %>% group_by(Região, Data) %>% summarise(Valor = Valor) %>% 
#  ggplot(aes(x = Data, y = Valor, col = Região)) + geom_smooth(se = FALSE) + theme_minimal()

# dfscale %>% group_by(Região, Data) %>% summarise(Valor = mean(Valor*10^18)) %>% 
#   ggplot(aes(x = Data, y = Valor, col = Região)) + geom_smooth(se = FALSE) + 
#   theme_minimal()



# Mapa 02 ####

library(ggplot2)
library(dplyr)
library(viridis)
# library(brazilmaps) 
library(geobr)
library(sf)
library(maptools)
library(leaflet)

theme_set(theme_bw())

# mapa <- brazilmaps::get_brmap("State")

mapa <- read_state(showProgress = FALSE)

#(ggplot(mapa)+ 
#  geom_sf())  %>% ggplotly()


normal = dfscale %>% group_by(Região, Data) %>% summarise(Valor = mean(Valor*10^18)) %>%
  arrange(desc(Data)) %>% filter(`Região` == "Brasil") %>% select(Valor) %>% .[1,2] %>% pull

valor1 <- data.frame(code_state = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15, 
                                    25, 41, 26, 22, 33, 24, 43, 11, 14, 42, 35, 28, 17), 
                     com_rede = rep(normal, 27)
)

aux = data.frame(UF = c("Brasil", "Goiás", "Mato Grosso do Sul", "Mato Grosso"),
                 code_state = c(1, 52, 50, 51))

valor = dfscale %>% group_by(Região, Data) %>% summarise(Valor = mean(Valor*10^18)*2)  %>% inner_join(aux, by = c("Região" = "UF"))

# (mapa %>% 
#   inner_join(valor, by = "code_state") %>% 
#   ggplot() +
#   geom_sf(aes(fill = Valor), color = "white") + 
#     scale_fill_gradient(low = "gray89", high = "blue", na.value = NA) +
#     coord_sf() +
#     facet_wrap(~factor(Data), ncol = 3) +
#     theme_minimal()) %>% ggplotly()

# (mapa %>% 
#     inner_join(valor, by = "code_state") %>% 
#     filter(Data==unique(Data)[4]) %>% 
#     ggplot() +
#     geom_sf(aes(fill = Valor), color = "white") + 
#     #  scale_fill_viridis(name = "Índice de recuperação", direction = 1, 
#     #                    option = "A", alpha = .752) +
#     scale_fill_gradient(low = "gray89", high = "blue", na.value = NA) +
#     coord_sf() +
#     theme_minimal()) %>% ggplotly()


library(shiny)
val = c("ago 2018", "nov 2018", 
        "fev 2019", "mai 2019",
        "ago 2019", "nov 2019",
        "fev 2020", "mai 2020")

nomes = paste0(1:4, "º ", "Trimestre de ", rep(2018:2020, each = 4))[3:10]

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tabsetPanel(
    # Application title
    tabPanel("Mapa",
             titlePanel(strong("Índice de Recuperação Econômica do 3º trimestre de 2018 ao 3º trimestre de 2020")),
             hr(),
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "data",
                              #choiceNames = list("Data"),
                              icon("calendar"),
                              choiceNames = nomes,#val %>% str_to_title,
                              choiceValues = val),
                 width = 3
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("myplotly"),
                 hr(),
                 plotlyOutput("linhas")
               )
             )#, setBackgroundImage(src = "https://www.hdwalls.com.tr/upload/orta_HD-605C-ABSTRACT-ART.jpg")
    ),
    tabPanel("Planilha indicadores",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "data1",
                              #choiceNames = list("Data"),
                              icon("calendar"),
                              choiceNames = nomes,#val %>% str_to_title,
                              choiceValues = val),
                 width = 3
               ),
             mainPanel(dataTableOutput("table"))
    )),
    tabPanel("Planilha indicadores ajustados",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "data2",
                              #choiceNames = list("Data"),
                              icon("calendar"),
                              choiceNames = nomes,#val %>% str_to_title,
                              choiceValues = val),
                 width = 3
               ),
               mainPanel(dataTableOutput("scaletable"))
             )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myplotly = renderPlotly({
    ggplotly(mapa %>% 
               left_join(valor, by = "code_state") %>% 
               filter(Data==input$data) %>% 
               ggplot() +
               geom_sf(aes(fill = Valor), color = "white") + 
               #  scale_fill_viridis(name = "Índice de recuperação", direction = 1, 
               #                    option = "A", alpha = .752) +
               scale_fill_gradient("Índice de recuperação", low = "gray89", high = "blue", na.value = NA) +
               coord_sf() + labs(title = input$data) +
               theme_minimal())})
  output$linhas = renderPlotly({
    ggplotly(dfscale %>% group_by(Região, Data) %>% summarise(Valor = mean(Valor*10^18)) %>% 
               ggplot(aes(x = Data, y = Valor, col = Região)) + geom_smooth(se = FALSE) + 
               theme_minimal()) %>% layout(hovermode = 'compare')
  }
  )    
  output$table = renderDataTable({
    df %>% filter(Data==input$data1) %>% select(Indices, Região, Valor) %>% 
      dcast(Indices~Região, fun.aggregate = mean) %>% mutate_if(is.numeric, round, 2) %>% 
      datatable(style = "bootstrap4")
  }
  )
  output$scaletable = renderDataTable({
    dfscale %>% filter(Data==input$data2) %>% select(Indices, Região, Valor) %>% 
      dcast(Indices~Região, fun.aggregate = mean) %>% mutate_if(is.numeric, round, 4) %>% 
      datatable(style = "bootstrap4")
  }
  )
  
}

# Run the application 
