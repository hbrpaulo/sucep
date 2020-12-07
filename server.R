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
    })
}
