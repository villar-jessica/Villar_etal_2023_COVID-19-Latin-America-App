server <- function(input,output,session){
  df_exploratory <- reactive({
    
    df %>% 
      filter(CountryName == input$country,
             Date <= max(input$date),
             Date >= min(input$date)) %>% 
      tk_augment_lags(!!input$dependent_variable, 
                      .lags = (as.numeric(input$time_lag)+1), 
                      .names = 'Dependent variable')
      
    
    # name_column <- paste(as.character(!!input$dependent_variable), '_lag', input$time_lag, sep = '')
    
    # df %>%
    #   tk_augment_lags(!!input$dependent_variable, .lags = input$time_lag) %>% 
    #   select(-c(!!input$dependent_variable)) #%>%
      # rename(name_column = !!input$dependent_variable[[length(input$dependent_variable)]])
    
  })
  
  create_scatterplot <- reactive({
    
    if (input$time_lag == 0) {
      
      y_label_name <- input$dependent_variable
      
    } else if  (input$time_lag == 1) {
      
      y_label_name <- paste(input$dependent_variable, 'lagged by', input$time_lag, 'day', sep = ' ')
      
    } else {
      
      y_label_name <- paste(input$dependent_variable, 'lagged by', input$time_lag, 'days', sep = ' ')
      
    }
    
    
    ggplot(data = df_exploratory(), aes(x = !!input$independent_variable, y = `Dependent variable`)) +
      geom_point() +
      ylab(y_label_name) +
      ggtitle(paste(input$independent_variable, 'X', y_label_name, sep = '\n')) +
      theme_bw() +
      theme(plot.title = element_textbox_simple(),
            axis.text = element_text(size = 11),
            panel.border = element_blank(),
            legend.position = c(.9, .9)) + 
      labs(color = guide_legend(title = 'Regression')) +
      
      if (input$regression_type == 'None'){
        
        NULL
        
      } else if (input$regression_type == 'Linear regression'){
        
        geom_smooth(method = 'lm', aes(colour = 'Linear regression'))
        
      } else if (input$regression_type == 'Generalized linear model') {
        
        geom_smooth(method = 'glm', aes(colour = 'Generalized linear model'))
        
      } else {
        
        geom_smooth(method = 'loess', aes(colour = 'LOESS (Locally Weighted Least Squares Regression)'))
        
      }
    
  })
  
  output$scatterplot <- renderPlot({create_scatterplot()})
  
  
  
  pearson_corr <- reactive({
    
    X <- select(df_exploratory(), !!input$independent_variable)[[1]]
    Y <- select(df_exploratory(), !!input$dependent_variable)[[1]]
    
    correlation <- cor.test(x = X, y = Y, method = "pearson", conf.level = 0.95)
    cor_value <- round(correlation$estimate, 1)
    p_value <- if (round(correlation$p.value, 4) == 0) {
      
      0.001
      
    } else {
      
      round(correlation$p.value, 4)
      
    }
    paste('Pearson correlation:', cor_value,', with p-value <', p_value)
    
  })
  
  output$pearsoncorrelation <- renderText(pearson_corr())
  
  
  
  # criando dataframe com dados de casos e mortes por pais
  df_descriptive <- reactive({
    
    df_descriptive <- df_desc %>% 
      filter(Date <= input$date_desc) %>% 
      group_by(CountryName) %>%
      summarise(`Total cases per 100,000 inhabitants` = max(`Total cases per 100,000 inhabitants`, na.rm = TRUE),
                `Total deaths per 100,000 inhabitants` = max(`Total deaths per 100,000 inhabitants`, na.rm = TRUE),
                `Case fatality rate` = max(`Total deaths per 100,000 inhabitants`, na.rm = TRUE)*100 / max(`Total cases per 100,000 inhabitants`, na.rm = TRUE),
                `Percentage of the population with at least one vaccine dose` = max(`Percentage of the population with at least one vaccine dose`, na.rm = TRUE)*100,
                `Percentage of the population with at least two vaccine doses` = max(`Percentage of the population with at least two vaccine doses`, na.rm = TRUE)*100)
    
  })
  
  
  
  # criando grafico de obitos
  create_desc_plot <- reactive({
    
       ggplot(data = df_descriptive(), aes(x = reorder(CountryName, !!input$variable_desc), y = !!input$variable_desc)) +
      geom_bar(stat = "identity", fill = "lightskyblue2") + 
      coord_flip() +
      ggtitle(input$variable_desc) +
      xlab('') +
      ylab('') +
      geom_text(aes(label = round(!!input$variable_desc,2)),
                    position = position_dodge(width = 0.9), hjust = 1) +
      theme_bw() +
      theme(axis.text = element_text(size = 11),
            plot.title = element_text(size = 14),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
  })
  
  output$desc_plot <- renderPlot({create_desc_plot()})
  
  
  
  df_descriptive_w_map <- reactive({
    
    df_descriptive <- df_desc %>% 
      filter(Date <= input$date_map) %>% 
      group_by(CountryName) %>%
      summarise(`Total cases per 100,000 inhabitants` = max(`Total cases per 100,000 inhabitants`, na.rm = TRUE),
                `Total deaths per 100,000 inhabitants` = max(`Total deaths per 100,000 inhabitants`, na.rm = TRUE),
                `Case fatality rate` = max(`Total deaths per 100,000 inhabitants`, na.rm = TRUE)*100 / max(`Total cases per 100,000 inhabitants`, na.rm = TRUE),
                `Percentage of the population with at least one vaccine dose` = max(`Percentage of the population with at least one vaccine dose`, na.rm = TRUE)*100,
                `Percentage of the population with at least two vaccine doses` = max(`Percentage of the population with at least two vaccine doses`, na.rm = TRUE)*100)
    
    left_join(shapename, df_descriptive, by = c('COUNTRY' = 'CountryName')) %>% 
      filter(COUNTRY %in% list_country)
    
  })
  
  
  
  df_sociodemo_w_map <- reactive({
    
    df_sociodemographic <- df_sociodemographic %>% 
      filter(Date == input$date_map) %>% 
      group_by(CountryName)
    
    left_join(shapename, df_sociodemographic, by = c('COUNTRY' = 'CountryName')) %>% 
      filter(COUNTRY %in% list_country)
    
  })
  
  
  
  # criando mapa de covid
  create_covid_map <- reactive({
    ggplot() +
      geom_sf(data = df_descriptive_w_map(), 
              aes(fill = !!input$map_variable_covid), 
              color = NA) +
      labs(title = input$map_variable_covid, 
           size = 15) +
      scale_fill_distiller(palette = 'BuPu',
                           direction = 1) +
      theme_minimal() +
      theme(legend.title = element_blank(), 
            legend.position = c(0.2, 0.2),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  
  output$covid_map <- renderPlot({create_covid_map()})
  
  
  
  # criando mapa sociodemografico
  create_sociodemo_map <- reactive({
    ggplot() +
      geom_sf(data = df_sociodemo_w_map(), 
              aes(fill = !!input$map_variable_sociodemo), 
              color = NA) +
      labs(title = input$map_variable_sociodemo, 
           size = 15) +
      scale_fill_distiller(palette = 'Oranges',
                           direction = 1) +
      theme_minimal() +
      theme(legend.title = element_blank(), 
            legend.position = c(0.2, 0.2),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  
  output$sociodemo_map <- renderPlot({create_sociodemo_map()})
  
}