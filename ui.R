ui <- fluidPage(
  navbarPage('COVID-19 in Latin America', 
             id = 'app', 
             theme = shinytheme('flatly'),
             
             tabPanel('Exploratory Analysis',
                      sidebarLayout(
                        # criando barra lateral para inputar dados
                        sidebarPanel(
                          # criando caixa de intervalo temporal
                          dateRangeInput(inputId = 'date',
                                         label = 'Date range:',
                                         start = min(df$Date),
                                         end = max(df$Date),
                                         format = "yyyy-mm-dd"),
                          
                          # criando caixa de selecao de paises
                          selectInput(inputId = 'country',
                                      label = 'Country:',
                                      choices = list_country,
                                      selected = 'Argentina'), # precisa ser um vetor com valores unicos
                          
                          # criando caixa de selecao de variavel dependente
                          varSelectInput(inputId = 'dependent_variable',
                                         label = 'Dependent variable:',
                                         data = df_dependent_variable,
                                         selected = 'new_deaths_per_100khabitants_mm7'),
                          
                          # criando caixa de selecao de variavel independente
                          varSelectInput(inputId = 'independent_variable',
                                         label = 'Independent variable:',
                                         data = df_independent_variable),
                          
                          # criando caixa de selecao de tipo de regressao
                          selectInput(inputId = 'time_lag',
                                      label = 'Day-lag between independent variable and dependent varible:',
                                      choices = c(0:35), # precisa ser um vetor com valores unicos
                                      selected = 0),
                          
                          # criando caixa de selecao de tipo de regressao
                          selectInput(inputId = 'regression_type',
                                      label = 'Type of regression:',
                                      choices = c('None','Linear regression', 'Generalized linear model', 'LOESS (Locally Weighted Least Squares Regression)'), # precisa ser um vetor com valores unicos
                                      selected = 'Linear regression')
                        ),
                        
                        # painel principal para apresentar outputs
                        mainPanel(plotOutput('scatterplot'),
                                  
                                  textOutput('pearsoncorrelation')
                        )
                        
                      )
             ),
             
             tabPanel('Descriptive Analysis',
                      sidebarLayout(
                        # criando barra lateral para inputar dados
                        sidebarPanel(
                          # criando caixa de intervalo temporal
                          sliderInput(inputId = 'date_desc',
                                      label = 'Date range (yyyy-mm-dd):',
                                      min = min(df$Date),
                                      max = max(df$Date),
                                      value = max(df$Date)),
                          
                          # criando caixa de selecao de variavel
                          varSelectInput(inputId = 'variable_desc',
                                         label = 'Variable:',
                                         data = df_desc_numerical),
                          
                        ),
                        
                        # painel principal para apresentar outputs
                        mainPanel(plotOutput('desc_plot')
                        )
                        
                      )
             ),
             
             tabPanel('Geo Heat Maps',
                      splitLayout(cellWidths = c('50%', '50%'),
                                  # criando filtro de data
                                  sliderInput(inputId = 'date_map',
                                              label = 'Date range (yyyy-mm-dd):',
                                              min = min(df$Date),
                                              max = max(df$Date),
                                              value = max(df$Date),
                                              animate = animationOptions(interval = 700, loop = FALSE))
                      ),
                      
                      # painel principal para apresentar selecao de variavel
                      splitLayout(cellWidths = c('50%', '50%'),
                                  # criando caixa de selecao de variavel
                                  varSelectInput(inputId = 'map_variable_covid',
                                                 label = 'COVID variable:',
                                                 data = df_desc_numerical,
                                                 selectize = FALSE,
                                                 size = 3,
                                                 width = '70%'),
                                  
                                  # criando caixa de selecao de variavel
                                  varSelectInput(inputId = 'map_variable_sociodemo',
                                                 label = 'Sociodemographic variable:',
                                                 data = df_sociodemo_numerical,
                                                 selectize = FALSE,
                                                 size = 3,
                                                 width = '70%')
                                  
                      ),
                      
                      # painel principal para apresentar outputs
                      splitLayout(cellWidths = c('50%', '50%'),
                                  plotOutput(outputId = 'covid_map'),
                                  
                                  plotOutput(outputId = 'sociodemo_map')
                                  
                      )
             )
  )
)