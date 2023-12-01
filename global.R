if (!require('shiny')) install.packages('shiny'); library(shiny)
if (!require('shinythemes')) install.packages('shinythemes'); library(shinythemes)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('readxl')) install.packages('readxl'); library(readxl)
if (!require('sf')) install.packages('sf'); library(sf)
if (!require('ggtext')) install.packages('ggtext'); library(ggtext)
if (!require('ggrepel')) install.packages('ggrepel'); library(ggrepel)
if (!require('timetk')) install.packages('timetk'); library(timetk)

options(scipen = 999) # converte número científico para real


df <- read.csv2('input/latin_america_covid.csv', check.names = FALSE, row.names = 1) 

df$Date <- as.Date(df$Date)

list_country <- sort(unique(df$CountryName))
df_dependent_variable <- select(df,
                                c('7-day moving average of new deaths per 100,000 inhabitants', 'New deaths per 100,000 inhabitants'))

df_independent_variable <- select(df,
                                  -c('7-day moving average of new deaths per 100,000 inhabitants', 'New deaths per 100,000 inhabitants',
                                     '...1','Date','CountryName', 'Population',
                                     'Median age of the population',	'Share of the population that is 65 years and older', 'Share of the population that is 70 years and older',
                                     'GPD per capita',	'Share of the population living in extreme poverty',
                                     'HDI',	'Average number of years of education received by people ages 25 and older',	'Gini index',
                                     'Percentage of people ages 20-79 who have type 1 or type 2 diabetes',	
                                     'Share of informal employment in total employment on agricultural activites',
                                     'Share of informal employment in total employment on non agricultural activites',
                                     'Share of informal employment in total employment on agricultural and non agricultural activites',	
                                     'Population Density',	'Average household size (number of members)',	'Hospitals per 100,000 inhabitants'))


df_desc <- df %>% 
  select(Date,
         CountryName,
         'Total cases per 100,000 inhabitants',
         'Total deaths per 100,000 inhabitants',
         'Case fatality rate',
         'Percentage of the population with at least one vaccine dose',
         'Percentage of the population with at least two vaccine doses')


df_desc_numerical <- df_desc %>% 
  select(-c('CountryName', 'Date'))


df_sociodemographic <- df %>% 
  select(Date,
         CountryName,
         'Median age of the population',	
         'Share of the population that is 65 years and older', 
         'Share of the population that is 70 years and older',
         'GPD per capita',	
         'Share of the population living in extreme poverty',
         'HDI',	
         'Average number of years of education received by people ages 25 and older',	
         'Gini index',
         'Percentage of people ages 20-79 who have type 1 or type 2 diabetes',	
         'Share of informal employment in total employment on agricultural activites',
         'Share of informal employment in total employment on non agricultural activites',
         'Share of informal employment in total employment on agricultural and non agricultural activites',	
         'Population Density',	
         'Average household size (number of members)')


df_sociodemo_numerical <- df_sociodemographic %>% 
  select(-c('CountryName', 'Date'))



shapename <- read_sf('shape_files/World_Countries__Generalized_.shp')


source('ui.R') 
source('server.R')


shinyApp(ui = ui,
         server = server)