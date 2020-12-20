# Cargar librerías
library(tidyverse)
library(hrbrthemes)
library(cluster)
library(factoextra)
library(gridExtra)
library(COVID19)

# Cargar datos
covid_data <- covid19()

# Datos actualizados
covid_today <- covid_data %>%
  filter(grepl(Sys.Date(), date))

# Eliminar data completa
rm(covid_data)

#############################
# 10 países con más muertes #
#############################

# Ordenar países
deaths_mayor_menor <- order(covid_today$deaths, decreasing = TRUE)
covid_deaths_mayor_menor <- covid_today[deaths_mayor_menor, ]
covid_deaths_mayor_menor <- covid_deaths_mayor_menor[c(1:10),]

# Crear gráfico de barras
top10 <- covid_deaths_mayor_menor %>%
  ggplot(aes(x = deaths, y = factor(covid_deaths_mayor_menor$administrative_area_level_1, levels = covid_deaths_mayor_menor$administrative_area_level_1[order(covid_deaths_mayor_menor$deaths)]), fill = administrative_area_level_1)) +
  geom_bar(stat = 'identity', show.legend = FALSE, fill = '#DC143C') +
  labs(
    y = '',
    x = 'Fallecidos',
    title = '10 países con más\nfallecidos por COVID19',
    subtitle = Sys.Date()
  ) +
  theme_ft_rc()

##########################################
# Proporción de fallecidos y recuperados #
##########################################

# Factor
situation <- c(rep('Fallecidos',10),rep('Recuperados',10))

# Duplicar los nombres de los países
country <- c(rep(covid_deaths_mayor_menor$administrative_area_level_1, 2))

# Situación de fallecidos y recuperados
data_situation <- c(covid_deaths_mayor_menor$deaths, covid_deaths_mayor_menor$recovered)

# Crear dataframe
proportion <- data.frame(country,situation,data_situation)


# Crear gráfico de proporciones
top10_prop <- proportion %>%
  ggplot(aes(x = country, y = data_situation, fill = situation)) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() +
  labs(
    y = 'Porcentaje',
    x = '',
    caption = 'Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub",\nWorking paper, doi: 10.13140/RG.2.2.11649.81763.'
  ) +
  scale_fill_manual(name = 'Situación', values = c('#DC143C','#ADFF2F')) +
  theme_ft_rc()

# Combinar gráficos
grid.arrange(top10, top10_prop, nrow = 1)

# Eliminar variables ya usadas
rm(
  country,
  data_situation,
  situation,
  deaths_mayor_menor,
  proportion,
  covid_deaths_mayor_menor,
  covid_today
  )
