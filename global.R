#############################################################################################################################################

# Extraccion, Transformacion y limpieza de datos


# Datos de: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv

# DataSet: chain_investment



#  variable	           class	          description


#  category	           character	      Category of investment
#  meta_cat	           character	      Group category of investment
#  group_num	         double	          Group number of investment
#  year	               integer	        Year of investment
#  gross_inv_chain	   double	          Gross investment (chained 2021 dollars) in millions of USD


#############################################################################################################################################
#
#                   Librerias a utilizar:
#
#############################################################################################################################################

library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(ggcorrplot)
library(lares)



#############################################################################################################################################
#
#                   Datos desde github:
#
#############################################################################################################################################



chain_investment <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv")


#############################################################################################################################################
#
#                   Limpieza y transformación de algunos datos:
#
#############################################################################################################################################


chain_investment$meta_cat[chain_investment$meta_cat == "Air /water /other transportation"] <- "Air/water/transp"

chain_investment$meta_cat[chain_investment$meta_cat == "Highways and streets"] <- "Highways and sts"

chain_investment$meta_cat[chain_investment$meta_cat == "Total basic infrastructure"] <- "Total basic infrast"

chain_investment$meta_cat[chain_investment$meta_cat == "Conservation and development"] <- "Conserv/development"

chain_investment$meta_cat[chain_investment$meta_cat == "Natural gas /petroleum power"] <- "Nat Gas/Petro"




# Data Set Agrupado


chain_investment_group <- chain_investment %>%
  group_by(year, meta_cat) %>%
  summarise(total = sum(gross_inv_chain))



chain_investment_1947 <- chain_investment_group %>% filter(year == min(chain_investment_group$year))

chain_investment_2017 <- chain_investment_group %>% filter(year == max(chain_investment_group$year))

percent_final <- cbind(chain_investment_1947, chain_investment_2017)

colnames(percent_final) <- c("año_1947", "cat_1947", "inv_1947", "año_2017", "cat_2017", "inv_2017")

percent_final$variacion <- round(((percent_final$inv_2017 - percent_final$inv_1947) / percent_final$inv_1947) * 100, 2)

remove(chain_investment_1947, chain_investment_2017)




# Inversion Maxima:

chain_investment$meta_cat[which(chain_investment$gross_inv_chain == max(chain_investment$gross_inv_chain))]

# Monto Maximo Invertido:

max(chain_investment$gross_inv_chain)

# Año de la inversion:

chain_investment$year[which(chain_investment$gross_inv_chain == max(chain_investment$gross_inv_chain))]


# Variacion maxima

percent_final$cat_2017[which(percent_final$variacion == max(percent_final$variacion))]

# Monto Variacion Maxima:

max(percent_final$variacion)


# Variacion minima

percent_final$cat_2017[which(percent_final$variacion == min(percent_final$variacion))]

# Monto Variacion minima:

min(percent_final$variacion)







#############################################################################################################################################
#
#                   Grafico de Proporciones de Inversion Anual:
#
#############################################################################################################################################


# Transformacion de los datos

library(plyr)


chain_proporciones <- adply(chain_investment_group, 1, function(x) {
  c("proporcion" = x$total / sum(chain_investment_group[x$year == chain_investment_group$year, ]$total))
})


chain_proporciones$proporcion <- round(chain_proporciones$proporcion, 4) * 100





#############################################################################################################################################
#
#                   Grafico de serie temporal de Proporciones de Inversion: (Se utilizara para hacerlo dinamico en app.R)
#
#############################################################################################################################################



# Transformacion de los datos


chain_proporciones_totales <- adply(chain_investment, 1, function(x) {
  c("proporcion_total" = x$gross_inv_chain / sum(chain_investment[x$year == chain_investment$year, ]$gross_inv_chain))
})



chain_proporciones_totales$proporcion_total <- round(chain_proporciones_totales$proporcion_total, 4) * 100


# Detach de plyr para no tener problemas con group_by

detach("package:plyr", unload = TRUE)


chain_proporciones_totales <- chain_proporciones_totales %>%
  select(year, meta_cat, proporcion_total) %>%
  group_by(meta_cat, year) %>%
  summarise(proporcion_anual = sum(proporcion_total))



#############################################################################################################################################
#
#                   Valores unicos para SelectInput:
#
#############################################################################################################################################



variables_totales <- sort(unique(chain_investment$meta_cat))




#############################################################################################################################################
#
#                   Grafico de barras con variaciones por rubro
#
#############################################################################################################################################


# Orden del set de datos por variable variacion decreciente

percent_final$cat_2017 <- factor(percent_final$cat_2017, levels = unique(percent_final$cat_2017)[order(percent_final$variacion, decreasing = FALSE)])



#############################################################################################################################################
#
#                   Transformando el set de datos en series temporales para un grafico de mayores correlaciones:
#
#############################################################################################################################################



# Transformando el set de datos:


chain_time_series <- spread(chain_investment_group, meta_cat, total, fill = 0)



#############################################################################################################################################
#
#                   Transformacion final data sets:
#
#############################################################################################################################################



porcentaje_final <- percent_final[, c("cat_2017", "variacion")]


#############################################################################################################################################








