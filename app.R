

# Source ------------------------------------------------------------------



source('global.R', encoding = 'UTF-8')




# UI ----------------------------------------------------------------------




ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title = "Inversion EE.UU",
                  titleWidth = 200,
                  disable = F),
  dashboardSidebar(width = 200, 
                   disable = F,
                   collapsed = F,
                   hr(),
                   sidebarMenu(menuItem("Dinamica", 
                                        tabName="analisis", 
                                        startExpanded = T,
                                        icon=icon("align-justify"),
                                        sliderInput(inputId = "bins",
                                                    label = "Años:",
                                                    min = 1947,
                                                    max = 2017,
                                                    value = 1950,
                                                    sep = ""),
                                        selectInput(inputId = 'grafico',
                                                    label = 'Graficos:',
                                                    choices = list('Series de Tiempo'= 1,
                                                                   'Correlaciones'= 2),
                                                    selected = 1
                                        )),
                               

# Panel Principal ---------------------------------------------------------

                               
                               menuItem("Panel Principal",
                                        tabName = "variation",
                                        startExpanded = F,
                                        icon=icon("align-justify")
                               ),

# Rubros ---------------------------------------------------------

                               menuItem("Rubros",
                                        tabName = "acercade",
                                        startExpanded = F,
                                        icon=icon("signal")
                                        
# Variaciones ---------------------------------------------------------
                                        
                               ),
                               menuItem("Variaciones",
                                        tabName = "vars",
                                        startExpanded = F,
                                        icon=icon("signal")
                                        
# Datos ---------------------------------------------------------
                                        
                               ),
                               menuItem("Datos",
                                        tabName = "datosbase",
                                        startExpanded = F,
                                        icon=icon("database")
                                        
# Resumen ---------------------------------------------------------
                                      
                               ),
                               menuItem("Resumen", 
                                        tabName = "tabla", 
                                        startExpanded = F,
                                        icon=icon("table")))),
  dashboardBody(
    tabItems(
      
# Panel Principal ---------------------------------------------------------
      
      tabItem(
        tabName="variation",
        fluidRow(
          valueBoxOutput("maxima_inversion", width = 4),
          valueBoxOutput("maxima_variacion", width = 4),
          valueBoxOutput("minima_variacion", width = 4),
          

            box(title = "Series Temporales y Correlaciones de Inversion por Rubro:",
                status = "primary",
                solidHeader = T, 
                height = "537" ,
                plotlyOutput(outputId = "scatPlot", height = "466")),
            
            box(title = "Grafico de Barras: Inversion por Rubro sobre Inversion Total",
                status = "primary",
                solidHeader = T,
                height = "537" ,
                plotlyOutput(outputId = "distPlot", height = "466"))),
        column(12,
               h2('Observaciones'),
               fluidRow(
                 box(
                   title = "Serie Temporal, Grafico de mayores correlaciones y Grafico de Barras:",
                   width = 100,
                   height = 170,
                   status = "primary",
                   h5('Podemos visualizar la evolucion a traves del tiempo del monto invertido por', strong('Rubro:'), 'las principales correlaciones y la proporcion de inversion anual por', strong('Rubro:')),
                   h5('En terminos absolutos, la inversion en ', strong('Infraestructura:'), 'es la mayor de todas a lo largo de los años analizados.'),
                   h5('Podemos observar en las series temporales un grafico muy util para comparar trayectorias de inversion por rubro e incluso poder compararlos.'),
                   solidHeader = TRUE)))),

# Rubros ---------------------------------------------------------
      
      tabItem(
        tabName = "acercade",
        fluidPage(
          
          h1('Proporcion de Inversion por Rubro Anual'),
          sidebarLayout(
            sidebarPanel(
              br(br(selectInput(inputId = 'day',
                                label = p('Seleccione el Rubro: '),
                                choices = variables_totales,
                                selected = 'Digital'), style = "background: #ffffff"))),
            
            box(title = "Grafico de Barras:",
                status = "primary",
                solidHeader = T,
                height = "537" ,
                plotlyOutput(outputId = 'barPlotProp', height = "480")))),
        
        column(12,
               h2("Observaciones:"),
               fluidRow(
                 box(
                   title = "Cambios en la Inversion desde 1947:",
                   width = 100,
                   height = 195,
                   status = "primary",
                   h5('Podemos visualizar los distintos componentes que integran la inversion en EE.UU desde 1947 hasta 2017:'),
                   br(),
                   h5('El Rubro Digital es el que mas crecio en terminos relativos, se puede ver un crecimiento exponencial en los ultimos años.'),
                   h5('Servicios de educacion, sociales y salud tambien tuvieron incrementos en terminos de los otros rubros.'),
                   solidHeader = TRUE)))),

# Variaciones ---------------------------------------------------------

      tabItem(
        tabName = 'vars',
        fluidPage(
          fluidRow(
            box(
            title = strong("Variacion Porcentual en Inversion por Rubro"), 
            solidHeader = TRUE,
            width = 100,
            p("Podemos visualizar que el Rubro que mas crecio en terminos relativos desde 1947 es el", strong("Digital")),
            p("Los Rubros que siguen a Digital son: ", strong("Health, Social y Educacion"), 'con un significativo incremento en la inversion en terminos relativos.'))),
          
          box(title = "Variaciones de Inversion desde 1947",
              status = "primary",
              solidHeader = T, 
              height = "550" ,
              width = '600',
              plotlyOutput(outputId = "varPlot", height = "480")))),


# Datos ---------------------------------------------------------
      
      tabItem(
        tabName = 'datosbase',
        h2('Tabla de Datos'),
        sidebarLayout(
          sidebarPanel(
            selectInput('dataset',"Selecciona el Data Set:", 
                        choices = c("Inversion Montos Absolutos", 
                                    "Inversion Proporciones",
                                    "Inv Agrupadas por Rubro",
                                    "Inv en Serie Temporal",
                                    "Inv Variacion Porcentual")),
            p(strong("Inv Montos Absolutos:"),'Data Set con el monto invertido por rubro anual.'),
            p(strong("Inv Proporciones:"),'Data Set con el porcentaje invertido por rubro en cada año sobre la inversion total.'),
            p(strong("Inv Agrupadas por Rubro:"),'Data Set el monto invertido agrupado por Rubro.'),
            p(strong("Inv en Serie Temporal:"),'Data Set con Series Temporales de los Rubros por Año.'),
            p(strong("Inv Variacion Porcentual:"),'Data Set con la variacion porcentual de Inversion desde 1947.'),

            numericInput("obs", "Numero de Filas:", 20),
            br(),
            helpText(" Elegir el formato:"),
            radioButtons("type", "Tipo de Formato:",
                         choices = c("Excel (CSV)", "Text (TSV)","Text (Space Separated)")),
            br(),
            helpText(" Descargar las observaciones"),
            downloadButton('downloadData', 'Descargar')
          ),
          mainPanel(
            div(tableOutput('table'))))),


# Resumen ---------------------------------------------------------

      tabItem(
        tabName = "tabla",
        h2("Resumen"),
              fluidRow(
                box(
                  title = strong("Acerca de este trabajo"), solidHeader = TRUE,
                  p("Este es un trabajo realizado para visualizar y cuantificar los componentes de la inversion en ", strong("EE.UU"), "desde 1947 a 2017."),
                  p("Los dos objetivos princiales fueron ver el componente que mayor crecimiento tuvo en terminos de los demas y los volumenes asignados a cada uno de ellos a traves del tiempo."),
                  p("Los resultados son significativos para el Rubro Digital, creciendo su inversion muy por encima de las demas"),
                  p("En volumen de dolares, el Rubro de mayor inversion es el de Infraestructura."),
                  p("Pudimos observar tambien que salud, inversion social y educacion son rubros de gran crecimiento."),
                  p(strong("Los datos representan valores de Inversión bruta en infraestructura en en millones de USD 2021 encadenados sin deflactar."))
                  
                ),
                box(
                  title = strong("Librerias:"), solidHeader = TRUE,
                  p("Las librerias utilizadas son: shiny, tidyverse, ggplot2, lubridate, dplyr, plyr, lares, ggcorrplot, plotly y DT."),
                  p("Link de donde se obtuvieron los datos:"),
                  tags$ul(
                    tags$li(tags$a(href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-10/readme.md", "Github R4DS"))
                  )
                  
                ),
                box(
                  title = strong("Trabajo hecho por:"), solidHeader = TRUE,
                  h5("Galoto Maximiliano"),
                  tags$ul(
                  tags$li(tags$a(href="https://www.linkedin.com/in/maximilianogaloto", "Linkedin")),
                  tags$li(tags$a(href="https://rpubs.com/MGaloto", "Rpubs")),
                  tags$li(tags$a(href="https://github.com/MGaloto", "GitHub")))
                  
                )
              )
              
      )
          )
    )
  
  
)


# SERVEER -----------------------------------------------------------------



server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Inversion Montos Absolutos" = chain_investment,
           "Inversion Proporciones" = chain_proporciones_totales,
           "Inv Agrupadas por Rubro" = chain_investment_group,
           "Inv en Serie Temporal" = chain_time_series,
           "Inv Variacion Porcentual" = porcentaje_final)
  
  })
  
  seconddata <- reactive({
    d <- datasetInput()

  })
  
  
  
  output$table <- renderTable({
    
    head(seconddata(), n = input$obs)
  })
  
  
  
  fileext <- reactive({
    switch(input$type,
           "Excel (CSV)" = "csv", "Text (TSV)" = "txt","Text (Space Separated)" = "txt")
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    
    
    filename = function() {
      paste(input$dataset, fileext(), sep = ".")
      
    },
    
    
    content = function(file) {
      sep <- switch(input$type, "Excel (CSV)" = ",", "Text (TSV)" = "\t","Text (Space Separated)" = " ")
      
      
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  

  
  output$varPlot <- renderPlotly({
    
    
    
    plot_ly(percent_final)  %>%
      add_trace(x = ~percent_final$cat_2017,
                y = ~percent_final$variacion, 
                type = 'bar',
                name = "Variacion Inversion 1947 - 2017",
                marker = list(color = colorRampPalette(list('blue', 'red'))(length(unique(percent_final$cat_2017))), 
                              line = list(color = 'black', 
                                          width = 1.5))) %>%  
      layout(title = 'Variacion Inversion 1947 - 2017',
             yaxis = list(side = 'left', 
                          title = '% Inversion',
                          showgrid = FALSE,
                          zeroline = FALSE),
             xaxis = list( title = 'Rubro')) %>%  config(displayModeBar = F)
    
    
    
  })
  
  
  
  
  
  output$barPlotProp <- renderPlotly({
    
    
    
    plot_ly(chain_proporciones_totales)  %>%
      add_bars(x = ~chain_proporciones_totales$year[meta_cat == input$day],
               y = ~chain_proporciones_totales$proporcion_anual[meta_cat == input$day], 
               name = "Proporcion Sobre Inversiones Totales",
               marker = list(color='#1651db')) %>%  
      layout(title = paste('<b>Rubro: ', input$day, '</b>'),
             yaxis = list(side = 'left', title = '% Inversion',showgrid = FALSE, zeroline = FALSE),
             xaxis = list( title = 'Año'),
             legend = list(x = 1.04),
             paper_bgcolor='#ffffff',
             plot_bgcolor='#ffffff') %>%  config(displayModeBar = F)
    
    
  })
  
  
  output$distPlot <- renderPlotly({
    
    
    plot_ly(chain_proporciones) %>%
      add_bars(x = ~as.factor(chain_proporciones$meta_cat[chain_proporciones$year == input$bins]),
               y = ~chain_proporciones$proporcion[chain_proporciones$year == input$bins], 
               name = "Proporciones",
               marker = list(color='#460eb5')) %>%  
      layout(title = paste('Inversion en USD año: ',input$bins),
             yaxis = list(side = 'left', title = '% Inversion',showgrid = FALSE, zeroline = FALSE),
             xaxis = list( title = 'Proporcion de Inversion por Sectores'),
             legend = list(x = 1.04)) %>%  config(displayModeBar = F)
    
    
    
  })
  
  
  output$scatPlot <- renderPlotly({
    
    if(input$grafico == 1 ) {
    
      
      plot_ly(chain_investment_group) %>%
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Digital'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Digital'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Digital", 
                  line = list(color = '#1149f2', width = 1.5)) %>%  
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Health'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Health'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Health", 
                  visible = 'legendonly',
                  line = list(color = '#11baf2', width = 1.5)) %>%  
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Air/water/transp'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Air/water/transp'], 
                  type = 'scatter', 
                  visible = 'legendonly',
                  mode = 'lines',  
                  name = "Air/water/transp", 
                  line = list(color = '#3711f2', width = 1.5)) %>%  
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Education'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Education'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Education", 
                  line = list(color = '#f21133', width = 1.5)) %>%  
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Highways and sts'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Highways and sts'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Highways and sts", 
                  line = list(color = '#d17189', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Public safety'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Public safety'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  visible = 'legendonly',
                  name = "Public safety", 
                  line = list(color = '#8f5765', width = 1.5)) %>%  
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Total basic infrast'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Total basic infrast'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Total basic infrast", 
                  line = list(color = '#4d58a1', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Water supply'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Water supply'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  visible = 'legendonly',
                  name = "Water supply", 
                  line = list(color = '#4d77a1', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Conserv/development'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Conserv/development'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Conserv/development", 
                  line = list(color = '#6b7b8a', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Electric power'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Electric power'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Electric power", 
                  line = list(color = '#961b46', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Nat Gas/Petro'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Nat Gas/Petro'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  visible = 'legendonly',
                  name = "Nat Gas/Petro", 
                  line = list(color = 'red', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Sewer and waste'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Sewer and waste'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Sewer and waste", 
                  line = list(color = '#7368e3', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Total infrastructure'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Total infrastructure'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  visible = 'legendonly',
                  name = "Total infrastructure", 
                  line = list(color = '#7468e3', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Power'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Power'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Power", 
                  line = list(color = 'blue', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Social'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Social'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  visible = 'legendonly',
                  name = "Social", 
                  line = list(color = '#e0539e', width = 1.5)) %>% 
        add_trace(x = ~chain_investment_group$year[chain_investment_group$meta_cat == 'Transportation'],
                  y = ~chain_investment_group$total[chain_investment_group$meta_cat == 'Transportation'], 
                  type = 'scatter', 
                  mode = 'lines',  
                  name = "Transportation", 
                  line = list(color = '#5c53e0', width = 1.5)) %>%
        layout(title = 'Inversión bruta (dólares 2021 encadenados) en millones de USD.',
               yaxis = list(side = 'left', title = 'U$D',showgrid = FALSE, zeroline = FALSE),
               xaxis = list( title = 'Año'),
               legend = list(x = 1.05),
               hovermode = "compare") %>%
        add_annotations(
          xref="paper",
          yref="paper",
          x=1.35,
          y=-0.02,
          text="Elaboracion propia en \nbase a datos de \nBureau Of Economics Analysys",
          showarrow= F
        ) %>%  config(displayModeBar = F)
      
    }
    
    else{
      
      corr_cross(chain_time_series[,2:ncol(chain_time_series)], 
                 max_pvalue = 0.05, type = 1, top = 35)
      
    }
    
    
    
  })
  
  output$maxima_inversion <- renderValueBox({
    
    valueBox(
      value = paste('Maxima Inversion: ',max(chain_investment$gross_inv_chain)), 
      subtitle = paste("La Inversion Maxima es del Rubro: ",
                       chain_investment$meta_cat[which(chain_investment$gross_inv_chain == max(chain_investment$gross_inv_chain))],
                       'en el año: ',
                       chain_investment$year[which(chain_investment$gross_inv_chain == max(chain_investment$gross_inv_chain))]),
      icon = icon("align-justify", lib = "glyphicon"),
      color = "blue"
    )
    
  })
  
  output$maxima_variacion <- renderValueBox({
    
    valueBox(
      value = paste('%: ',max(percent_final$variacion)),
      subtitle = paste("Rubro con Maxima Variacion en Inversion desde 1947: ", percent_final$cat_2017[which(percent_final$variacion == max(percent_final$variacion))]),
      icon = icon("italic", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  output$minima_variacion <- renderValueBox({
    
    valueBox(
      value = paste('%: ',min(percent_final$variacion)), 
      subtitle = paste("Rubro con Minima Variacion Inversion desde 1947: ", percent_final$cat_2017[which(percent_final$variacion == min(percent_final$variacion))]),
      icon = icon("bold", lib = "glyphicon"),
      color = "light-blue"
    )
    
  })
  
}


# SHINY APP ---------------------------------------------------------------




shinyApp(ui, server)