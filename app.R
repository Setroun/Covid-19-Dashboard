
#Carga e instalacion de librerias


library(viridis)
library(hrbrthemes)
library(lubridate)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)

#Procesamiento de los Datos

#Filtrado de datos a los ultimos 6 meses: Desde 2022-01-30 hasta 2022 - 07-30

#Convertir factor a fecha
Datafecha$`Fecha Reporte Web` = as.Date(Datafecha$`Fecha Reporte Web`)
data$fecha.reporte.web = as.Date(data$fecha.reporte.web)
data$Fecha.de.diagnóstico = as.Date(data$Fecha.de.diagnóstico)
data$Fecha.de.recuperación = as.Date(data$Fecha.de.recuperación)
data$Fecha.de.muerte = as.Date(data$Fecha.de.muerte)


#Creacion de variables tipo fecha
date1 = as.Date("2022-01-30")
date2 = as.Date("2022-07-30")
tiempo_recuperacion = interval(start = data$Fecha.de.diagnóstico,end = data$Fecha.de.recuperación) / days()
tiempo_muerte = interval(start = data$Fecha.de.diagnóstico,end = data$Fecha.de.muerte) / days()

#Convertir a dataframe los tiempos de recuperacion y muerte

tiempos_enfermedad = data.frame(tiempo_muerte,tiempo_recuperacion)


#Filtrado
datos_ultimos_6_meses = data[data$fecha.reporte.web %in% date1:date2, ]

# Media de tiempo de recuperacion y de muerte

tiempo_recuperacion_sin_na = na.omit(tiempo_recuperacion)
mean(tiempo_recuperacion_sin_na)
tiempo_muerte_sin_na = na.omit(tiempo_muerte)
mean(tiempo_muerte_sin_na)

#Media incidencia epidemiologica

incidencias_sin_na = na.omit(Datadepto$`INCIDENCIA EPIDEMIOLOGICA (NUMERO DE CASOS / POBLACION)`)
mean(incidencias_sin_na)

#Estadisticas Edad

summary(datos_ultimos_6_meses$Edad)


#Coeficientes de correlacion

#Poblacion VS Numero de casos

coeficiente_pearson_pobl = cor(Datadepto$`NUMERO DE CASOS`,Datadepto$POBLACION, method = "p")
coeficiente_spearman_pobl = cor(Datadepto$`NUMERO DE CASOS`,Datadepto$POBLACION, method = "s")
coeficiente_kendall_pobl= cor(Datadepto$`NUMERO DE CASOS`,Datadepto$POBLACION, method = "k")


#Poblacion VS Numero de casos

coeficiente_pearson_PIB = cor(Datadepto$`NUMERO DE CASOS`,Datadepto$`PIB PER CAPITA (2020)`, method = "p")
coeficiente_spearman_PIB = cor(Datadepto$`NUMERO DE CASOS`,Datadepto$`PIB PER CAPITA (2020)`, method = "s")
coeficiente_kendall_PIB = cor(Datadepto$`NUMERO DE CASOS`,Datadepto$`PIB PER CAPITA (2020)`, method = "k")


#Modelo de regresion lineal scatterplots

modelo_lineal_pobl = lm(Datadepto$`NUMERO DE CASOS`~ Datadepto$POBLACION)
modelo_lineal_PIB = lm(Datadepto$`NUMERO DE CASOS`~ Datadepto$`PIB PER CAPITA (2020)`)


summary(modelo_lineal_pobl)

#Graficas
#HIstogramas

histograma_edad = ggplot(datos_ultimos_6_meses,aes(x=datos_ultimos_6_meses$Edad)) +
  geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("AGE HISTOGRAM") + xlab("Age") + ylab("Frequency") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#Diagramas de densidad

densidad_tiempo_recuperacion = ggplot(tiempos_enfermedad, aes(tiempos_enfermedad$tiempo_recuperacion)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.9)  + 
  xlab("Recovery Time") + ylab("Probability") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


densidad_tiempo_muerte = ggplot(tiempos_enfermedad, aes(tiempos_enfermedad$tiempo_muerte)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  xlab("Death Time") + 
  ylab("Probability") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )



#Boxplot edad por sexo


boxplot_edadvssexo = ggplot(datos_ultimos_6_meses,aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_boxplot(notch = TRUE, notchwidth = 0.2, outlier.colour="red",
               outlier.fill="red",
               outlier.size=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.2)  +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Age Vs Sex Boxplot") +
  xlab("Sex") + ylab("Age")


#Barras casos por sexo

bar = ggplot(datos_ultimos_6_meses, aes(x = Sexo)) + geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Cases by Sex Barplot") +
  xlab("Sex") + 
  ylab("Cases") + 
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#Serie de tiempo

serie_tiempo_general<- ggplot(Datafecha, aes(x=Datafecha$`Fecha Reporte Web`, y=Datafecha$`Numero de casos`)) +
  geom_line(color = "darkgreen") +
  ggtitle("Time Series Chart") +
  xlab("Date") + 
  ylab("Cases by day") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#Scatterplot and regression casos vs poblacion

scatterplot_depto_pobl = ggplot(Datadepto, aes(x = Datadepto$POBLACION , y = Datadepto$`NUMERO DE CASOS`)) + geom_point() +
  geom_smooth(method = 'lm', color = "darkgreen") +
  ggtitle("Linear Regression for Population Vs Cases") +
  xlab("Population") + 
  ylab("Cases") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#ScatterplotCasos and regression vs PIB

scatterplot_depto_PIB  = ggplot(Datadepto, aes(x = Datadepto$`PIB PER CAPITA (2020)`, y =Datadepto$`NUMERO DE CASOS` )) +
  geom_point() +  geom_smooth(method = "lm", color = "darkgreen") +
  ggtitle("Linear Regression for Population Vs Cases") +
  xlab("PIB") + 
  ylab("Cases") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#Doughnut chart

#Creating dataframe

current_state <- data.frame(
  Recovery = c("Active", "Death", "No info", "Recovered"),
  count=c(19163, 3929, 2615,324452))

# Computing percentages
current_state$fraction = current_state$count / sum(current_state$count)

# Compute the cumulative percentages of the chart
current_state$ymax = cumsum(current_state$fraction)

# Compute the bottom of each rectangle plotted 
current_state$ymin = c(0, head(current_state$ymax, n=-1))

# Plotting the doughnut chart

doughnut_Currenstate = ggplot(current_state, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Recovery)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  ggtitle("Doughnut chart for Recovery") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


#UI

#HEADER

header = dashboardHeader(title = HTML("Covid 19 Dashboard"), 
                         disable = FALSE, 
                         titleWidth  = 550,
                         dropdownMenu(type = 'message',
                                      messageItem(
                                        from = "sgmorenob@unal.edu.co",#'Feedback and suggestions',
                                        message =  "",
                                        icon = icon("envelope"),
                                        href = "mailto:sgmorenob@unal.edu.co"
                                      ),
                                      icon = icon('comment')
                         ),
                         dropdownMenu(type = 'message',
                                      icon = icon("share-alt"),
                                      messageItem(
                                        from = 'Linkedin',
                                        message = "",
                                        icon = icon("linkedin"),
                                        href = "https://www.linkedin.com/in/sebastian-gustavo-moreno-bar%C3%B3n-8313a3125/"
                                      ),
                                      messageItem(
                                        from = 'Github',
                                        message = "",
                                        icon = icon("github"),
                                        href = "https://github.com/Setroun"
                                      )
                                      
                         )
                         
)


#Sidebar

sidebar = dashboardSidebar(
  
  
  sidebarMenu(id = "sidebarID",
              menuItem("Data", icon = shiny::icon("database"),
                       menuSubItem("Database",tabName = "datos"),
                       menuSubItem("Data by department",tabName = "datosdepto"),
                       menuSubItem("Cases by date",tabName = "datosfecha")
              )
              
  ),
  
  sidebarMenu(id = "sidebarID",
              menuItem("Charts", icon = shiny::icon("line-chart"),
                       menuSubItem("Histogram by age",tabName = "histograma"),
                       menuSubItem("Boxplot",tabName = "boxplot"),
                       menuSubItem("Time Series",tabName = "serie_tiempo"),
                       menuSubItem("Scatterplot PIB",tabName = "scatterplot_depto_PIB"),
                       menuSubItem("Scatterplot Pop",tabName = "scatterplot_depto_pobl"),
                       menuSubItem("Bar Cases per Sex",tabName = "bar"),
                       menuSubItem("Doughnut chart for recovery",tabName = "doughnut"))
  )
)








#Body


body = dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "datos",
            
            fluidRow(
              column(width=12,  
                     valueBox("350159 cases","Period: February - July 2022",icon=icon("virus-covid"),color="orange"),
                     infoBox("Department with more cases", "Cundinamarca", icon = icon("earth-americas"), color = "purple"),
                     infoBox("Average recovery time", "10.32 days", icon = icon("bed-pulse")),
                     infoBox("Average death time", "27.29 days" , icon = icon("skull"), color = "black"),
                     infoBox("Epidemiological incidence", "39,69%" , icon = icon("viruses"), color = "red")
              ),
              fluidRow(box(title="COVID 19 GENERAL DATASET",DT::dataTableOutput("datos"), width=12, status="primary", solidHeader=TRUE)))
    ), 
    
    tabItem(tabName = "datosfecha",
            fluidRow(box(title="No of cases by date table",DT::dataTableOutput("datosfecha"), width=12, status="primary", solidHeader=TRUE))
            
    ), 
    
    tabItem(tabName = "datosdepto",
            fluidRow(box(title="Information by department table",DT::dataTableOutput("datosdepto"), width=12, status="primary", solidHeader=TRUE))
    ), 
    
    tabItem(tabName = "histograma",
            
            fluidRow(
              column(width=12,  
                     valueBox("Age mean","43.51428",icon=icon("pi"),color="orange"),
                     valueBox("Age st dev","21.4965",icon=icon("pi"),color="blue")
              ),
              
              fluidRow(box(title="General Age Histogram",plotOutput("histograma"), width=12, status="primary", solidHeader=TRUE))
              
            )
    ),
    
    tabItem(tabName = "boxplot",
            
            fluidRow(
              column(width=12,  
                     valueBox("1st Quartile","28",icon=icon("pi"),color="orange"),
                     valueBox("Median","42",icon=icon("pi"),color="blue"),
                     valueBox("3rd Quartile","60",icon=icon("pi"),color="blue"),
                     valueBox("Min data","112",icon=icon("pi"),color="blue"),
                     valueBox("Max data","1",icon=icon("pi"),color="blue")
              ),
              
              fluidRow(box(title="Age by sex boxplot",plotOutput("boxplot"), width=12, status="primary", solidHeader=TRUE))
            )      
    ),
    
    tabItem(tabName = "serie_tiempo",
            fluidRow(
              column(width=12,  
                     valueBox("Day with more cases","04/02/2022 - 2 cases",icon=icon("pi"),color="orange"),
                     valueBox("Day with less cases","24/07/2022 - 24649 cases",icon=icon("pi"),color="blue")
              ),
              fluidRow(box(title="General Cases Time Series over the last 6 months",plotOutput("serie_tiempo"), width=12, status="primary", solidHeader=TRUE)) 
            )
    ),
    
    tabItem(tabName = "scatterplot_depto_pobl",
            
            fluidRow(
              column(width=12,  
                     valueBox("Pearson Coeff.","0.9323677",icon=icon("pi"),color="orange"),
                     valueBox("Spearman Coeff.","0.7892229",icon=icon("pi"),color="blue"),
                     valueBox("Kendall Coeff.","0.6370968",icon=icon("pi"),color="red"),
                     valueBox("R-Squared","0,865",icon=icon("pi"),color="black")
                     
                     
              ),
              fluidRow(box(title="Scatterplot and linear regression model for population and number of cases",plotOutput("scatterplot_depto_pobl"), width=12, status="primary", solidHeader=TRUE))
            )
    ) ,
    
    tabItem(tabName = "scatterplot_depto_PIB",
            fluidRow(
              column(width=12,  
                     valueBox("Pearson Coeff.","0.4807095",icon=icon("pi"),color="orange"),
                     valueBox("Spearman  Coeff.","0.5593842",icon=icon("vpi"),color="blue"),
                     valueBox("Kendall Coeff.","0.391129",icon=icon("pi"),color="red"),
                     valueBox("R-Squared","0,2055",icon=icon("pi"),color="black")
                     
              ),       
              fluidRow(box(title="Scatterplot and linear regression model for PIB and number of cases",plotOutput("scatterplot_depto_PIB"), width=12, status="primary", solidHeader=TRUE))
            )
    ),
    tabItem(tabName = "bar",
            fluidRow(box(title="Barchart for number of cases by sex",plotOutput("bar"), width=12, status="primary", solidHeader=TRUE)),
            
    ),
    tabItem(tabName = "doughnut",
            fluidRow(box(title="Doughnot for recovery",plotOutput("doughnut"), width=12, status="primary", solidHeader=TRUE)),
    )
  )
)
 







ui = dashboardPage(title = "Covid 19 Dashboard", skin = "green",
                   header = header,
                   sidebar = sidebar,
                   body = body)

#SERVER

server = function(input,output){
  
  output$datos = DT::renderDataTable(datos_ultimos_6_meses)
  output$datosfecha = DT::renderDataTable(Datafecha)
  output$datosdepto = DT::renderDataTable(Datadepto)
  output$histograma = renderPlot({histograma_edad})
  output$boxplot = renderPlot({boxplot_edadvssexo})
  output$serie_tiempo = renderPlot({serie_tiempo_general})
  output$scatterplot_depto_PIB = renderPlot({scatterplot_depto_PIB})
  output$scatterplot_depto_pobl = renderPlot({scatterplot_depto_pobl})
  output$bar = renderPlot({bar})
  output$doughnut = renderPlot({doughnut_Currenstate})
  
}

#APP

shinyApp(ui,server)