#             Carlos Avila Arguello

#                   K-modas

# Queremos ver cómo cambian clústers de una muestra categórica al momento
# de cambair los parámetros dentro de la función kmodes(). 

# Los parámetros que puede cambiar el usuario son:
# modes = número de modas/clústers.
# iter.max = número máximo de iteraciones que hace el algoritmo.
# weighted = (TRUE/FALSE) para ver el modelo con una distancia ponderada o no.


# ----- Librerías           ----

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(klaR)
library(gridExtra)

# ----- Datos               ----

set.seed(1)

sexo <- as.factor(sample(c("hombre", "mujer"), 2000, TRUE, c(0.5, 0.5)))
estado_civil <-
  as.factor(sample(
    c("solterx", "casadx", "viudx", "divorciadx"),
    2000,
    TRUE,
    c(0.55, 0.3, 0.1, 0.05)
  ))
situ_labo <-
  as.factor(sample(c("empleadx", "desempleadx"), 2000, TRUE, c(0.7, 0.3)))
grpo_edad <-
  as.factor(sample(
    c("ninx", "joven", "adulto", "ancianx"),
    2000,
    TRUE,
    c(0.05, 0.35, 0.4, 0.2)
  ))
niv_educ <-
  as.factor(sample(c("basico", "medio", "avanzado"), 2000, TRUE, c(0.6, 0.3, 0.1)))

info_personal <-
  as.data.frame(cbind(sexo, estado_civil , situ_labo, grpo_edad, niv_educ))

info_personal$sexo <- as.factor(info_personal$sexo)
info_personal$estado_civil <- as.factor(info_personal$estado_civil)
info_personal$situ_labo <- as.factor(info_personal$situ_labo)
info_personal$grpo_edad <- as.factor(info_personal$grpo_edad)
info_personal$niv_educ <- as.factor(info_personal$niv_educ)

  # Información desglosada de los datos        

sexo1 <- c("1. Hombre", "2. Mujer", "", "")
estado_civ_1 <- c("1. Casado(a)", "2. Viudo(a)", "3. Soltero(a)", "4. Divorciado(a)")
situ_labo1 <- c("1. Desmpleado(a)", "2. Empleado(a)", "", "")
grupo_eda1 <- c("1. Adulto(a)", "2. Anciano(a)", "3. Joven", "4. Niño(a)")
nivel_esco1 <- c("1. Avanzado", "2. Básico", "3. Medio", "")

informacion <- as.data.frame(cbind(sexo1, estado_civ_1, situ_labo1, grupo_eda1, nivel_esco1), row.names = FALSE)
colnames(informacion) <- c("Sexo", "Estado Civil", "Situación Laboral", "Grupo de Edad", "Nivel Escolar")


# ----- Interfás de usuario ---- 

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  # shinythemes::themeSelector(),
  titlePanel("K-modas en R"),
  
  sidebarLayout(
    
    # Entradas que cambia el usuario
    sidebarPanel(
      
      actionButton("mas_info", "¿De qué trata la aplicación?"),
      
      selectInput("k",
                  "Número de clústers (k)", 
                  choices = seq(2, 10, by = 1)),
      
      sliderInput("iterations",
                  "Iteraciones para el algoritmo", 
                  min = 1, max = 50, value = 10),
      
      checkboxInput("ponderations", "Disimilaridad ponderada", FALSE),
      
      checkboxInput("veloz", "Optimizar algorítmo", FALSE),

      strong("Implementación en"), icon("r-project"),
      textOutput("funcion1"),
      textOutput("funcion2"),
      textOutput("funcion3"),
      textOutput("funcion4"),
      textOutput("funcion5"),
      
      actionButton("crea_diag", 
                   "Generar resultados", 
                   class = "btn-primary")
    ),
    
    # Resultados que ve el usuario
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica", strong("Gráfico de puntos.") ,
                 "Este gráfico nos muestra cómo se distribuyeron las diferentes 
                   observaciones en los k-clúster (que elegiste) por color, para 
                   cada par de características de los individuos.",
                 plotOutput("grafico")),
        tabPanel("Datos", DT::DTOutput("moditas"),
                 strong("Modas, tamaño y distancia.") ,
                 "Las primeras 5 columnas de la tabla corresponden al valor de
                 las modas. La columna 'Freq' nos dice el número de observaciones en 
                 dicho clúster y 'modelo()$withindiff' es la distancia total 
                 que tienen entre sí las observaciones del clúster."),
        tabPanel("Distribución de datos", plotOutput("distr")),
        tabPanel("Descripción de datos", DT::DTOutput("info"),
                 "Esta tabla nos dice que por cada categoría (columna) tenemos 
                 valores numéricos asignados por etiqueta. Por ejemplo: 1. Mujer 
                 quiere decir que la etiqueta 'Mujer' dentro de la categoría 
                 'sexo', es representada por el 1 (en todos los gráficos).")
      )
    )
  )
)


# ----- Server              ---- 

server <- function(input, output, session){
  
  # Buca más información
  
  output$tab <- renderUI({
    a("Leer artículo completo", href="https://k-modes.netlify.app/")
  })
  
  # Botón para pedir ayuda
  
  observeEvent(input$mas_info, {
      showModal(modalDialog(
        icon("check-circle"),
        " Esta app tiene el objetivo de que te familiarices con la 
              función 'kmodes' del programa R. ",
        uiOutput("tab"),
        icon("check-circle"),
        " Los datos sobre los que se aplica la función son de información
              personal de 2000 individuos (Descripción de datos.)"
      ))
  })
  
  # Para escribir los parámetros de la función

  output$funcion1 <- renderText({
    "kmodes(data = info_personal,"
  })
  output$funcion2 <- renderText({
    paste("modes = ", input$k, ",", sep = "")
  })
  output$funcion3 <- renderText({
    paste("iter.max = ",input$iterations,",", sep = "")
  })
  output$funcion4 <- renderText({
    paste("weighted = ", input$ponderations,",", sep = "")
  })
  output$funcion5 <- renderText({
    paste("fast = ", input$veloz, ")", sep = "")
  })
    
  # Botón para calcular todo
  
  modelo <- eventReactive(input$crea_diag, { 
                set.seed(1)
                kmodes(data = info_personal, 
                       modes = as.numeric(input$k), 
                       iter.max = input$iterations,
                       weighted = input$ponderations,
                       fast = input$veloz) 
              })
 
  # Clustering
  
  output$grafico <- renderPlot({
    
      clusters <- modelo()$cluster #Tomamos el vector de clusters por observación
      dat_clos <- cbind(info_personal, clusters) # y lo unimos al data original
    
              grid.arrange(
              # SEXO      
              ggplot(dat_clos, 
                       aes(x = sexo, y = situ_labo)) + 
                geom_jitter(color = clusters) + 
                xlab("Sexo")+
                ylab("Situación Laboral")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = sexo, y = estado_civil)) + 
                geom_jitter(color = clusters) + 
                xlab("Sexo")+
                ylab("Estado Civil")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = sexo, y = grpo_edad)) + 
                geom_jitter(color = clusters) + 
                xlab("Sexo")+
                ylab("Grupo de Edad")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = sexo, y = niv_educ)) + 
                geom_jitter(color = clusters) + 
                xlab("Sexo")+
                ylab("Nivel Educativo")+
                theme_minimal(),
              # ESTADO CIVIL
              ggplot(dat_clos, 
                     aes(x = estado_civil, y = situ_labo)) + 
                geom_jitter(color = clusters) + 
                xlab("Estado Civil")+
                ylab("Situación Laboral")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = estado_civil, y = grpo_edad)) + 
                geom_jitter(color = clusters) + 
                xlab("Estado Civil")+
                ylab("Grupo de Edad")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = estado_civil, y = niv_educ)) + 
                geom_jitter(color = clusters) + 
                xlab("Estado Civil")+
                ylab("Nivel Educativo")+
                theme_minimal(),
              # SITUACIÓN LABORAL
              ggplot(dat_clos, 
                     aes(x = situ_labo, y = grpo_edad)) + 
                geom_jitter(color = clusters) + 
                xlab("Situación Laboral")+
                ylab("Grupo de Edad")+
                theme_minimal(),
              ggplot(dat_clos, 
                     aes(x = situ_labo, y = niv_educ)) + 
                geom_jitter(color = clusters) + 
                xlab("Situación Laboral")+
                ylab("Nivel Educativo")+
                theme_minimal(),
              # GRUPO DE EDAD vs NIVEL EDUCATIVO
              ggplot(dat_clos, 
                     aes(x = grpo_edad, y = niv_educ)) + 
                geom_jitter(color = clusters) + 
                xlab("Grupo de Edad")+
                ylab("Nivel Educativo")+
                theme_minimal(),
              ncol = 5)
  })
  
  # Modas
  
  output$moditas <- DT::renderDT({
    
    as.data.frame(cbind(modelo()$modes, modelo()$size, modelo()$withindiff))
  
  })
  
  # Dsitribucíon de datos
  
  output$distr <- renderPlot({grid.arrange( 
    
              ggplot(info_personal, aes(x = sexo)) + 
                geom_bar(fill = c("#922B21", "#CD6155")) + 
                ggtitle("Sexo") +
                xlab("")+
                ylab("")+
                theme_minimal(),
              ggplot(info_personal, aes(x = estado_civil)) + 
                geom_bar(fill = c("#1B4F72", "#21618C", "#2874A6", "#D4E6F1")) + 
                ggtitle("Estado Civil") + 
                xlab("")+
                ylab("")+
                theme_minimal(),
              ggplot(info_personal, aes(x = situ_labo)) + 
                geom_bar(fill = c("#F8C471", "#F39C12")) + 
                ggtitle("Situación Laboral") + 
                xlab("")+
                ylab("")+
                theme_minimal(),
              ggplot(info_personal, aes(x = grpo_edad)) + 
                geom_bar(fill = c("#512E5F", "#9B59B6", "#D7BDE2", "#F5EEF8")) + 
                ggtitle("Grupo Edad") + 
                xlab("")+
                ylab("")+
                theme_minimal(),
              ggplot(info_personal, aes(x = niv_educ)) + 
                geom_bar(fill = c("#0E6251", "#17A589", "#A2D9CE")) + 
                ggtitle("Nivel Educativo") +  
                xlab("")+
                ylab("")+
                theme_minimal(),
          ncol = 3, top  = "Información Personal")
    })

  # Descripción de datos
  
  output$info <- DT::renderDT({
    
    informacion
    
  })
  
}

# ----- Carga de la app     ---- 

shinyApp(ui = ui, server = server)









