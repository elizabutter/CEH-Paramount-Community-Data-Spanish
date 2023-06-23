#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(fresh)
library(bslib)
library(shinyBS)
library(bs4Dash)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plotly)
library(RcppRoll)
library(viridisLite)
library(rintrojs)


#Read the CSV file that contains the air monitoring data
#This CSV file is written by the dataImportHandler.R script
airDataFull <- readr::read_csv("Output Data/fullData.csv") 


#ASSEMBLING THE SLIDER DATES
#These dates and durations are used as the default dates for the input slider for the main graph
firstDateFull <- as.Date(airDataFull[[1]][1])
observationsTolastDateFull <- nrow(airDataFull)
lastDateFull <- as.Date(airDataFull[[1]][observationsTolastDateFull])
differenceOfDaysFull <- lastDateFull - firstDateFull


#The text for the hover-popups. The popups activate when the user hovers over the (i) symbols on the main graph page
tooltipText <- tibble(text = c(paste("En el control deslizante debajo de la gráfica, seleccióne la fecha para ver cómo han cambiado los niveles con el tiempo."),
                               paste('La EPA es la Agencía Gubernamental de Protección Ambiental de los Estados Unidos. <br> La EPA utiliza la mejor información científica para garantizar que tengamos aire, tierra y agua limpia. ')))


#these colors are tuned to meet the WCAG 2 AAA standard for accessibility.
#https://polypane.app/color-contrast/

somersetColor <- "#F9A620"
vermontColor <- "rgb(101, 155, 208)"



header <- dashboardHeader(
  
  #load the CEH logo
  title = HTML("<div style = 'height: 35px; text-align: center;'>",
               '<img class="header-picture" src="ceh.logo.rgb.FNL_2.png" width = "105px">',
                 "</div>"),
  
  #load the creator credit button on the top right of the user interface
  rightUi = userOutput("user")
)




sidebar <- bs4DashSidebar(
  
  #set the width of the sidebar
  use_theme(create_theme(bs4dash_layout(sidebar_width = "172px"))),
  
  #Initialize the package used for the tutorial
  introjsUI(),

  
  bs4SidebarMenu(
    #This sets an identifier for the sidebar. This is so we can use actionButtons to change pages without using the sidebar buttons
    
    id = "sidebarID",
    
    
    bs4SidebarMenuItem(
      "Datos",
      tabName = "dashboard",
      icon = icon("area-chart")
    ),
    
    bs4SidebarMenuItem(
      HTML("<p style ='font-size:0.9em'>Antecedentes <br> y Recursos</p>"),
      tabName = "background",
      icon = icon("info")
    ),
    
    bs4SidebarMenuItem(
      HTML("<p style ='font-size:0.9em'>Configuración</p>"),
      icon = icon("gear"),
      tabName = "devSetting"
    )
    
  )

)





body <- bs4DashBody(
  
  # Styling Well 
  #https://engineering-shiny.org/css.html
  #Set the id parameter for each box to whatever
  #In the style tag (see below), put a #before the id to change the color
  #Styling cannot be applied to specific box instances, they must be applied according to their statuses.
  #Valid statuses: https://www.rdocumentation.org/packages/shinydashboard/versions/0.7.2/topics/validStatuses
  #This tag applies to all boxes with the "info" status. 
  tags$style(
    HTML(
      "
      
      div.polaroid {
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  text-align: center;
  margin: 15px;
}

            p{ font-size:1.0em
            }
            
            .mt-2{ font-size:1.15em
            }

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#222d32
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#222d32
                    }
                    "
    )
  ),
  
  #Slightly contrary to the comment above, you can actually change the background color of boxes bases on their ID
  #On the backend, the ID only applies to the HTML div class that contains the background of the box. 
  #It exludes the header and the header and the outline. Very frustrating. 
  #To learn more, run the app, right click, inspect element, examine the HTML for the boxes.
  tags$style(".small-box.bg-purple { background-color:", somersetColor, "!important; color: #000000 !important; margin-bottom: 15px}"),
  tags$style(".small-box.bg-maroon { background-color:", vermontColor, "!important; color: #000000 !important; margin-bottom: 15px}"),
  tags$style(".small-box-footer {height: 0px !important;}"),
  
  
  tags$style("#modalStartTut {text-align: center; font-size: 20px; background-color: #4CB944; color: white;}"),
  tags$style("#confirmDataCheckButton {text-align: center; font-size: 20px; background-color: #4CB944; color: white;}"),
  
  tags$style("#confirmRevertDataButton {text-align: center; font-size: 20px; background-color: #dc3545; color: white;}"),
  tags$style("#closeModal {text-align: center; font-size: 20px}"),
  
  tags$style(".small-box-subtitle {display: none;}"), #this removes an empty html box from inside the ValueBoxes
  
  tags$style(".card-header {padding-left: 0.75rem;}"), # moves the icon in the red box further to the left, but also affects other boxes
  
  tags$style(".nav-link {padding-left: 8px;}"), # moves the icon in the sidebar boxes further to the left
  
  tags$style(".card-header.border-0 { padding-top: 5px;padding-bottom: 5px;}"), #this fixes the big gap on top of the graph box
  
  tags$style(".fa.fa-info.nav-icon {transform: translateY(-80%);}"), #this fixes the location of the info icon in the sidebar
  tags$style(".brand-image.img-circle.elevation-3 {box-shadow: NULL;"), #this fixes the location of the info icon in the sidebar
  


  tags$style("#devWebsite {margin-left: 24px; !important}"),
  
  tags$style(".modal-footer {text-align: center}"),
  tags$style(".modal-content {
    top: 100px;
    right: 0px;
    bottom: 0;
    left: 0;
    z-index: 10040;
    overflow: auto;
    overflow-y: auto;
    !important;
  }"
  ),
  
  tags$style(".modal-dialog.modal-xl{
    top: -100px;
    right: 100px;
    bottom: 0;
    left: 0;
    z-index: 10040;
    overflow: auto;
    overflow-y: auto;
    max-width: 1300px;
    !important;
  }"
  ),
  
  
  tags$style("li.user-header{
    height: 205px!important;
  }"
  ),
  
  
  #enables tooltips to be shown
  tags$script(HTML(
    "$(document).ready(function() {
    $('body').tooltip({ selector: '[data-toggle=tooltip]' });
});"
  )),
  

  bs4TabItems(
    
    bs4TabItem(tabName = "dashboard",
            
            #Row 1 Start
            fluidRow(
              #Row 1, Column 1 Start
              column(width = 9, 
                     style = "padding-right: 0px; padding-left: 0px;",
                     align = "center",
                     
                     #Box to contain graph and slider Start
                     box(
                       id = "mainGraphTutID", #this is so we can reference it in css styling
                       headerBorder = FALSE,
                       collapsible = FALSE,
                       solidHeader = F,
                       width = 12,
                       style = "padding: 0px;",
                       
                       introBox(
                         plotlyOutput(outputId = "primaryAirMonitorGraph", height = "600"),
                         
                         data.step = 1,
                         data.intro = HTML("<h5>Gráfico de Datos de Monitoreo de Aire</h5>
                                            <p>Aquí, puede ver todos los datos recopilados por los monitores de aire.</p>
                                            <i>Las lecturas por encima de la línea roja superan el umbral de riesgo de la EPA.</i>
                                            <br><br>
                                           <b>Pruebe esto: </b>Pase el ratón sobre cada punto de datos para obtener más información sobre esa lectura específica."),
                         data.position = "right"
                       ), 
                       
                       introBox(
                         sliderInput(
                           
                           inputId = "graphDateSelection",
                           label = ("Selección de Fechas"),
                           min = firstDateFull,
                           max = lastDateFull,
                           #-6 is for minus 6 months
                           value = c(firstDateFull, lastDateFull),
                           width = "80%"
                         ),
                         
                         data.step = 4,
                         data.intro = "<h5>Selección de Fechas</h5>
                                            <p>Haga clic y arrastre el dislizante de cada extremo para cambiar las fechas que se muestran en el Gráfico Principal.</p>
                             <p>¡Observe cómo cambian los promedios de datos en la columna derecha en función de las fechas que seleccione!</p>"
                       )
                       
                     ),#Box to contain graph and slider End
                     
                     
                     box(
                       status = "lightblue",
                       collapsible = FALSE,
                       title = "Additional Data",
                       solidHeader = TRUE,
                       width = 12,
                       HTML(
                         paste(
                           "<p>La Ciudad de Paramount está monitoreando activamente el cromo hexavalente en cinco ubicaciones, los resultados están disponibles ",
                           a(href = "https://tbsysclient.com/paramount/paramounthexchrometbsys.pdf", target =
                               "_blank",
                             "aquí."),
                           "</p>",
                           "<p>Vea historeal de datos de monitoreo del aire ",
                           a(href = "https://ceh-data.shinyapps.io/ParamountHexChromeTool/", target =
                               "_blank",
                             "aquí"),
                           ", recopilados por el Distrito de Gestión de la Calidad del Aire de la Costa Sur.",
                           "</p>"
                           
                         )
                       )
                     )
                     
                     
              ),#Row 1, Column 1 End
              
              
              #Row 1, Column 2 Start
              column(width = 3,
                     style = "padding-right: 0px;",
                     
                     introBox(
                       
                       
                       
                       valueBoxOutput("somersetValueBox", width = 12),
                       valueBoxOutput("vermontValueBox", width = 12),
                       
                       data.step = 2,
                       data.intro = "<h5>Promedios de Datos</h5>
                                            <p>Estas son las lecturas promedio de cada monitor de aire dentro del marco de tiempo seleccionado.</p>
                     <p>Por predeterminación, estos promedios provienen de toda la amplitud de datos disponibles.</p>",
                       data.position = "left"
                     ),
                     
                     introBox(
                       box(
                         id = "epaBox",
                         collapsible = FALSE,
                         status = "danger",
                         
                         
                         
                         
                         title = HTML(
                           '<table
                                                  <tr>
                                                    <td><i data-html="true"; style="font-size: 22px"; class="fa fa-info-circle"; data-toggle="tooltip"; data-placement="left"; 
                                                      title ="', tooltipText[[1]][2], '"></i></td>
                                                    <td style="padding-left:10px"> <b id="epaTitle"> Límite de Riesgo <br> de la EPA </b></td>
                                                    <td><i style="font-size: 40px; padding-left:35px; color: #9D2835"; class="fa fa-exclamation-triangle";></i></td>
                                                  </tr>
                                                </table>'
                         ),
                         width = 12,
                         solidHeader = T,
                         HTML(
                           paste(
                             "<p>",
                             "El límite de riesgo establecido por la EPA para el cromo hexavalente en el aire es de <b>0.01 ng/m3</b>,",
                             "<i><u>",
                             " exposición crónica por encima de este límite se asocia con un riesgo elevado de desarrollar cáncer de los pulmónes.",
                             "</i></u>",
                             " </p>"
                           )
                         )
                       ),
                       data.step = 3,
                       data.intro = HTML("<h5>El límite umbral de riesgo de acuerdo a la EPA</h5>
                       <p>Durante toda la vida, estar expuesto a niveles de cromo hexavalente por encima de este límite puede poner a las personas en un mayor riesgo de desarrollar cáncer.</p>
                      <b>Recuerde: </b>  <i>Las lecturas por encima de la línea roja exceden el umbral de riesgo de la EPA de EE. UU.</i>"),
                       data.position = "left"
                     ), 
                     
                     
                     
                     # actionButton(
                     #   "dataAboutButton",
                     #   "Acerca de Estos Datos",
                     #   icon("info-circle"),
                     #   style =
                     #     "color: #fff; background-color: #337ab7;
                     #                                                     border-color: #2e6da4;
                     #                                                     margin-left: 8px;"
                     # ),
                     
                     
                     # introBox(
                     downloadButton(
                       "dataDownload",
                       "Descargar Datos",
                       icon("download"),
                       style =
                         "color: #fff; background-color: #6c757d;
                                                                         border-color: #2e6da4;
                                                                         margin-left: 8px;
                                                                         margin-top: 15px;"
                     ),
                     #   data.step = 5,
                     #   data.intro = "Please feel welcome to download the raw air monitoring data for personal use.",
                     #   data.position = "left"
                     # ),
                     
                     
                     introBox(
                       actionButton(
                         "help",
                         "Inicio Tutorial",
                         icon("book"),
                         style =
                           "color: #fff; background-color: #4CB944;
                                                                         border-color: #2e6da4;
                                                                         margin-left: 8px;
                                                                         margin-top: 15px;"
                       ),
                       data.step = 5,
                       data.intro = HTML("<h5>¡Tutorial Completo!</h5>
                                           <p>Si desea iniciar el tutorial de nuevo, simplemente haga clic en este botón.</p>"),
                       data.position = "left"
                     )
                     
              )#Row 1, Column 2 End
              
              
            ) #Row 1 end
            
    ), #dashboard bs4TabItem End
    
    bs4TabItem(tabName = "background",

            htmlOutput("myBackground")
    ),
    bs4TabItem(tabName = "devSetting",
               
               fluidRow(
                
                 column(width = 6, 
                        
                        box(width = 12,
                            title = "Código fuente",
                            collapsible = FALSE,
                            status = "purple",
                            solidHeader = TRUE,
                            HTML("¡El código fuente de esta aplicación es de código abierto y de uso gratuito!<br><br>"),
                            actionButton("githubButton", "Vea el código en GitHub",
                                         onclick ="window.open('https://github.com/elizabutter/CEH-Paramount-Community-Data-Spanish', '_blank')")
                            
                        )
                        
                 ),
                 
                 column(width = 6, 
                        
                        box(width = 6,
                            title = "Configuración del Administrador",
                            collapsible = FALSE,
                            status = "lightblue",
                            solidHeader = TRUE,
                            
                            
                            textInput(inputId = "devPassInput",
                                      label = "Introduzca su contraseña", 
                                      value = ""),
                            
                            actionButton(
                              "devPasswordBtn",
                              "Iniciar sesión")
                        )
                        
                 )
               )
               
    
            
            
    )
    
  ) 
)



ui <- bs4DashPage(
  
  dark = NULL,
  header,
  sidebar,
  body
  
)


tickFont <- list(family = 'Arial',
                 size = 16,
                 color = 'rgb(82, 82, 82)')

legendFont <- list(family = 'Arial',
                   size = 16,
                   color = 'rgb(82, 82, 82)')

axisTitleFont <- list(family = 'Arial',
                      size = 18,
                      color = 'rgb(82, 82, 82)')

graphTitleFont <- list(family = 'Source Sans Pro',
                       size = 24,
                       
                       color = 'rgb(82, 82, 82)')

font1 <- list(family = 'Arial',
              size = 16,
              color = "black")

settingsModal <- modalDialog(
  size = "xl",
  easyClose = TRUE,
  fluidRow(
    column(width = 6,
           box(width = 12,
               title = "Actualizar Datos Principales de la Gráfica",
               collapsible = FALSE,
               status = "olive",
               solidHeader = TRUE,
               
               HTML("Utilice el botón de abajo para obtener nuevos datos de las Hojas de Cálculo de Google "),
               HTML(paste("<br><b>Última actualización:</b>", file.info("Output Data/fullData.csv")$ctime), "<br><br>"),
               
               
               actionButton(
                 "dataCheckButton",
                 "Buscar Nuevos Datos"),
               
               HTML("<br><br>"),
               
               box(
                 width = 8,
                 title = "¿Algo se rompió?",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 
                 actionButton(
                   "revertDataButton",
                   "Revertir a Datos Antiguos")
               )
               
               
               
           )
           
    ),
    column(width = 6,
           box(width = 12,
               title = "Revertir a Datos Antiguos ",
               collapsible = FALSE,
               status = "navy",
               solidHeader = TRUE,
               
               HTML("Utilice las siguientes herramientas para actualizar el contenido de la página de Antecedentes y Recursos. 
                              Necesitarás un editor HTML como [blank] para editar los archivos. <br>"),
               
               HTML(paste("<b>Última actualización:</b>", file.info("Output Data/background.html")$ctime), "<br><br>"),
               
               HTML("<b>1. Descargue una copia del archivo HTML de Antecedentes y Recursos</b><br>"),
               
               downloadButton(
                 "downloadBackgroundButton",
                 "Descargar plantilla de Antecedentes"),
               
               HTML("<br><br><b>2. Cargue el archivo HTML editado para sustituir el actual..</b><br>"),
               
               fileInput("backgroundFileInput", 
                         label = "",
                         multiple = FALSE,
                         accept = ".html"),
               
               HTML("<b>3. Confirme los cambios. Esto cerrará la aplicación, refrescará la página a través de su navegador para ver sus cambios.</b><br>"),
               
               actionButton(
                 "saveNewBackgroundButton",
                 "Confirmar Cambios"),
               
               HTML("<br><br>"),
               
               box(
                 width = 8,
                 title = "¿Algo se rompió?",
                 status = "warning",
                 solidHeader = TRUE,
                 
                 HTML("<b>Restablecer la página Antecedentes y Recursos al archivo HTML predeterminado.</b><br>"),
                 
                 actionButton("revertBackgroundPopupButton",
                              "Revertir a copia de Antecedentes")
                 
               )
           )
           
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$user <- renderUser({
    dashboardUser(
      name = "",
      image = "developer.jpg",
      title = HTML("Hola, soy Eliza Butterfield! Programé esta aplicación."),
      subtitle = HTML("Defensora de la Justicia Ambiental, Desarrolladora de Aplicaciones, Especialista en Salud Publica"),
      #footer = p("The footer", class = "text-center"),
      fluidRow(
        actionButton("devWebsite", "Mi sitio web",
                     onclick ="window.open('https://elizabutterfield.com/', '_blank')"),
        actionButton("devWebsite", "LinkedIn",
                     onclick ="window.open('https://www.linkedin.com/in/elijahbutterfield/', '_blank')")
      )
      
    )
  })
  
  

    
  
  observeEvent(input$revertBackgroundButton, {
    
    file.copy(from = "Backup Data/background_Backup.html",
              to = "Output Data/background.html",
              overwrite = TRUE)
    stopApp(returnValue = invisible())
    
  })
  
  output$downloadBackgroundButton <- downloadHandler(
    filename = function() {
      paste("background_Backup", ".html", sep="")
    },
    content = function(file) {
      file.copy("Backup Data/background_Backup.html", file)
    }
  )
  
  
  
  observeEvent(input$saveNewBackgroundButton, {
    print("yay")
    file <- input$backgroundFileInput
    ext <- tools::file_ext(file$datapath)

    req(file)

    validate(need(ext == "html", "Por favor, cargue un archivo HTML"))
    file.copy(from = file$datapath,
              to = "Output Data/background.html",
              overwrite = TRUE)

    stopApp(returnValue = invisible())
    
  })
  

  
  observeEvent(input$revertBackgroundPopupButton, {
    
    revertBackgroundModal <- modalDialog(
      title = "Uh oh, ¿algo está roto?",
      HTML("Está a punto de sobrescribir los datos almacenados con una copia de seguridad. Esto cerrará la aplicación. Utilice el botón de recarga/actualización de su navegador para reiniciar la aplicación."),
      easyClose = TRUE,
      size = "m",
      footer = tagList(
        actionButton("closeModal", "Cerrar"),
        actionButton("revertBackgroundButton", "Revertir a copia de Antecedentes"))
    )
    
    showModal(revertBackgroundModal)  
    
  })
 

output$myBackground <- renderUI({
  

  includeHTML("Output Data/background.html")

  
})

  
  
  
  # observeEvent(input$modalStartTut,{
  #   introjs(session,options = list(steps=steps()))
  #   
  # })
  # 


  
  introModal <- modalDialog(
    id = "introModal",
    title = "¡Bienvenido!",
    HTML("<div class='polaroid'; style = 'text-align: center;'>",
      '<iframe width="100%" height="315" src="https://www.youtube.com/embed/3RlTxV3hhPM" title="Community-Led Air Monitoring for Hexavalent Chromium in Paramount, CA " frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>',
      "</div>",
      "<br>",
      "<p>En esta aplicación, puede ver los datos del monitor de aire recopilados en Paramount, CA.</p><b>¡Haga clic en el botón Iniciar Tutorial a continuación para obtener un breve recorrido sobre cómo usar la aplicación.</b>",
    ),
    easyClose = TRUE,
    size = "l",
    footer = tagList(
      actionButton("closeModal", "Cerrar"),
      actionButton("modalStartTut", "Iniciar Tutorial"))
  )
  
  showModal(introModal)  
  
  observeEvent(input$modalStartTut,
               
               #TRUE only happens when the Okay button is pushed
               introjs(session)
  )
  
  observeEvent(input$modalStartTut,
               removeModal()
  )
  observeEvent(input$closeModal,
               removeModal()
  )
  
  
  
  # initiate hints on startup with custom button and event
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  # start introjs when button is pressed with custom options and events
  
  
  
  
  observeEvent(input$help,
               
               introjs(session)
  )
  
  
  
  output$dataDownload <- downloadHandler(
    filename = function() {
      paste("ParamountCA_CommunityAirMonitorData_",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    
    content = function(file) {
      write.csv(airDataFull %>% select(-`popUp`, -`calculated`), file)
    },
    contentType = "text/csv"
  )
  
  
  
  
  
  observeEvent(input$devPasswordBtn, {
    if (input$devPassInput == "ceh4health") {
      print("correct password")
      updateTextInput(session, "devPassInput",
                      label = HTML("¿Cuál es tu contraseña?"))
      showModal(settingsModal)  
    } else {
      updateTextInput(session, "devPassInput",
                      label = HTML("¿Cuál es tu contraseña?", " Contraseña incorrecta, inténtelo de nuevo."))
      print("wrong password")
    }
    
    
    #updatebs4TabItems(session, inputId = "sidebarID", selected = "dashboard")
  })
    
  
  observeEvent(input$dataCheckButton, {
    print("dataCheckButton")
    
    dataCheckModal <- modalDialog(
      title = "Fetch New Data",
      HTML("Está a punto de sobrescribir los datos almacenados con una nueva versión de Hojas de Cálculo de Google. Esto cerrará la aplicación. Utilice el botón de recarga/actualización de su navegador para reiniciar la aplicación."),
      easyClose = TRUE,
      size = "m",
      footer = tagList(
        actionButton("closeModal", "Cerrar"),
        actionButton("confirmDataCheckButton", "Recuperar Nuevos Datos"))
    )
    
    showModal(dataCheckModal)  

  })
  
  observeEvent(input$confirmDataCheckButton, {
    
    loadingDataModal <- modalDialog(
      title = "Recuperando Nuevos Datos, Espere",
      HTML("Esto puede tardar un minuto, tenga paciencia. Vuelva a crear la página cuando haya terminado."),
      easyClose = F,
      size = "m"
    )
    showModal(loadingDataModal)  
    
    
      source("dataImportHandler.R")
      stopApp(returnValue = invisible())
    
    
  })
  
  observeEvent(input$revertDataButton, {

    revertDataModal <- modalDialog(
      title = "Uh oh, ¿algo está roto?",
      HTML("Está a punto de sobrescribir los datos almacenados con una copia de seguridad. Esto cerrará la aplicación. Utilice el botón de recarga/actualización de su navegador para reiniciar la aplicación."),
      easyClose = TRUE,
      size = "m",
      footer = tagList(
        actionButton("closeModal", "Cerrar"),
        actionButton("confirmRevertDataButton", "Revertir a Datos Antiguos"))
    )
    
    showModal(revertDataModal)  
    
  })
  
  observeEvent(input$confirmRevertDataButton, {
      file.copy("Backup Data/fullData_Backup.csv", "Output Data/fullData.csv", overwrite = TRUE)
      stopApp(returnValue = invisible())
    
  })
  
  observeEvent(input$dataAboutButton, {
    

    dataInfo <- modalDialog(
      title = "About This Data",
      HTML("Información sobre muestras no válidas, períodos de muestreo perdidos, contexto alrededor de las variables que influyen en las concentraciones de cromo hexavalente, como la velocidad y dirección del viento"),
      easyClose = TRUE,
      size = "m"
    )
    
    showModal(dataInfo)  
  })
  
  
  #This is the text with the arrow that lives on top of the graph 
  epaThresholdTitle <- list(
    xref = 'paper',
    yref = 'y',
    x = 0.03,
    y = 0.05,
    xanchor = 'middle',
    yanchor = 'middle',
    text = HTML("<b > Límite de Umbral /nde Riesgo EPA</b>"),
    font = list(
      family = 'Arial',
      size = 14,
      color = '#dc3545'
    ),
    showarrow = T,
    ax = -25,
    ay = -60,
    arrowcolor = '#dc3545'
  )
  
  
  
  #This observeEvent updates whenever the main data date selection slider is changed
  #It contains variables that need to be global so they can be accessed by the 
  #graph, boxes, etc. Otherwise, the code would need to be repeated in each graph and box function.
  #This saves space and is overall cleaner.
  observeEvent(input$graphDateSelection,{
    
    #Get the start and end dates from the main graph input slider.
    #the "<<-" is what makes them global variables
    startDate <<- input$graphDateSelection[1]
    endDate <<- input$graphDateSelection[2]
    
    #Filter the data by the selected dates
    filteredByDate <<- airDataFull %>%
      filter(`Sample Date` >= input$graphDateSelection[1]) %>%
      filter(`Sample Date` <= input$graphDateSelection[2]) 
    
    #need to purge epa line ends dates on each refresh
    
    epaLineData <<- tibble("Sample Date" = c(min(filteredByDate$`Sample Date`), max(filteredByDate$`Sample Date`)), 
                           variable = "EPA Risk Threshold", 
                           value = 0.01)
    
    
    #calculate the average air monitor reading during the dates selected
    averageByDate <<- filteredByDate %>%
      group_by(variable) %>%
      summarise(value = round(mean(value), 2))
    
    
  })
  
  
  
  
  
  
  
  output$primaryAirMonitorGraph <- renderPlotly({
    
    
    
    xaxis <- list(
      # title = list(
      #     text = paste("Date Range: ", startDateText, " to ", endDateText),
      #     standoff = 20,
      #     font = font1
      # ),
      title = NA,
      
      showgrid = FALSE,
      linecolor = 'rgb(204, 204, 204)',
      linewidth = 1,
      ticks = 'outside',
      tickcolor = 'rgb(204, 204, 204)',
      tickwidth = 2,
      ticklen = 7,
      tickfont = tickFont,
      fixedrange = TRUE
    )
    
    
    yaxis <-list(
      title = list(text = "Concentración de Cromo Hexavalente (ng/m3)",
                   font = axisTitleFont),
      zeroline = FALSE,
      showline = FALSE,
      tickfont = tickFont,
      gridcolor = "rgb(100, 100, 100)",
      #set custom tick values
      tickvals = c( 0.01, seq(
        from = 0.5, to = 10, by = 0.25
      )),
      tickmode = "array",
      fixedrange = TRUE
      #  ticksuffix = " ng/m3  "
    )
    
    
    
    primaryGraphMargin <- list(
      autoexpand = FALSE,
      l = 75,
      r = 5,
      t = 15,
      b = 45
    )
    
    
    
    label <- list(
      # bgcolor = "#232F34",
      bordercolor = "black",
      font = font1
    )
    
    
    
    epaData <- filteredByDate %>%
      filter(`variable` == unique(airDataFull["variable"])[[1]][3])
    
    vermontData <- filteredByDate %>%
      filter(`variable` == unique(airDataFull["variable"])[[1]][1])
    
    somersetData <- filteredByDate %>%
      filter(`variable` == unique(airDataFull["variable"])[[1]][2])
    
    reactiveDifferenceOfDaysFull <-
      input$graphDateSelection[2] - input$graphDateSelection[1]
    
    
    primaryAirMonitorGraph <- plot_ly(
      
      #This function is where the Vermont data trace is created
      #The trace for the somerset data is created in the following add_trace
      data = epaLineData,
      
      #The following variables are used to visually differentiate this trace 
      #from the somerset trace
      line = list(shape = "spline", dash = "solid", width = 4),
      colors = "red",
      
      #The following variables will apply to both traces 
      x = ~ `Sample Date`,
      y = ~ `value`,
      color = ~ `variable`,
      connectgaps = T,
      hoverinfo = "none",

      type = 'scatter',
      mode = 'lines+markers',
      symbol = ~ `variable`,
      source = "hoverplotsource"
      
      
    )%>%
      config(displaylogo = FALSE,
             modeBarButtons = list(list("toImage"))) %>% 
      #here's the trace for the vermont data, with it's own visual customizations.
      add_trace(
        data = vermontData,
        text = ~ (`popUp`),
        hoverlabel = label,
        hovertemplate = "%{text}<extra></extra>",
        marker = list(size = 12, 
                      symbol = "x",
                      color = vermontColor,
                      line = list(color = "rgb(8,48,107)",
                                  width = 1.5)),
        line = list(
          shape = "spline",
          dash = "dashdot",
          width = 3,
          color = vermontColor
        )
      ) %>% 
      #here's the trace for the somerset data, with it's own visual customizations.
      add_trace(
        data = somersetData,
        text = ~ (`popUp`),
        hoverlabel = label,
        hovertemplate = "%{text}<extra></extra>",
        marker = list(size = 12, 
                      symbol = "hexagon",
                      color = somersetColor,
                      line = list(color = "rgb(8,48,107)",
                                  width = 1.5)),
        line = list(
          shape = "spline",
          dash = "dashdot",
          width = 3,
          color = somersetColor
        )
      ) %>% 
      #These are all the visual layout components are loaded in.
      layout(
        title = list(
          text = HTML("Datos de Monitoreo del Aire Comunitario de Paramount"),
          font = graphTitleFont,
          x = 0.03,
          y = 0.98),
        # annotations = epaThresholdTitle,
        
        xaxis = xaxis,
        yaxis = yaxis,
        margin = primaryGraphMargin,
        autosize = TRUE,
        showlegend = F,
        height = 605
        #change the x and y to move the legend around
        #legend = list(x = -0.03, y = 1.17, font = legendFont)
      ) %>%
      event_register('plotly_hover') %>% #lets me use if statements
      htmlwidgets::onRender("
function(unhoverFunc, x) {

// when hovering over an element, do something
unhoverFunc.on('plotly_unhover', function(d) {



// show image and annotation
Plotly.relayout(unhoverFunc.id, {
images: [],
//title: ''
});


})
}
")
      

    
    
    
    
    
    
    # %>%
    #   #Add in the red threshold line without needing it to be included in the dateframe
    #   add_segments(
    #     #Add padding at the left side of the graph to make room for 
    #     #both the arrow/label visual and the legend
    #     x = input$graphDateSelection[1] - reactiveDifferenceOfDaysFull * 0.12, #0.18
    #     #Add padding to the left side of the graph
    #     xend = input$graphDateSelection[2] + reactiveDifferenceOfDaysFull * 0.0,
    #     y = 0.01,
    #     yend = 0.01,
    #     name = "EPA Risk Threshold Limit (0.01 ng/m3)",
    #     line = list(dash = "dash", 
    #                 color = 'red',
    #                 width = 4.5),
    #     inherit = FALSE,
    #     hovertemplate = NA
    #   ) 
    # add_text(text = ~value, textposition = "top right")
    
  })
  
  #This is for the caution sign in the background of the graph when you hover over a data point above 0.01
  #See https://stackoverflow.com/questions/61496833/is-there-a-way-to-read-images-stored-locally-into-plotly
  hover_event <- reactive({
    event_data(event = "plotly_hover", source = "hoverplotsource")
  })
  
  unhover_event <- reactive({
    event_data(event = "plotly_unhover", source = "hoverplotsource")
  })
  
  
  hoverplotlyProxy <- plotlyProxy("primaryAirMonitorGraph", session)
  
  observeEvent(unhover_event(), {
    hoverplotlyProxy %>%
      plotlyProxyInvoke("relayout", list(images = list(NULL)))
  })
  
  observeEvent(hover_event(), {
    if(hover_event()$y > 0.01){
      print("above 0.01!")
    
    hoverplotlyProxy %>%
      plotlyProxyInvoke("relayout", list(images = list(
        list(
          source = 'caution.png',
          xref = "paper",
          yref = "paper",
          x = 0.77,
          y = 0.9,
          sizex = 0.2,
          sizey = 0.2,
          opacity = 0.4
        )
      )))
    }
  })
  
  output$somersetText <- renderText({
    
    myTitle <- "Somerset Blvd and Colorado Ave"
    
    paste("<b>Monitor de Aire:</b>",
          "<p'>", myTitle, "<br>")
    # paste("<h4>", myTitle)
    
  })
  
  HTML("<button type='button' class='btn btn-secondary' data-toggle='tooltip' data-placement='top' title='Tooltip on top'>
  Tooltip on top
</button>
")
  
  output$somersetValueBox <- renderValueBox({
    startDateText <- format(input$graphDateSelection[1], format = "%B %d, %Y")
    endDateText <- format(input$graphDateSelection[2], format = "%B %d, %Y")
    
    valueBox(
      HTML(
        '
              <p style="margin: 0px">
                  <b style="font-size: 1.5rem; line-height: 0.2;" >', "Niveles Medios de<br> Cromo Hexavalente",'
                  </b>
                </p>
              <hr style = "border-top: 0.10rem solid ; border-radius: 5px; margin: 0px; margin-top: 9px; opacity: 0.3; width: 50%; color: black;">

        <table style = "margin-top: 5px">
            <tr>
              <td>
                <p style = "margin-bottom: 0px">
                <b>
                  <i data-html="true"; style="font-size: 22px; padding-top:10px"; class="fa fa-info-circle"; id="somersetIcon"; data-toggle="tooltip"; data-placement="left"; 
                  title="', tooltipText[[1]][1], '"></i>
                  </b>
                </p>
              </td>
              <td style="padding-left:10px";> 
                <h2  style = "margin-bottom: 0px">
                  <b>', paste0(averageByDate[[2]][1] , " ng/m3"),'
                  </b>
                </h2>
              </td>
            </tr>
          </table>'
      ),        subtitle = htmlOutput("somersetText"), 
      icon = icon("industry"),
      color = "purple",
      width = 12
    )
  })
  
  output$vermontText <- renderText({
    
    
    myTitle <- "Vermont Ave. and 70th St."
    paste("<b>Monitor de Aire:</b>",
          "<p'>", myTitle, "<br>")
    # paste("<h4>", myTitle)
    
  })
  
  output$vermontValueBox <- renderValueBox({
    startDateText <- format(input$graphDateSelection[1], format = "%B %d, %Y")
    endDateText <- format(input$graphDateSelection[2], format = "%B %d, %Y")
    
    valueBox(
      
      HTML(
        '
              <p style="margin: 0px">
                  <b style="font-size: 1.5rem; line-height: 0.2;" >', "Niveles Medios de <br> Cromo Hexavalente",'
                  </b>
                </p>
                
              <hr style = "border-top: 0.10rem solid ; border-radius: 5px; margin: 0px; margin-top: 9px; opacity: 0.3; width: 50%; color: black;">

      <table style = "margin-top: 5px">
            <tr>
              <td>
                <p style = "margin-bottom: 0px">
                <b>
                  <i data-html="true"; style="font-size: 22px; padding-top:10px"; class="fa fa-info-circle"; data-toggle="tooltip"; data-placement="left"; 
                  title="', tooltipText[[1]][1], '"></i>
                  </b>
                </p>
              </td>
              <td style="padding-left:10px";> 
                <h2>
                  <b>', paste0(averageByDate[[2]][2] , " ng/m3"),'
                  </b>
                </h2>
              </td>
            </tr>
          </table>'
      ),
      subtitle = htmlOutput("vermontText"), 
      icon = icon("industry"),
      color = "maroon",
      width = 12
    )
  })
  
  

  
}

# Run the application 
#run_with_themer
(shinyApp(ui = ui, server = server))

