#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(bsplus)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    fluidRow(
      column(12,
             # App title ----
             titlePanel( 
               img(src='tarificacion.png', align = "left", width="40%")
             )
      )
    ), 
    
    #Condicional Login
    conditionalPanel(condition = 'output.authUser', 
                     fluidRow( 
                       column(12, 
                              
                              sidebarLayout(
                                
                                column( 12,
                                        fluidRow(   
                                          column(width = 2,
                                                 # Sidebar panel for inputs ----
                                                 sidebarPanel(width=12,
                                                              
                                                              # Input: Checkbox if file has header ----
                                                              #checkboxInput("header", "Cabecera", TRUE),
                                                              
                                                              # Input: Select separator ----
                                                              # radioButtons("sep", "Separador",
                                                              #              choices = c(Coma = ",",
                                                              #                          Punto_y_Coma = ";",
                                                              #                          Tabulador = "\t"),
                                                              #              selected = ","),
                                                              
                                                              # Horizontal line ----
                                                              #tags$hr(),
                                                              
                                                              # Input: Select number of rows to display ----
                                                              # radioButtons("disp", "Mostrar",
                                                              #              choices = c(Cabecera = "head",
                                                              #                          Todo = "all"),
                                                              #              selected = "head"),
                                                              
                                                              # Horizontal line ----
                                                              #tags$hr(),
                                                              
                                                              # Input: Select a file ----
                                                              fileInput("file1", "Selecciona el fichero de la promoción [CSV File]",
                                                                        multiple = FALSE,
                                                                        accept = c("text/csv",
                                                                                   "text/comma-separated-values,text/plain",
                                                                                   ".csv")),
                                                              
                                                              # Horizontal line ----
                                                              tags$hr(),
                                                              
                                                              actionButton("calculate", "Calcular Tarifa", width = "100%", class = "btn-primary")
                                                              
                                                              
                                                 ), #sidebarPanel
                                                 #Sidebar para datos de Precio de la promoción
                                                 sidebarPanel(width=12,
                                                              
                                                              # Input: Select a file ----
                                                              fileInput("file2", "Selecciona el fichero con la tarifa API [CSV File]",
                                                                        multiple = FALSE,
                                                                        accept = c("text/csv",
                                                                                   "text/comma-separated-values,text/plain",
                                                                                   ".csv")),
                                                              
                                                              # Horizontal line ----
                                                              tags$hr(),
                                                              
                                                              actionButton("compareTo", "Comparar", width = "100%", class = "btn-primary")
                                                              
                                                 ) #sidebarPanel
                                          ), #col-2
                                          
                                          column(4,
                                                 fluidRow(
                                                   sidebarPanel(width= 12, 
                                                                radioButtons("tipoPromocion", label = "Tipología de promoción",
                                                                             choices = list("Unifamiliar" = 2, "Plurifamiliar" = 1, "Torre" = 3, "Personalizar" = 4), 
                                                                             selected = 4, inline = TRUE),
                                                                conditionalPanel(
                                                                  condition = "input.tipoPromocion == 1",
                                                                  radioButtons("tipoBajoJardin", label = "Bajos y Jardín",
                                                                               choices = list("Sin Bajos" = 1, "Con Bajos - Con Jardín" = 2, "Con Bajos - Sin Jardín" = 3), 
                                                                               selected = 1, inline = T)
                                                                ) 
                                                   ) #sidebarPanel
                                                 ), #fluidRow
                                                 
                                                 fluidRow(
                                                   column(12,
                                                          conditionalPanel(
                                                            condition = "input.tipoPromocion == 4",
                                                            fluidRow(
                                                              sidebarPanel(width=12, #Sidebar para datos de importancia de variables
                                                                           
                                                                           fluidRow(
                                                                             
                                                                             column(6,                
                                                                                    conditionalPanel(
                                                                                      condition = "input.gapH == false",
                                                                                      sliderInput("height",
                                                                                                  "Repercusión €/m2 en altura:",
                                                                                                  min = 1,  max = 10, value = 5
                                                                                      )
                                                                                    ),
                                                                                    
                                                                                    conditionalPanel(
                                                                                      condition = "input.gapH == true",
                                                                                      textInput('heightGapPrice',
                                                                                                label = 'Diferencia de precio por altura (€/m2):',
                                                                                                value = ""
                                                                                      )
                                                                                    ),
                                                                                    checkboxInput("gapH", "Valor absoluto", FALSE)
                                                                             ),
                                                                             column(6,
                                                                                    # Horizontal line ----
                                                                                    #tags$hr(),
                                                                                    
                                                                                    
                                                                                    conditionalPanel(
                                                                                      condition = "input.gapT == false",
                                                                                      sliderInput("tipology",
                                                                                                  "Repercusión €/m2 en tipología:",
                                                                                                  min = 1,  max = 10, value = 5
                                                                                      )
                                                                                    ),
                                                                                    
                                                                                    conditionalPanel(
                                                                                      condition = "input.gapT == true",
                                                                                      textInput('tipologyGapPrice',
                                                                                                label = 'Diferencia de precio por tipología (€/m2):',
                                                                                                value = ""
                                                                                      )
                                                                                    ),
                                                                                    checkboxInput("gapT", "Valor absoluto", FALSE)
                                                                             )
                                                                           ) #fluidRow
                                                              ) #sidebarPanel
                                                            ) #fluidRow
                                                          ) #conditionalPanel
                                                          
                                                   ) #column
                                                   
                                                   
                                                 ), #fluidRow
                                                 fluidRow(
                                                   sidebarPanel(width=12,
                                                                fluidRow(
                                                                  column(6,             
                                                                         sliderInput("terrace",
                                                                                     "Cómputo superficie de terraza (%)",
                                                                                     min = 10,  max = 100, value = 50
                                                                         )
                                                                  ),
                                                                  column(6,
                                                                         sliderInput("garden",
                                                                                     "Cómputo superficie de jardín (%)",
                                                                                     min = 10,  max = 100, value = 50
                                                                         )
                                                                  )
                                                                ) #fluidRow
                                                   ) #sidebarPanel
                                                 ) #fluidRow
                                                 
                                                 
                                          ), #column
                                          column(4,
                                                 
                                                 fluidRow(
                                                   sidebarPanel(width=6,
                                                                fluidRow(
                                                                  column(12,
                                                                         tags$b("Orientación Salón")
                                                                  )
                                                                ),
                                                                fluidRow( 
                                                                  column(4,
                                                                         selectInput("sNO", "NO", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("sN", "N", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("sNE", "NE", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         selectInput("sO", "O", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  ),
                                                                  column(4,
                                                                         img(src='rosa_vientos.png', align = "right", width="130%") # height = 150, width = 150
                                                                  ),
                                                                  column(4,
                                                                         selectInput("sE", "E", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )        
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         selectInput("sSO", "SO", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("sS", "S", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("sSE", "SE", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  )                               
                                                                ),
                                                                fluidRow(
                                                                  sliderInput("salonOrientation",
                                                                              "Importancia",
                                                                              min = 0,  max = 4, value = 0
                                                                  )                                  
                                                                )
                                                   ),
                                                   sidebarPanel(width=6,
                                                                fluidRow(
                                                                  column(12,
                                                                         tags$b("Orientación Dormitorio")
                                                                  )
                                                                ),
                                                                fluidRow( 
                                                                  column(4,
                                                                         selectInput("dNO", "NO", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("dN", "N", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("dNE", "NE", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         selectInput("dO", "O", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  ),
                                                                  column(4,
                                                                         img(src='rosa_vientos.png', align = "right", width="130%") # height = 150, width = 150
                                                                  ),
                                                                  column(4,
                                                                         selectInput("dE", "E", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )        
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         selectInput("dSO", "SO", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("dS", "S", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         )
                                                                  ),
                                                                  column(4,
                                                                         selectInput("dSE", "SE", width = "90px", selected = "R",
                                                                                     c("B" = "B",
                                                                                       "R" = "R",
                                                                                       "M" = "M")
                                                                         ) 
                                                                  )                               
                                                                ),
                                                                fluidRow(
                                                                  sliderInput("dormOrientation",
                                                                              "Importancia",
                                                                              min = 0,  max = 4, value = 0
                                                                  )                                  
                                                                )
                                                   ) #sidebarPanel
                                                 ) #fluidRow
                                                 
                                          ), #column-4
                                          column(2,
                                                 sidebarPanel(width=12,
                                                              # fluidRow(
                                                              #   column(12,             
                                                              textInput('totalPrice',
                                                                        label = 'Precio total promoción',
                                                                        value = ""
                                                              ),
                                                              
                                                              textInput('comercialPricem2',
                                                                        label = 'Precio m2 comercial',
                                                                        value = ""
                                                              ),
                                                              
                                                              
                                                              textInput('garagePrice',
                                                                        label = 'Precio garaje adicional',
                                                                        value = ""
                                                              ),
                                                              textInput('garageNumber',
                                                                        label = 'Nº. garajes adicionales',
                                                                        value = ""
                                                                        
                                                              ),
                                                              
                                                              
                                                              # Horizontal line ----
                                                              tags$hr()
                                                              #   ) #column
                                                              # ) #fluidRow
                                                 ) #sidebarPanel
                                          ) #column-2
                                          
                                        ), #fluidRow
                                        fluidRow(
                                          conditionalPanel(
                                            condition = "output.showSummary",
                                            column(12,
                                                   sidebarPanel(width=12,
                                                                fluidRow(
                                                                  DT::dataTableOutput("summary")
                                                                ),
                                                                fluidRow(
                                                                  tags$hr(),
                                                                  column(6,
                                                                         plotOutput("summaryM2Plot") 
                                                                  ),
                                                                  column(6,
                                                                         plotOutput("summaryMeanPlot")  
                                                                  )
                                                                )
                                                   )
                                                   
                                            )
                                          )
                                        )
                                ), # column-12
                                sidebarPanel(width=12,
                                             fluidRow(
                                               # Main panel for displaying outputs ----
                                               mainPanel(
                                                 
                                                 # conditionalPanel(
                                                 #  condition = 'output.showContents',
                                                 
                                                 
                                                 # Output: Data file ----
                                                 #tableOutput("contents"),
                                                 # fluidRow(
                                                 #   column(1,
                                                 #          actionButton("viewPDF", "Ver planos", width = "100px", class = "btn btn-primary")
                                                 #   )
                                                 # ),
                                                 conditionalPanel(
                                                   condition = "output.showPDF",
                                                   #absolutePanel('pdfviewer')
                                                   htmlOutput('pdfviewer')
                                                 ),
                                                 
                                                 DT::dataTableOutput("contents"),
                                                 DT::dataTableOutput("contentsLocales")
                                                 #,
                                                 
                                                 # bsCollapse(id = "collapsePanel",
                                                 #            bsCollapsePanel("Viviendas", style = "info",
                                                 #                            tableOutput("contents")
                                                 #                            )
                                                 #            ),
                                                 
                                                 # absolutePanel(
                                                 #   id = "controls",
                                                 #   class = "panel panel-default",
                                                 #   fixed = TRUE,
                                                 #   draggable = TRUE,
                                                 #   top = 130,
                                                 #   left = 1230,
                                                 #   right = "auto",
                                                 #   bottom = "auto",
                                                 #   width = 600,
                                                 #   height = "auto",
                                                 #   HTML(
                                                 #     '<button data-toggle="collapse" data-target="#demo">Ver Mapa</button>'
                                                 #   )
                                                 #,
                                                 
                                                 #tags$iframe(style="height:100%; width:100%", src="plano_solagua.pdf")
                                                 # tags$div(
                                                 #   id = 'demo',
                                                 #   class = "collapse",
                                                 #   leafletOutput("mymap")
                                                 # )
                                                 
                                                 #) #absolutePanel
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 # ) #conditionalPanel
                                                 
                                                 
                                               ) #mainPanel
                                               
                                             ) # fluidRow
                                ) #sidebarPanel
                                
                              ) #sidebarLayout
                              
                       ) # column
                       
                     ), # fluidRow    
                     conditionalPanel(condition = 'output.showValidateButton',
                                      fluidRow(
                                        column(7),
                                        column(1,
                                               actionButton("validateAll", "Validar todo", width = "100%", class = "btn btn-primary")
                                        ),
                                        column(1,
                                               actionButton("saveAll", "Guardar", width = "100%", class = "btn btn-success")
                                        ),
                                        column(1,
                                               conditionalPanel(condition = 'output.showDownloadButton',
                                                                #actionButton("downloadTar", "Descargar", width = "100%", class = "btn btn-info"),  
                                                                downloadButton("downloadTar", "Descargar", width = "100%", class = "btn btn-info")
                                               )
                                        ),
                                        column(1,
                                               conditionalPanel(condition = 'output.showDownloadButton',
                                                                downloadButton("downloadTube", "Tubo",  width = "100%", class = "btn btn-info")
                                               )
                                        ), 
                                        column(1,
                                               conditionalPanel(condition = 'output.showDownloadButton',
                                                                actionButton("sendTarifa", "Enviar",  width = "100%", class = "btn btn-danger", icon = icon("envelope") )
                                               )
                                        )
                                        
                                        
                                      )# fluidRow
                     )  #conditionalPanel
                     
                     
    ), # Condicional Login
    
    conditionalPanel(condition = '!output.authUser', 
                     fluidRow(
                       column(5),
                       column(2,
                              sidebarPanel(width=12,
                                           textInput('userLogin',
                                                     label = 'Usuario',
                                                     value = ""
                                           ),
                                           passwordInput('passwordLogin',
                                                         label = 'Contraseña',
                                                         value = ""
                                           ),
                                           conditionalPanel(condition = 'output.loginError',
                                                            div(
                                                              tags$b("Error de credenciales!!", style = "color: red;"),
                                                              tags$hr()
                                                            )
                                           ),
                                           #actionButton("cancellogin", "Cancel", class = "btn-danger"),
                                           actionButton("oklogin", "Login", class = "btn-primary") 
                                           
                              )
                       ),
                       column(5)
                       
                     )
    )
  ) # fluidPage
  
)
