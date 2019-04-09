#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(rgdal)
# if (!require(geojsonio)) {
#   install.packages("geojsonio")
#   library(geojsonio)
# }
library(dplyr)
library(DT)
library(jsonlite)
library(sp)
library(maps)
library(ggmap)
library(maptools)
library(leaflet)
library(leaflet.extras)
library(digest)
library(openxlsx)
library(stringr)
library(scales)

library(mailR)

source("RPostgresConnection.R")

#Necesario para permitir subir ficheros de más de 5MB. Los ficheros de superficies son bastante pesados 
options(shiny.maxRequestSize=30*1024^2)

calculateCounter <- reactiveValues(count=0)
compareCounter <- reactiveValues(count=0)
validateAllCounter <- reactiveValues(count=0)
validateCounter <- reactiveValues(count=0)
saveCounter <- reactiveValues(count=0)


dataModal <- function(failed = FALSE, tagText = "Debes validar la tarifa de todas las viviendas") {
  modalDialog(
    div(tags$b(tagText, style = "color: red;")),
    footer = tagList(
      actionButton("okmodal", "Entendido", class = "btn-primary")
    )
  )
}


dataModalTarifaLow <- function(value, failed = FALSE) {
  modalDialog(
    div(tags$b(paste("La suma de la tarifa esta ",value," € por debajo de lo debido", sep=""), style = "color: red;")),
    footer = tagList(
      actionButton("okmodal", "Entendido", class = "btn-primary")
    )
  )
}


dataModalOk <- function(failed = FALSE, tagText = "Enhorabuena. Tus tarifas validadas han sido guardadas.") {
  modalDialog(
    div(tags$b(tagText, style = "color: black;")),
    footer = tagList(
      actionButton("okmodal", "Yuhuuu!!", class = "btn-primary")
    )
  )
}

dataModalSendOk <- function(failed = FALSE, tagText = "El tubo de tarifas ha sido enviado a altasistemas@neinorhomes.com") {
  modalDialog(
    div(tags$b(tagText, style = "color: black;")),
    footer = tagList(
      actionButton("okmodal", "Perfecto", class = "btn-primary")
    )
  )
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Store session client data in a convenience variable
  u_data <- session$clientData
  url <- isolate(u_data[["url_hostname"]])
  
  # This code will be run once per user
  users_data <- data.frame(START = Sys.time(), url = url )
  users_data$user <- NA
  users_data$id_user <- NA
  users_data$loginTime <- as.Date(NA)
  users_data$id_sesion <- session$token
  
  
  onStop(
    function() {
      users_data$END <- Sys.time()
      users_data$calculateClicks <- isolate(calculateCounter$count) 
      users_data$compareClicks <- isolate(compareCounter$count) 
      users_data$validateAllClicks <- isolate(validateAllCounter$count) 
      users_data$validateIndividualClicks <- isolate(validateCounter$count) 
      users_data$saveClicks <- isolate(saveCounter$count) 
      # Write a file in your server directory
      
      #users_data$id_sesion <- session$token
      
      #Update DataBase with user connection data
      #users_data_to_save <- users_data[ ,c("user","url", "START","loginTime","END", "calculateClicks", "compareClicks", "validateAllClicks", "validateIndividualClicks", "saveClicks")] %>% rename (usuario = user, connectionTime = START, disconnectionTime = END )
      #write_df_as_table(users_data_to_save, "usuario_traza_tarificacion", T)
      
      #sqlserver
      users_data_to_save <- users_data[ ,c("id_sesion", "START","loginTime","END", "calculateClicks", "compareClicks", "validateAllClicks", "validateIndividualClicks", "saveClicks", "id_user")] %>% rename (login_time = loginTime, connection_time = START, disconnection_time = END, calculate_clicks = calculateClicks, compare_clicks = compareClicks, validate_all_clicks = validateAllClicks, validate_individual_clicks = validateIndividualClicks, save_clicks = saveClicks )
      write_df_as_sqlserver_table(users_data_to_save, "sesion_tarificacion", T)
    }
  )

   
  values <- reactiveValues(
    namePromo = NULL,
    proyecto = NULL,
    promocion = NULL,
    promDf = NULL,
    localesDf = NULL,
    promDfWithTarifa = NULL,
    summaryDF = NULL,
    authenticated = FALSE,
    showsummary = FALSE,
    showpdf = FALSE,
    showcontents = FALSE,
    logError = FALSE,
    finalTable = NULL,
    validateButton = FALSE,
    downloadButton = FALSE,
    prices_m2 = NULL,
    totalViviendas = NULL
  )
  
  output$authUser <- reactive({
    values$authenticated
  })
  outputOptions(output, "authUser", suspendWhenHidden = FALSE)
  
  output$showSummary <- reactive({
    values$showsummary
  })
  outputOptions(output, "showSummary", suspendWhenHidden = FALSE)
  
  output$showPDF <- reactive({
    values$showpdf
  })
  outputOptions(output, "showPDF", suspendWhenHidden = FALSE)
  
  output$showContents <- reactive({
    values$showcontents
  })
  outputOptions(output, "showContents", suspendWhenHidden = FALSE)
  
  output$loginError <- reactive({
    values$logError
  })
  outputOptions(output, "loginError", suspendWhenHidden = FALSE)
  
  output$showValidateButton <- reactive({
    values$validateButton
  })
  outputOptions(output, "showValidateButton", suspendWhenHidden = FALSE)
  
  output$showDownloadButton <- reactive({
    values$downloadButton
  })
  outputOptions(output, "showDownloadButton", suspendWhenHidden = FALSE)
  
  observeEvent( input$oklogin ,{
    isolate({
      Username <- input$userLogin
      Password <- input$passwordLogin
    })
    
    #cred <- read_table("login_tarificacion")
    cred <- read_sqlserver_table("login_tarificacion")
    user <- cred[cred$username == Username,]
    
    Id.username <- which(user$username == Username)
    # digest() makes md5 hash of password
    Id.password <- which(user$passwd == digest(Password)) 
    
    #sqlserver
    Id.userid <- user$id_user
    
    if (length(Id.username) == 1 && length(Id.password) >= 1 &&  (Id.username %in% Id.password)){
      values$authenticated <- TRUE
      #sqlserver
      users_data$id_user <<- Id.userid
      
      users_data$user <<- input$userLogin
      users_data$loginTime <<- Sys.time()
      values$logError <- FALSE
    }else if(length(Id.username) == 0 ){
      values$logError <- TRUE
    }else{
      values$logError <- TRUE
    }
    
    
  })
  
  
  #Default matrix values
  #unifamiliar <- as.data.frame(matrix(c(3100,3050,3000,2955,2880), ncol = 5, nrow = 1, byrow=TRUE))
  unifamiliar <- as.data.frame(matrix(c(1.08,1.025,1,0.98,0.96), ncol = 5, nrow = 1, byrow=TRUE))

  
  pluri_NoBajos_2p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.08,1.025,1,0.98,0.96), ncol = 5, nrow = 2, byrow=TRUE))
  pluri_NoBajos_3p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.08,1.025,1,0.98,0.96,1.13,1.075,1.05,1.03,1.01), ncol = 5, nrow = 3, byrow=TRUE))
  pluri_NoBajos_4p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.13,1.075,1.05,1.03,1.01), ncol = 5, nrow = 4, byrow=TRUE))
  pluri_NoBajos_5p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01), ncol = 5, nrow = 5, byrow=TRUE))
  pluri_NoBajos_6p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01,1.15,1.095,1.07,1.05,1.03), ncol = 5, nrow = 6, byrow=TRUE))
  pluri_NoBajos_7p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01,1.15,1.095,1.07,1.05,1.03,1.17,1.115,1.09,1.07,1.05), ncol = 5, nrow = 7, byrow=TRUE))
  pluri_NoBajos_8p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01,1.15,1.095,1.07,1.05,1.03,1.17,1.115,1.09,1.07,1.05,1.19,1.135,1.11,1.09,1.07), ncol = 5, nrow = 8, byrow=TRUE))
  pluri_NoBajos_9p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01,1.15,1.095,1.07,1.05,1.03,1.17,1.115,1.09,1.07,1.05,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1), ncol = 5, nrow = 9, byrow=TRUE))
  pluri_NoBajos_10p <- as.data.frame(matrix(c(1.04,0.985,0.96,0.94,0.92,1.06,1.005,0.98,0.96,0.94,1.08,1.025,1,0.98,0.96,1.1,1.045,1.02,1,0.98,1.13,1.075,1.05,1.03,1.01,1.15,1.095,1.07,1.05,1.03,1.17,1.115,1.09,1.07,1.05,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1,1.24,1.185,1.16,1.14,1.12), ncol = 5, nrow = 10, byrow=TRUE))
  
  # pluri_bajos_NoJardin_2p <- as.data.frame(matrix(c(3159,2998,2925,2881,2808,3240,3075,3000,2955,2880), ncol = 5, nrow = 2, byrow=TRUE))
  # pluri_bajos_NoJardin_3p <- as.data.frame(matrix(c(3159,2998,2925,2881,2808,3434,3260,3180,3132,3053,3532,3352,3270,3221,3139), ncol = 5, nrow = 3, byrow=TRUE))
  # pluri_bajos_NoJardin_4p <- as.data.frame(matrix(c(3159,2998,2925,2881,2808,3451,3275,3195,3147,3067,3532,3352,3270,3221,3139,3629,3444,3360,3310,3226), ncol = 5, nrow = 4, byrow=TRUE))
  # pluri_bajos_NoJardin_5p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3467,3290,3210,3162,3082,3645,3459,3375,3324,3240), ncol = 5, nrow = 5, byrow=TRUE))
  # pluri_bajos_NoJardin_6p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3645,3459,3375,3324,3240), ncol = 5, nrow = 6, byrow=TRUE))
  # pluri_bajos_NoJardin_7p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3596,3413,3330,3280,3197,3710,3521,3435,3383,3298), ncol = 5, nrow = 7, byrow=TRUE))
  # pluri_bajos_NoJardin_8p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3596,3413,3330,3280,3197,3694,3506,3420,3369,3283,3791,3598,3510,3457,3370), ncol = 5, nrow = 8, byrow=TRUE))
  # pluri_bajos_NoJardin_9p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3596,3413,3330,3280,3197,3694,3506,3420,3369,3283,3791,3598,3510,3457,3370,3888,3690,3600,3546,3456), ncol = 5, nrow = 9, byrow=TRUE))
  # pluri_bajos_NoJardin_10p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3596,3413,3330,3280,3197,3694,3506,3420,3369,3283,3791,3598,3510,3457,3370,3888,3690,3600,3546,3456,3985,3782,3690,3635,3542), ncol = 5, nrow = 10, byrow=TRUE))
  # pluri_bajos_NoJardin_11p <- as.data.frame(matrix(c(3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3240,3075,3000,2955,2880,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3596,3413,3330,3280,3197,3694,3506,3420,3369,3283,3791,3598,3510,3457,3370,3888,3690,3600,3546,3456,3985,3782,3690,3635,3542,4082,3875,3780,3723,3629), ncol = 5, nrow = 11, byrow=TRUE))
  pluri_bajos_NoJardin_2p <- as.data.frame(matrix(c(1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96), ncol = 5, nrow = 2, byrow=TRUE))
  pluri_bajos_NoJardin_3p <- as.data.frame(matrix(c(1.05,0.995,0.97,0.95,0.93,1.14,1.085,1.06,1.04,1.02,1.17,1.115,1.09,1.07,1.05), ncol = 5, nrow = 3, byrow=TRUE))
  pluri_bajos_NoJardin_4p <- as.data.frame(matrix(c(1.05,0.995,0.97,0.95,0.93,1.15,1.095,1.07,1.05,1.03,1.17,1.115,1.09,1.07,1.05,1.2,1.145,1.12,1.1,1.08), ncol = 5, nrow = 4, byrow=TRUE))
  pluri_bajos_NoJardin_5p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.15,1.095,1.07,1.05,1.03,1.21,1.155,1.13,1.11,1.09), ncol = 5, nrow = 5, byrow=TRUE))
  pluri_bajos_NoJardin_6p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.21,1.155,1.13,1.11,1.09), ncol = 5, nrow = 6, byrow=TRUE))
  pluri_bajos_NoJardin_7p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.19,1.135,1.11,1.09,1.07,1.23,1.175,1.15,1.13,1.11), ncol = 5, nrow = 7, byrow=TRUE))
  pluri_bajos_NoJardin_8p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1,1.25,1.195,1.17,1.15,1.13), ncol = 5, nrow = 8, byrow=TRUE))
  pluri_bajos_NoJardin_9p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1,1.25,1.195,1.17,1.15,1.13,1.28,1.225,1.2,1.18,1.16), ncol = 5, nrow = 9, byrow=TRUE))
  pluri_bajos_NoJardin_10p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1,1.25,1.195,1.17,1.15,1.13,1.28,1.225,1.2,1.18,1.16,1.31,1.255,1.23,1.21,1.19), ncol = 5, nrow = 10, byrow=TRUE))
  pluri_bajos_NoJardin_11p <- as.data.frame(matrix(c(1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.19,1.135,1.11,1.09,1.07,1.22,1.165,1.14,1.12,1.1,1.25,1.195,1.17,1.15,1.13,1.28,1.225,1.2,1.18,1.16,1.31,1.255,1.23,1.21,1.19,1.34,1.285,1.26,1.24,1.22), ncol = 5, nrow = 11, byrow=TRUE))
  
  
  # pluri_bajos_jardin_2p <- as.data.frame(matrix(c(3159,2998,2925,2881,2808,3321,3152,3075,3029,2952), ncol = 5, nrow = 2, byrow=TRUE))
  # pluri_bajos_jardin_3p <- as.data.frame(matrix(c(3467,3290,3210,3162,3082,3240,3075,3000,2955,2880,3629,3444,3360,3310,3226), ncol = 5, nrow = 3, byrow=TRUE))
  # pluri_bajos_jardin_4p <- as.data.frame(matrix(c(3499,3321,3240,3191,3110,3013,2860,2790,2748,2678,3272,3106,3030,2985,2909,3791,3598,3510,3457,3370), ncol = 5, nrow = 4, byrow=TRUE))
  # pluri_bajos_jardin_5p <- as.data.frame(matrix(c(3499,3321,3240,3191,3110,3013,2860,2790,2748,2678,3159,2998,2925,2881,2808,3321,3152,3075,3029,2952,3791,3598,3510,3457,3370), ncol = 5, nrow = 5, byrow=TRUE))
  # pluri_bajos_jardin_6p <- as.data.frame(matrix(c(3483,3306,3225,3177,3096,2689,2552,2490,2453,2390,2851,2706,2640,2600,2534,3159,2998,2925,2881,2808,3321,3152,3075,3029,2952,3953,3752,3660,3605,3514), ncol = 5, nrow = 6, byrow=TRUE))
  # pluri_bajos_jardin_7p <- as.data.frame(matrix(c(3483,3306,3225,3177,3096,2689,2552,2490,2453,2390,2851,2706,2640,2600,2534,3240,3075,3000,2955,2880,3467,3290,3210,3162,3082,3726,3536,3450,3398,3312,4050,3844,3750,3694,3600), ncol = 5, nrow = 7, byrow=TRUE))
  # pluri_bajos_jardin_8p <- as.data.frame(matrix(c(3483,3306,3225,3177,3096,2689,2552,2490,2453,2390,2851,2706,2640,2600,2534,3159,2998,2925,2881,2808,3321,3152,3075,3029,2952,3483,3306,3225,3177,3096,3726,3536,3450,3398,3312,4050,3844,3750,3694,3600), ncol = 5, nrow = 8, byrow=TRUE))
  pluri_bajos_jardin_2p <- as.data.frame(matrix(c(1.05,0.995,0.97,0.95,0.93,1.11,1.055,1.03,1.01,0.99), ncol = 5, nrow = 2, byrow=TRUE))
  pluri_bajos_jardin_3p <- as.data.frame(matrix(c(1.15,1.095,1.07,1.05,1.03,1.08,1.025,1,0.98,0.96,1.2,1.145,1.12,1.1,1.08), ncol = 5, nrow = 3, byrow=TRUE))
  pluri_bajos_jardin_4p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.06,1.04,1.01,0.955,0.93,0.91,0.89,1.09,1.035,1.01,0.99,0.97,1.25,1.195,1.17,1.15,1.13), ncol = 5, nrow = 4, byrow=TRUE))
  #pluri_bajos_jardin_4p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.1,1.04,1.01,0.955,0.93,0.95,0.89,1.09,1.035,1.01,1.03,0.97,1.25,1.195,1.17,1.21,1.13), ncol = 5, nrow = 4, byrow=TRUE))
  pluri_bajos_jardin_5p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.06,1.04,1.01,0.955,0.93,0.91,0.89,1.05,0.995,0.97,0.95,0.93,1.11,1.055,1.03,1.01,0.99,1.25,1.195,1.17,1.15,1.13), ncol = 5, nrow = 5, byrow=TRUE))
  pluri_bajos_jardin_6p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.06,1.04,0.91,0.855,0.83,0.81,0.79,0.96,0.905,0.88,0.86,0.84,1.05,0.995,0.97,0.95,0.93,1.11,1.055,1.03,1.01,0.99,1.3,1.245,1.22,1.2,1.18), ncol = 5, nrow = 6, byrow=TRUE))
  pluri_bajos_jardin_7p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.06,1.04,0.91,0.855,0.83,0.81,0.79,0.96,0.905,0.88,0.86,0.84,1.08,1.025,1,0.98,0.96,1.15,1.095,1.07,1.05,1.03,1.23,1.175,1.15,1.13,1.11,1.33,1.275,1.25,1.23,1.21), ncol = 5, nrow = 7, byrow=TRUE))
  pluri_bajos_jardin_8p <- as.data.frame(matrix(c(1.16,1.105,1.08,1.06,1.04,0.91,0.855,0.83,0.81,0.79,0.96,0.905,0.88,0.86,0.84,1.05,0.995,0.97,0.95,0.93,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.23,1.175,1.15,1.13,1.11,1.33,1.275,1.25,1.23,1.21), ncol = 5, nrow = 8, byrow=TRUE))
  
  #torre <- as.data.frame(matrix(c(1901,1804,1760,1734,1690,2106,1999,1950,1921,1872,2160,2050,2000,1970,1920,2214,2101,2050,2019,1968,2322,2204,2150,2118,2064,2354,2235,2180,2147,2093,2419,2296,2240,2206,2150,2484,2358,2300,2266,2208,2549,2419,2360,2325,2266,2614,2481,2420,2384,2323,2722,2583,2520,2482,2419,2786,2645,2580,2541,2477,2851,2706,2640,2600,2534,2938,2788,2720,2679,2611,3002,2850,2780,2738,2669,3067,2911,2840,2797,2726,3154,2993,2920,2876,2803,3197,3034,2960,2916,2842,3240,3075,3000,2955,2880), ncol = 5, nrow = 19, byrow=TRUE))
  torre <- as.data.frame(matrix(c(0.96,0.905,0.88,0.86,0.84,1.05,0.995,0.97,0.95,0.93,1.08,1.025,1,0.98,0.96,1.11,1.055,1.03,1.01,0.99,1.16,1.105,1.08,1.06,1.04,1.17,1.115,1.09,1.07,1.05,1.2,1.145,1.12,1.1,1.08,1.23,1.175,1.15,1.13,1.11,1.26,1.205,1.18,1.16,1.14,1.29,1.235,1.21,1.19,1.17,1.34,1.285,1.26,1.24,1.22,1.37,1.315,1.29,1.27,1.25,1.4,1.345,1.32,1.3,1.28,1.44,1.385,1.36,1.34,1.32,1.47,1.415,1.39,1.37,1.35,1.5,1.445,1.42,1.4,1.38,1.54,1.485,1.46,1.44,1.42,1.56,1.505,1.48,1.46,1.44,1.58,1.525,1.5,1.48,1.46), ncol = 5, nrow = 19, byrow=TRUE))
  
  goodImportance <- c(1, 1.0125, 1.025, 1.0375, 1.05)
  badImportance <- c(1, 0.9875, 0.975, 0.9625, 0.95)
  
  loadTable <- function(){
    
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      ############### Lectura .csv ###########################
      # df <- read.csv(
      #   input$file1$datapath,
      #   header = TRUE, #input$header,
      #   sep = ";",  #input$sep,
      #   quote = ""
      # )
      
      ############## Nombre Promo   ###########################
      nombre_promo <- openxlsx::read.xlsx(input$file1$datapath, sheet = "Resumen Ejecutivo",  colNames = FALSE, cols = c(10), rows = c(16))
      values$namePromo <- tolower(nombre_promo$X1)
      
      sql_query <- "select distinct ID_PROMOCION PROYECTO, CONCAT(ID_PROMOCION, ' - ', TX_PROMOCION) PROMOCION
              from [NH_DWH_ODS].[dbo].[PROMOCIONES] 
              where TX_UNIDAD_NEGOCIO IN ('WIP', '1st GO', 'FP DEVELOPMENT') 
              AND TX_TIPO_PROMOCION = 'OBRA_NUEVA' 
              AND TX_SECTOR = 'PERIMETRO' 
              AND TX_PROMOCION LIKE '%aretxabaleta%' ";
      
      project_promo <- query_sqlserver_db(sql_query)
      values$proyecto <- project_promo["PROYECTO"][1,1] 
      values$promocion <- project_promo["PROMOCION"][1,1]
      
      ############### Lectura .xlsm ###########################
      superficies <- openxlsx::read.xlsx(input$file1$datapath, sheet = "VIVIENDAS DATA",  colNames = TRUE)
      superficies <- superficies[rowSums(is.na(superficies[ ,1:63])) < 7, ]
      superficies <- superficies %>% group_by(`Portal/Escalera.VIVIENDAS`, Planta.VIVIENDAS, Puerta.VIVIENDAS) %>% mutate(T_S_U_Terraza =  max(as.numeric(as.character(Terraza) ), (as.numeric(as.character(Terraza.1))+as.numeric(as.character(Terraza.2)) ) ),
                                                           T_S_U_Porche = max(as.numeric(as.character(Porche) ), (as.numeric(as.character(Porche.1))+as.numeric(as.character(Porche.2)) ) ),
                                                           T_S_U_Jardin = max(as.numeric(as.character(Jardín) ), (as.numeric(as.character(Jardín.1))+as.numeric(as.character(Jardin.2)) ) ) )
      
      df <- superficies %>% select(`Portal/Escalera.VIVIENDAS`, Planta.VIVIENDAS, Puerta.VIVIENDAS, N.Dorm, T.S.U..INTERIOR, Terraza.1, Terraza.2, T_S_U_Terraza, T_S_U_Porche, T_S_U_Jardin, `ORIENTACIÓN.SALÓN.-.COMEDOR`, `ORIENTACIÓN.DORMITORIO.PRINCIPAL`) %>% rename(Portal = `Portal/Escalera.VIVIENDAS`, Planta = Planta.VIVIENDAS, Puerta = Puerta.VIVIENDAS,  Dormitorios = N.Dorm, T_S_U_Interior= T.S.U..INTERIOR, T_S_U_TerrazaCubierta = Terraza.1, T_S_U_TerrazaDescubierta=Terraza.2, Or_Salon=`ORIENTACIÓN.SALÓN.-.COMEDOR`, Or_Dormitorio=`ORIENTACIÓN.DORMITORIO.PRINCIPAL` )
      extractNumber <- function(x) str_extract(x, "[0-9]+")
      df$Planta <- as.numeric( sapply( df$Planta, extractNumber) )
      df$Portal <- as.factor(df$Portal)
      df$Puerta <- as.factor(df$Puerta)
      
      
      locales <- openxlsx::read.xlsx(input$file1$datapath, sheet = "LOCALES DATA",  colNames = TRUE)
      locales <- locales[rowSums(is.na(locales[ ,1:8])) < 3, ]
      
      locales$ID.PROMOCION <- NULL
      locales$S.U..Locales <- NULL
      locales$Rep.Z.Com..LOCALES <- NULL
      locales$S.C.TOTAL.VENTA.LOCALES <- NULL
      locales$S..COMPUTABLE.LOCALES <- NULL
      colnames(locales) <- c("Portal", "Planta", "Puerta", "T_S_U_Interior")
      
      if(nrow(locales)>0){
        locales$Planta <- 0
        locales$Dormitorios <- "Local"
        locales$T_S_U_Terraza <- NA
        locales$T_S_U_Jardin <- NA
        locales$Or_Salon <- NA
        locales$Or_Dormitorio <- NA 
      }


      
      #########################################################
      
            
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    #initialize reactive value promDf
    values$promDf <- df
    values$localesDf <- locales
    
    values$summaryDF <- df %>% group_by(Dormitorios) %>% summarise(unidades = n(), superficie_media = round(mean( T_S_U_Interior , na.rm = T ), digits=2) ) %>% rename(tipologia = Dormitorios)
    values$showsummary <- TRUE
    
    
    # if (input$disp == "head") {
    #   return(head(values$promDf))
    # }
    # else {
    #   return(values$promDf)
    # }
    
    return(values$promDf)
    
    
  }
  
  loadTarifaReal <- function(){
    
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      trdf <- read.csv(
        input$file2$datapath,
        header = TRUE, #input$header,
        sep = ";", #input$sep,
        quote = ""
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    #initialize reactive value promDf
    values$tarifaRealDf <- trdf
    
  }
  
  output$summary <-   DT::renderDataTable({
    values$summaryDF
  }, style = 'bootstrap', rownames = FALSE, options = list(autoWidth = T, pageLength = 1000, dom = 't'), colnames = c('Tipología','Unids.','Sup. media') )
  
  
  #output$contents <- renderTable({
  output$contents <-   DT::renderDataTable({
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    loadTable()
    
  } ,  filter = 'top', style = 'bootstrap', rownames = FALSE, options = list(autoWidth = T, pageLength = 1000, dom = 'ft'), colnames = c('Portal','Planta','Puerta', 'Dormitorios', 'Util (m2)','Terraza Cubierta (m2)','Terraza Desubierta (m2)','Terraza (m2)','Porche (m2)','Jardin (m2)','Or. Salon', 'Or. Dormitorio') ) 


  observeEvent( input$calculate, {
    
    #values$showcontents <- TRUE
    
    isolate(calculateCounter$count <- calculateCounter$count + 1)
    
    salon <- c(input$sNO, input$sN, input$sNE, input$sO, input$sE, input$sSO, input$sS, input$sSE)
    names(salon) <- c("NO", "N", "NE", "O", "E", "SO", "S", "SE")
    dormitorio <- c(input$dNO, input$dN, input$dNE, input$dO, input$dE, input$dSO, input$dS, input$dSE)
    names(dormitorio) <- c("NO", "N", "NE", "O", "E", "SO", "S", "SE")
    
    # read the current value
    current_table <- values$promDf

    #read current locales 
    current_locales <- values$localesDf
    
    terracePercent <- input$terrace
    gardenPercent <- input$garden 
    
    if( input$tipoPromocion == 1){
      terracePercent <- 40
      gardenPercent <- 40 
    } else if( input$tipoPromocion == 2 ){
      terracePercent <- 60
      gardenPercent <- 60 
    } else if( input$tipoPromocion == 3 ){
      terracePercent <- 50
      gardenPercent <- 30 
    }

    totalTarifa <- as.numeric(input$totalPrice)
    garajeTarifa <- 0    
    if(!is.na(as.numeric(input$garageNumber)) && !is.na(as.numeric(input$garagePrice))){
      garajeTarifa <- as.numeric(input$garageNumber) *  as.numeric(input$garagePrice)
    }
    totalLocales <- 0    
    if(!is.na(as.numeric(input$comercialPricem2)) && !is.na(as.numeric(input$comercialPricem2))){
      totalLocales <- sum(current_locales$T_S_U_Interior) *  as.numeric(input$comercialPricem2)
    }
    totalViviendas <- totalTarifa - garajeTarifa - totalLocales
    
    
    values$terracePercent <- terracePercent
    values$gardenPercent <- gardenPercent
    
    total_sq_mt <- sum( current_table$T_S_U_Interior ) + ( terracePercent * sum( current_table$T_S_U_Terraza )  )/100 + (gardenPercent*( sum( current_table$T_S_U_Jardin ) ) )/100
    mean_sq2m_price <- ceiling(totalViviendas / total_sq_mt)
    
    heigh_room_count_matrix <- as.data.frame.matrix( table(current_table$Planta, current_table$Dormitorios) )
    mean_room <- round(mean(current_table$Dormitorios), digits = 0) 
    mean_heigh <- round(mean(current_table$Planta), digits = 0)   
    
    different_heighs <- dim(heigh_room_count_matrix)[1]
    different_rooms <- dim(heigh_room_count_matrix)[2]

    index_height_mean <- which(rownames(heigh_room_count_matrix) %in% c(mean_heigh))
    index_room_mean <- which(colnames(heigh_room_count_matrix) %in% c(mean_room))
    
    gapHeight <- ifelse( input$gapH == FALSE, (1 + input$height) * 15, as.numeric(input$heightGapPrice) )
    gapTipology <- ifelse( input$gapT == FALSE, (1 + input$tipology) * 15, as.numeric(input$tipologyGapPrice) )
    
    # if( input$tipoPromocion == 1){
    #   gapHeight <- 50
    #   gapTipology <- 30 
    # } else if(input$tipoPromocion == 2){
    #   gapHeight <- 0
    #   gapTipology <- 50 
    # } else if(input$tipoPromocion == 3){
    #   gapHeight <- 60
    #   gapTipology <- 20 
    # }
    
    if(input$tipoPromocion == 4){
      central_col <- as.vector(seq(mean_sq2m_price-((index_height_mean-1)*gapHeight), by = gapHeight, length.out = different_heighs))
      first_col <- central_col + ((index_room_mean-1)*gapTipology)
      
      a1 <- first_col
      for (i in 1:different_rooms-1 ){
        a1 <- rbind(a1, (first_col - i*gapTipology)) 
      }
      prices_m2 <- as.data.frame(t(a1))[-c(1)]
      colnames(prices_m2) <- paste("V", unique(current_table$Dormitorios), sep="")
      rm(a1)
      values$prices_m2 <- prices_m2 

    } else if( input$tipoPromocion == 1){
        if(input$tipoBajoJardin == 1){
          name <- paste("pluri_NoBajos_", nrow(heigh_room_count_matrix), "p", sep = "")
        }else if(input$tipoBajoJardin == 2){
          name <- paste("pluri_bajos_jardin_", nrow(heigh_room_count_matrix), "p", sep = "")
        }else if(input$tipoBajoJardin == 3){
          name <- paste("pluri_bajos_NoJardin_", nrow(heigh_room_count_matrix), "p", sep = "")
        }
        prices_m2 <- get(name) * mean_sq2m_price
    } else if(input$tipoPromocion == 2){
        prices_m2 <- unifamiliar * mean_sq2m_price
    } else if(input$tipoPromocion == 3){
      prices_m2 <- torre * mean_sq2m_price
    }

    if( 0 %in% unique(current_table$Planta) ){
      rownames(prices_m2) <- as.numeric(as.character(rownames(prices_m2))) - 1
      values$prices_m2 <- prices_m2
    }
    
#newtarifa <- apply( current_table[, c( "Or_Dormitorio", "Or_Salon", "Planta", "Dormitorios", "T_S_U_Interior", "T_S_U_Jardin", "T_S_U_Terraza")], 1, getTarifa )
    
    current_table <- current_table %>% group_by(Portal, Planta, Puerta) %>% mutate(arg1 = mapply(getCoeff, dormitorio[as.character(Or_Dormitorio)],  as.numeric(input$dormOrientation) ), arg2 = mapply(getCoeff, salon[as.character(Or_Salon)],  as.numeric(input$salonOrientation) ), arg3 = ( T_S_U_Interior + (gardenPercent * T_S_U_Jardin / 100) + ( terracePercent * T_S_U_Terraza / 100 ) ), precio_m2 = mapply( getPrice, Planta, Dormitorios ), tarifa = arg1*arg2*arg3*precio_m2)
    current_table$arg1 <- NULL
    current_table$arg2 <- NULL
    current_table$arg3 <- NULL
    
    # # update the value of tarifa
    # for (i in 1:nrow(current_table)){
    #   current_table$tarifa[i] <- getCoeff(dormitorio[as.character(current_table$Or_Dormitorio[i])], as.numeric(input$dormOrientation)) * getCoeff(salon[as.character(current_table$Or_Salon[i])], as.numeric(input$salonOrientation)) * prices_m2[ current_table$Planta[i], paste("V",current_table$Dormitorios[i], sep = "") ] * ( current_table$T_S_U_Interior[i] + (gardenPercent * current_table$T_S_U_Jardin[i] / 100) + ( terracePercent * (current_table$T_S_U_TerrazaCubierta[i] + current_table$T_S_U_TerrazaDescubierta[i]) / 100 ) ) 
    #   #print( paste( current_table$Or_Dormitorio[i], dormitorio[as.character(current_table$Or_Dormitorio[i])], getCoeff(as.factor(dormitorio[as.character(current_table$Or_Dormitorio[i])]), as.numeric(input$dormOrientation)) , getCoeff(dormitorio[as.character(current_table$Or_Dormitorio[i])], as.numeric(input$dormOrientation)), current_table$Or_Salon[i], salon[as.character(current_table$Or_Salon[i])], getCoeff(as.factor(salon[as.character(current_table$Or_Salon[i])]), as.numeric(input$salonOrientation)), getCoeff(salon[as.character(current_table$Or_Salon[i])], as.numeric(input$salonOrientation)), sep=" -- " ) )
    #   current_table$precio_m2[i] <- prices_m2[ current_table$Planta[i], paste("V",current_table$Dormitorios[i], sep = "") ]
    # }
    
   
    b <- sum(current_table$tarifa)
    
    current_table$tarifa <- round( current_table$tarifa * totalViviendas / b , 0)
    
    current_table$precioMedioFinal_M2 <- round( current_table$tarifa / ( current_table$T_S_U_Interior + (gardenPercent * current_table$T_S_U_Jardin / 100) + (terracePercent * current_table$T_S_U_Terraza / 100)  ) , 0)
    
    c <- sum(current_table$tarifa)
    
    precioMedio <- c/dim(current_table)[1]
    
    #current_table$respectoMedia <- round(100 * (current_table$tarifa - precioMedio) / precioMedio, 2) 
    #current_table$respectoMedia <- round( (current_table$tarifa - precioMedio) / precioMedio, 4) 
    current_table$diferencia_M2_Medio <- round( (current_table$precioMedioFinal_M2 - mean_sq2m_price) / mean_sq2m_price, 4) 
       
    
    dormitorios <- current_table[current_table$Dormitorios == 1,]
    dormitorios$ranking <- rank(-dormitorios$tarifa)
    limit <- ceiling(2*nrow(dormitorios)/10)
    dormitorios$tipo <- if_else( dormitorios$ranking <= limit, "Joya", if_else(dormitorios$ranking > nrow(dormitorios)-limit, "Cola", "Tipo"))
    for(i in 2:8){
      dorm <- current_table[current_table$Dormitorios == i,]
      dorm$ranking <- rank(-dorm$tarifa)
      limit <- ceiling(2*nrow(dorm)/10)
      dorm$tipo <- if_else( dorm$ranking <= limit, "Joya", if_else(dorm$ranking > nrow(dorm)-limit, "Cola", "Tipo"))
      
      dormitorios <- rbind(dormitorios, dorm) 
    }

    new_table <- dormitorios %>% select( Portal, Planta, Puerta, Dormitorios, T_S_U_Interior, T_S_U_Terraza, T_S_U_Jardin, Or_Salon, Or_Dormitorio, tarifa, precioMedioFinal_M2, diferencia_M2_Medio, tipo) %>% arrange(Portal, Planta, Puerta)
    
    current_locales$tarifa <- current_locales$T_S_U_Interior * as.numeric(input$comercialPricem2)
    current_locales$precioMedioFinal_M2 <- as.numeric(input$comercialPricem2)
    current_locales$diferencia_M2_Medio <- 0
    current_locales$tipo <- NA

    current_locales <- current_locales %>% select(Portal, Planta, Puerta, Dormitorios, T_S_U_Interior, T_S_U_Terraza, T_S_U_Jardin, Or_Salon, Or_Dormitorio, tarifa, precioMedioFinal_M2, diferencia_M2_Medio, tipo)
    
    #current_table$Tarifa <- "200.000"
    #new_table <- current_table
    
    # write the new value to the reactive value
    #values$promDf = new_table
    values$promDfWithTarifa <- new_table
    
    Sys.sleep(1)  # long computation

    # scrollX = '1000px', scrollY = '500px',
    opt1 <- list(autoWidth = T, pageLength = 1000, dom = 't',
                footerCallback = JS(
                  "function( tfoot, data, start, end, display ) {",
                  "var api = this.api(), data;",
                  "var summary4 = api.column(4).data().reduce( function ( a, b ) {",
                  "return Math.round(a + b);",
                  "} )",
                  "$( api.column(4).footer()).html(",
                  "(summary4).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var summary5 = api.column(5).data().reduce( function ( a, b ) {",
                  "return Math.round(a + b);",
                  "} )",
                  "$( api.column(5).footer()).html(",
                  "(summary5).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var summary6 = api.column(6).data().reduce( function ( a, b ) {",
                  "return Math.round(a + b);",
                  "} )",
                  "$( api.column(6).footer()).html(",
                  "(summary6).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var summary9 = api.column(9).data().reduce( function ( a, b ) {",
                  "return a + b;",
                  "} )",
                  "$( api.column(9).footer()).html('€'+",
                  "(summary9).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var rowsnum = api.column(10).data().count();",
                  "var sumOverPriceM2 = api.column(10).data().reduce( function ( a, b ) {",
                  "return Math.round((Number(a)) + (Number(b)));",
                  "} )",
                  "$( api.column(10).footer()).html('€'+",
                  " (sumOverPriceM2/rowsnum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");","}")
    )
    
    Names1 <- c('Portal','Planta','Puerta', 'Dorm.', 'Util (m2)','Terraza (m2)','Jardin (m2)','Or. Salon', 'Or. Dorm.','Precio (€)', 'Precio (€/m2)', 'Diferencia respecto €/m2 medio', 'Rank')
    FooterNames1 <- c(rep("",4),Names1[5:7],rep("",2),Names1[10],rep("",3))
    
    sketch1 <- htmltools::withTags(table(
      tableHeader(Names1),tableFooter(FooterNames1)
    ))
    
    output$contents = DT::renderDataTable(DT::datatable(new_table,  filter = 'top', style = 'bootstrap', rownames = FALSE, container=sketch1, options = opt1, colnames = Names1  ) %>% formatCurrency(c('tarifa', 'precioMedioFinal_M2'), '\U20AC', digits = 0, interval = 3, mark = "." ) %>% formatPercentage('diferencia_M2_Medio', 2) )

    if( nrow(current_locales) > 0 ){
      output$contentsLocales = DT::renderDataTable(DT::datatable(current_locales, filter = 'top', style = 'bootstrap', rownames = FALSE, container=sketch1, options = opt1, colnames = Names1  ) %>% formatCurrency(c('tarifa', 'precioMedioFinal_M2'), '\U20AC', digits = 0, interval = 3, mark = "." ) %>% formatPercentage('diferencia_M2_Medio', 2) )
    }
    
    
    # leganes_geojson <- readLines("www/leganes.geojson", warn = FALSE) %>% paste(collapse = "\n") %>% fromJSON(simplifyVector = FALSE)
    # leganes_geojson$style = list(
    #   weight = 1,
    #   color = "#FF0000",
    #   fillColor = "#0000FF",
    #   opacity = 0.6,
    #   fillOpacity = 0.2
    # )
    
    #Creamos el mapa y le añadimos el geojson
    #m <- leaflet() %>% setView(lng = -3.780736, lat = 40.343159, zoom = 18) %>% addProviderTiles(providers$Stamen.Toner) %>% addGeoJSON(leganes_geojson) 
    # %>% addSearchFeatures(targetGroups  = "id", options = searchFeaturesOptions(zoom = 10, openPopup = TRUE)) %>% addControl("<P><B>Hint!</B> Busqueda por piso</P>")
    
    #output$mymap = renderLeaflet(m)
    
    
    values$summaryDF <- new_table %>% group_by(Dormitorios) %>% summarise( unidades = n(), superficie_media = round(mean( T_S_U_Interior , na.rm = T ), digits=1), ingreso_total = sum(tarifa, na.rm = T), percent = ingreso_total/totalTarifa, precio_unitario = round(ingreso_total/unidades, digits=2), repercusion_media = round(mean(precioMedioFinal_M2, na.rm = T ), digits=2) ) %>% rename(tipologia = Dormitorios)
    if( nrow(current_locales) > 0 ){
      values$summaryDF <- rbind(values$summaryDF, c("Local", nrow(current_locales), round(sum(current_locales$T_S_U_Interior)/nrow(current_locales) , digits=1), sum(current_locales$tarifa ), sum(current_locales$tarifa )/totalTarifa, sum(current_locales$tarifa )/nrow(current_locales), as.numeric(input$comercialPricem2)  ))
    }
    
    resume_m2DF <- values$summaryDF
    
    
    if(!is.na(as.numeric(input$garageNumber))){
      values$summaryDF <- rbind(values$summaryDF, c("Garaje", as.numeric(input$garageNumber), NA, as.numeric(input$garageNumber)*as.numeric(input$garagePrice), (as.numeric(input$garageNumber)*as.numeric(input$garagePrice)/totalTarifa ),as.numeric(input$garagePrice), NA ) )
    }
    
    
    optSummary <- list(autoWidth = T, pageLength = 1000, dom = 't',
                footerCallback = JS(
                  "function( tfoot, data, start, end, display ) {",
                  "var api = this.api(), data;",
                  "$( api.column(1).footer()).html(",
                  "api.column(1).data().reduce( function ( a, b ) {",
                  "return Math.round(Number(a) + Number(b));",
                  "} )",
                  ");",
                  "var sumaIT = api.column(3).data().reduce( function ( a, b ) {",
                  "return Math.round(Number(a) + Number(b));",
                  "} )",
                  "$( api.column(3).footer()).html('€'+",
                  "(sumaIT).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var resume = api.column(4).data().reduce( function ( a, b ) {",
                  "return Math.round((Number(a)) + (Number(b)));",
                  "} )",
                  "$( api.column(4).footer()).html(",
                  " 100*resume + '%' ",
                  ");",
                  "var sumOverCeil = api.column(5).data().reduce( function ( a, b ) {",
                  "return Math.round((Number(a)) + (Number(b)));",
                  "} )",
                  "var rowsnum = api.column(5).data().count();",
                  "if( api.column(0).data().indexOf('Garaje') > -1 ){ rowsnum = rowsnum-1 };",
                  "$( api.column(5).footer()).html('€'+",
                  " (sumOverCeil/rowsnum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");",
                  "var sumOverCeilM2 = api.column(6).data().reduce( function ( a, b ) {",
                  "return Math.round((Number(a)) + (Number(b)));",
                  "} )",
                  "$( api.column(6).footer()).html('€'+",
                  " (sumOverCeilM2/rowsnum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                  ");","}")
    )
    
    
    NamesSummary <- c('Tipología','Unids.','Sup. media', 'Ingreso Total', 'Porcentaje', 'Precio medio unitario', 'Repercusión media m2')
    FooterNamesSummary <- c(rep("",1),NamesSummary[2],rep("",1),NamesSummary[4], NamesSummary[5], rep("",2))
    
    sketchSummary <- htmltools::withTags(table(
      tableHeader(NamesSummary),tableFooter(FooterNamesSummary)
    ))
    
    output$summary = DT::renderDataTable(DT::datatable(values$summaryDF, style = 'bootstrap', rownames = FALSE, container = sketchSummary, colnames = NamesSummary , options = optSummary ) %>% formatCurrency(c('ingreso_total', 'precio_unitario', 'repercusion_media'), '\U20AC', digits = 0, interval = 3, mark = "." ) %>% formatPercentage('percent', 2)  ) 
  
      
    output$summaryM2Plot = renderPlot({
      ggplot(resume_m2DF, aes(x = tipologia, y =  gsub(',','.', comma(round(as.numeric(repercusion_media), 0)) )   )) + geom_bar(stat = "identity", col = 'blue', fill = 'blue') + geom_text(aes(label = paste(gsub(',','.', comma(round(as.numeric(repercusion_media), 0)) ), "€")), position = position_dodge(0.9), vjust = -0.5) +
        xlab("Tipología") + ylab("Repercusión (€/m2)") + theme_minimal() + theme( panel.border = element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Repercusión €/m2 por tipología") + theme(
          plot.title = element_text(color="black", size=14, face="italic", hjust = 0.5)) 
    })
    
   # + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
    
    output$summaryMeanPlot = renderPlot({
      ggplot(values$summaryDF, aes(x = tipologia, y = gsub(',','.', comma(round(as.numeric(precio_unitario), 0)) )   )) + geom_bar(stat = "identity", col = 'blue', fill = 'blue') + geom_text(aes(label = paste(gsub(',','.', comma(round(as.numeric(precio_unitario), 0)) ), "€")), position = position_dodge(0.9), vjust = -0.5) +
        xlab("Tipología") + ylab("Precio unitario (€)") + theme_minimal() + theme( panel.border = element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Precio medio por tipología") + theme(
          plot.title = element_text(color="black", size=14, face="italic", hjust = 0.5))
    })
    
    
    parameter_data <- data.frame(tarifaDate = Sys.time(),
                                 #url = url,
                                 tipoPromo = as.numeric(input$tipoPromocion),
                                 tipoBajo = as.numeric(input$tipoBajoJardin),
                                 gapAltura = gapHeight,
                                 gapDormitorio = gapTipology,
                                 repercusionTerraza = terracePercent,
                                 repercusionJardin = gardenPercent,
                                 salonNO = input$sNO,
                                 salonN = input$sN,
                                 salonNE = input$sNE,
                                 salonO = input$sO,
                                 salonE = input$sE,
                                 salonSO = input$sSO,
                                 salonS = input$sS,
                                 salonSE = input$sSE,
                                 importanciaSalon = as.numeric(input$salonOrientation),
                                 dormitorioNO = input$dNO,
                                 dormitorioN = input$dN,
                                 dormitorioNE = input$dNE,
                                 dormitorioO = input$dO,
                                 dormitorioE = input$dE,
                                 dormitorioSO = input$dSO,
                                 dormitorioS = input$dS,
                                 dormitorioSE = input$dSE,
                                 importanciaDormitorio = as.numeric(input$dormOrientation),
                                 tarifaTotal = as.numeric(input$totalPrice),
                                 m2Locales = as.numeric(input$comercialPricem2),
                                 m2Garajes = as.numeric(input$garagePrice),
                                 garajes = as.numeric(input$garageNumber),
                                 id_sesion = session$token
    ) %>% rename(date = tarifaDate, tipo_promo = tipoPromo, tipo_bajo = tipoBajo, gap_altura = gapAltura, gap_dormitorio = gapDormitorio, repercusion_terraza = repercusionTerraza, repercusion_jardin = repercusionJardin, tarifa_total = tarifaTotal, m2_locales = m2Locales, m2_garajes = m2Garajes, importancia_salon = importanciaSalon, importancia_dormitorio = importanciaDormitorio  )
    #write_df_as_table(parameter_data, "parametros_tarificacion", T)
    write_df_as_sqlserver_table(parameter_data, "parametros_tarificacion", T)
    
     
  })
  
  
  observeEvent( input$compareTo, {
    
    isolate(compareCounter$count <- compareCounter$count + 1)
    
    gP <- isolate(values$gardenPercent)
    tP <- isolate(values$terracePercent)
    
    loadTarifaReal()
    
    actualTable <- values$promDfWithTarifa
    tfrTable <- values$tarifaRealDf
    current_locales <- values$localesDf
    
    comparativeTable <- left_join(actualTable, tfrTable, by = c("Portal","Planta","Puerta")) 
    
    comparativeTable$precioMedioM2_API <- round( comparativeTable$TarifaAPI / ( comparativeTable$T_S_U_Interior + (gP * comparativeTable$T_S_U_Jardin / 100) + (tP * comparativeTable$T_S_U_Terraza / 100)  ) , 0)
    comparativeTable$diferencia <- comparativeTable$tarifa - comparativeTable$TarifaAPI
    
    
    comparativeTable$planos <- shinyInput(actionButton, nrow(comparativeTable), 'plano_', class= "btn-info btn-sm", label = "Plano", onclick = 'Shiny.onInputChange(\"plano_button\",  this.id)' )
    
    comparativeTable$validar <- shinyInput(actionButton, nrow(comparativeTable), 'validate_', class= "btn-primary btn-sm", label = "Validar ->", onclick = 'Shiny.onInputChange(\"validate_button\",  this.id)' )
    # comparativeTable$modificar <- shinyInput(actionButton, nrow(comparativeTable), 'change_', class= "btn-warning btn-sm", label = "Cambiar ->", onclick = 'Shiny.onInputChange(\"change_button\",  this.id)' )
    
    
    
    comparativeTable$tarifaValidada <- NA

    values$finalTable <- comparativeTable
          
    Sys.sleep(1)  # long computation
    
    opt <- list(autoWidth = T, pageLength = 1000, dom = 'ft',
                footerCallback = JS(
                      "function( tfoot, data, start, end, display ) {",
                      "var api = this.api(), data;",
                      "var sum4 = api.column(4).data().reduce( function ( a, b ) {",
                      "return Math.round(a + b);",
                      "} )",
                      "$( api.column(4).footer()).html(",
                      "(sum4).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum5 = api.column(5).data().reduce( function ( a, b ) {",
                      "return Math.round(a + b);",
                      "} )",
                      "$( api.column(5).footer()).html(",
                      "(sum5).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum6 = api.column(6).data().reduce( function ( a, b ) {",
                      "return Math.round(a + b);",
                      "} )",
                      "$( api.column(6).footer()).html(",
                      "(sum6).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum18 = api.column(18).data().reduce( function ( a, b ) {",
                      "return a + b;",
                      "} )",
                      "$( api.column(18).footer()).html('€'+",
                      "(sum18).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum13 = api.column(13).data().reduce( function ( a, b ) {",
                      "return a + b;",
                      "} )",
                      "$( api.column(13).footer()).html('€'+",
                      "(sum13).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum9 = api.column(9).data().reduce( function ( a, b ) {",
                      "return a + b;",
                      "} )",
                      "$( api.column(9).footer()).html('€'+",
                      "(sum9).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var rowsnum = api.column(10).data().count();",
                      "var sum10 = api.column(10).data().reduce( function ( a, b ) {",
                      "return a + b;",
                      "} )",
                      "$( api.column(10).footer()).html('€'+",
                      "(sum10/rowsnum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "var sum14 = api.column(14).data().reduce( function ( a, b ) {",
                      "return a + b;",
                      "} )",
                      "$( api.column(14).footer()).html('€'+",
                      "(sum14/rowsnum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                      ");",
                      "}")
    )
    
    Names <- c('Portal','Planta','Puerta', 'Dorm.', 'Util (m2)','Terraza (m2)','Jardin (m2)','Or. Salon', 'Or. Dorm.','Precio (€)', 'Precio (€/m2)', 'Diferencia respecto €/m2 medio', 'Rank','€ API','€/m2 API','Diferencia', '','', 'Tarifa Validada') 
    FooterNames <- c(rep("",9),Names[10],rep("",3),Names[14], rep("",4),Names[19])
    
    sketch <- htmltools::withTags(table(
      tableHeader(Names),tableFooter(FooterNames)
    ))
    
    output$contents = DT::renderDataTable(DT::datatable(values$finalTable, filter = 'top', style = 'bootstrap', editable = TRUE, rownames = FALSE, container = sketch, options = opt, colnames = Names , escape = FALSE )  %>% formatCurrency(c('tarifa', 'precioMedioFinal_M2', 'TarifaAPI', 'precioMedioM2_API', 'diferencia', 'tarifaValidada'), '\U20AC', digits = 0, interval = 3, mark = "." ) %>% formatPercentage('diferencia_M2_Medio', 2)  )
    values$validateButton <- TRUE
      
    totalAPI <- sum(comparativeTable$TarifaAPI)
    totalPrice <- as.numeric(input$totalPrice)
    garajeTarifa <- 0    
    if(!is.na(as.numeric(input$garageNumber)) && !is.na(as.numeric(input$garagePrice))){
      garajeTarifa <- as.numeric(input$garageNumber) *  as.numeric(input$garagePrice)
    }
    totalLocales <- 0    
    if(!is.na(as.numeric(input$comercialPricem2)) && !is.na(as.numeric(input$comercialPricem2))){
      totalLocales <- sum(current_locales$T_S_U_Interior) *  as.numeric(input$comercialPricem2)
    }
    totalViviendas <- totalPrice - totalLocales - garajeTarifa
    totalAPI <- totalAPI + totalLocales + garajeTarifa
    
    values$totalViviendas <- totalViviendas
    
    summaryTarifa <- values$summaryDF
    summaryTarifa$tipo <- "Predicción"
    summaryTarifa_API <- comparativeTable %>% group_by(Dormitorios) %>% summarise( unidades = n(), superficie_media = round(mean( T_S_U_Interior , na.rm = T ), digits=2), ingreso_total = sum(TarifaAPI, na.rm = T), percent = ingreso_total/totalAPI, precio_unitario = round(ingreso_total/unidades, digits=2), repercusion_media = round(mean(precioMedioM2_API, na.rm = T ), digits=2) ) %>% rename(tipologia = Dormitorios)
    summaryTarifa_API$tipo <- "API"
    
    
    summaryTable <- rbind(summaryTarifa_API, summaryTarifa)
    
    values$summaryDF <- comparativeTable %>% group_by(Dormitorios) %>% summarise( unidades = n(), superficie_media = round(mean( T_S_U_Interior , na.rm = T ), digits=1), ingreso_total = sum(tarifa, na.rm = T), percent = ingreso_total/totalPrice, ingreso_total_api = sum(TarifaAPI, na.rm = T), percent_api=ingreso_total_api/totalAPI , precio_unitario = round(ingreso_total/unidades, digits=2), precio_unitario_api = round(ingreso_total_api/unidades, digits=2), repercusion_media = round(mean(precioMedioFinal_M2, na.rm = T ), digits=2),  repercusion_media_api = round(mean(precioMedioM2_API, na.rm = T ), digits=2) ) %>% rename(tipologia = Dormitorios)
    resume_compare_m2DF <- values$summaryDF
    
    if(!is.na(as.numeric(input$garageNumber))){
      values$summaryDF <- rbind(values$summaryDF, c("Garaje", as.numeric(input$garageNumber), NA, as.numeric(input$garageNumber)*as.numeric(input$garagePrice), (as.numeric(input$garageNumber)*as.numeric(input$garagePrice)/totalPrice ), as.numeric(input$garageNumber)*as.numeric(input$garagePrice), as.numeric(input$garageNumber)*as.numeric(input$garagePrice)/totalAPI , as.numeric(input$garagePrice) , as.numeric(input$garagePrice), NA, NA ) )
      summaryTable <- rbind(summaryTable, c("Garaje", as.numeric(input$garageNumber), NA, as.numeric(input$garageNumber)*as.numeric(input$garagePrice), (as.numeric(input$garageNumber)*as.numeric(input$garagePrice)/totalPrice ),as.numeric(input$garagePrice), NA, 'API' ))
    }
    if(nrow(current_locales) > 0){
      values$summaryDF <- rbind(values$summaryDF, c("Local", nrow(current_locales), round(sum(current_locales$T_S_U_Interior)/nrow(current_locales), digits = 1), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2)), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/totalPrice , sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2)), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/totalAPI  , sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/nrow(current_locales), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/nrow(current_locales), as.numeric(input$comercialPricem2), as.numeric(input$comercialPricem2) ) )
      summaryTable <- rbind(summaryTable, c('Local', nrow(current_locales), round(sum(current_locales$T_S_U_Interior)/nrow(current_locales), digits = 1), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2)), sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/totalPrice, sum(current_locales$T_S_U_Interior*as.numeric(input$comercialPricem2))/nrow(current_locales), as.numeric(input$comercialPricem2), 'API'  ))
    }
    summaryTableWithout <- summaryTable %>% subset(tipologia!= 'Garaje')
    
    
    
    namesOutputSummary = c('Tipología','Unids.','Sup. media', 'Ingreso Total', 'Porcentaje', 'Ingreso Total API', 'Porcentaje API', 'Precio medio unitario', 'Precio medio unitario API', 'Repercusión media m2', 'Repercusión media m2 API') 
    footerNamesSummary <- c(rep("",1),namesOutputSummary[2],rep("",1),namesOutputSummary[4], rep("",1),namesOutputSummary[6], rep("",5))
    optOutputSummary =  list(autoWidth = T, pageLength = 1000, dom = 't',
                             footerCallback = JS(
                               "function( tfoot, data, start, end, display ) {",
                               "var api = this.api(), data;",
                               "var summ1 = api.column(1).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(1).footer()).html(",
                               "(summ1).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var rownum = api.column(2).data().count();",
                               "if( api.column(0).data().indexOf('Garaje') > -1 ){ rownum = rownum-1 };",
                               "var summ2 = api.column(2).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(2).footer()).html(",
                               "(summ2/rownum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ3 = api.column(3).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(3).footer()).html('€'+",
                               "(summ3).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ4 = api.column(4).data().reduce( function ( a, b ) {",
                               "return Math.round((Number(a)) + (Number(b)));",
                               "} )",
                               "$( api.column(4).footer()).html(",
                               " 100*summ4 + '%' ",
                               ");",
                               
                               "var summ5 = api.column(5).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(5).footer()).html('€'+",
                               "(summ5).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ6 = api.column(6).data().reduce( function ( a, b ) {",
                               "return Math.round((Number(a)) + (Number(b)));",
                               "} )",
                               "$( api.column(6).footer()).html(",
                               " 100*summ6 + '%' ",
                               ");",
                               
                               "var summ7 = api.column(7).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(7).footer()).html('€'+",
                               "(summ7/rownum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ8 = api.column(8).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(8).footer()).html('€'+",
                               "(summ8/rownum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ9 = api.column(9).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(9).footer()).html('€'+",
                               "(summ9/rownum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "var summ10 = api.column(10).data().reduce( function ( a, b ) {",
                               "return Math.round(Number(a) + Number(b));",
                               "} )",
                               "$( api.column(10).footer()).html('€'+",
                               "(summ10/rownum).toFixed(2).replace(/\\d(?=(\\d{3})+\\.)/g, '$&,').replace(/,/g, '.').slice(0, -3)",
                               ");",
                               
                               "}")
                             )
    sketchSummary <- htmltools::withTags(table(
      tableHeader(namesOutputSummary),tableFooter(footerNamesSummary)
    ))
    
    output$summary = DT::renderDataTable(DT::datatable(values$summaryDF, style = 'bootstrap', container = sketchSummary, rownames = FALSE, options = optOutputSummary, colnames = namesOutputSummary  ) %>% formatCurrency(c('ingreso_total', 'ingreso_total_api', 'precio_unitario', 'precio_unitario_api', 'repercusion_media', 'repercusion_media_api'), '\U20AC', digits = 0, interval = 3, mark = "." ) %>% formatPercentage(c('percent', 'percent_api'), 2)  ) 
    
    
    #summaryTableWithout <- summaryTableWithout %>% arrange(tipo, repercusion_media)
    
    output$summaryM2Plot = renderPlot({
      ggplot(summaryTableWithout, aes(x = tipologia, y = gsub(',','.', comma(round(as.numeric(repercusion_media), 0)) ) , fill=tipo, group = tipo )) + geom_bar(width=0.7, stat = "identity", position = position_dodge(width=0.8)) + scale_fill_manual("legend", values = c("black", "blue")) + geom_text(aes(label = paste(gsub(',','.', comma(round(as.numeric(repercusion_media), 0)) ), "€")), position = position_dodge(0.9), vjust = -0.5) +
        xlab("Tipología") + ylab("Repercusión (€/m2)") + theme_minimal() + theme( panel.border = element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Repercusión €/m2 por tipología") + theme(
          plot.title = element_text(color="black", size=14, face="italic", hjust = 0.5)) 
    })
      
    output$summaryMeanPlot = renderPlot({
      ggplot(summaryTable, aes(x = tipologia, y = gsub(',','.', comma(round(as.numeric(precio_unitario), 0)) ), fill=tipo, group = tipo))+ geom_bar(width=0.7,stat = "identity", position = position_dodge(width=0.8)) + scale_fill_manual("legend", values = c("black", "blue")) + geom_text(aes(label = paste(gsub(',','.', comma(round(as.numeric(precio_unitario), 0)) ), "€")), position = position_dodge(0.9), vjust = -0.5) +
        xlab("Tipología") + ylab("Precio unitario (€)") + theme_minimal() + theme( panel.border = element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Precio medio por tipología") + theme(
          plot.title = element_text(color="black", size=14, face="italic", hjust = 0.5))
    })
    
    
    rsme = sqrt(sum((comparativeTable$diferencia)**2))
    compare_data <- data.frame(compareDate = Sys.time(), 
                               rsme = rsme, 
                               #url = url
                               id_sesion = session$token) %>% rename(date = compareDate)
    #write_df_as_table(compare_data, "comparacion_tarificacion", T)
    write_df_as_sqlserver_table(compare_data, "comparacion_tarificacion", T)
    
  })
  
  
  
  observeEvent(input$contents_cell_edit, {
    
    proxy = dataTableProxy('contents')
    
    info = input$contents_cell_edit
    
    i = info$row
    j = info$col = info$col + 1  # column index offset by 1
    v = info$value
    
    info$value <- as.numeric(info$value)
    if( !is.na(as.numeric(info$value)) ) v = as.numeric(info$value)
    
    values$finalTable[i, j] <<- DT::coerceValue(v, values$finalTable[i, j])
    replaceData(proxy, values$finalTable, resetPaging = FALSE, rownames = FALSE)
    
  })
  
  
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  observeEvent(input$validate_button, {
    isolate(validateCounter$count <- validateCounter$count + 1)
    selectedRow <- as.numeric(strsplit(input$validate_button, "_")[[1]][2])
    temp <- values$finalTable
    temp$tarifaValidada[selectedRow] <- temp$tarifa[selectedRow]
    values$finalTable <- temp
  })
  
  observeEvent(input$validateAll, {
    isolate(validateAllCounter$count <- validateAllCounter$count + 1)
    temp <- values$finalTable
    temp$tarifaValidada <- temp$tarifa
    values$finalTable <- temp
  })
  
  observeEvent( input$plano_button, {
    #isolate(validateCounter$count <- validateCounter$count + 1)
    selectedRow <- as.numeric(strsplit(input$plano_button, "_")[[1]][2])
    temp <- values$finalTable
    
    name = paste("PORTAL", temp$Portal[selectedRow], "PLANTA", temp$Planta[selectedRow], "PUERTA", temp$Puerta[selectedRow], sep = "_")
    name_ext = paste(values$namePromo, "/", name, ".pdf", sep = "")  
    
    showModal(   
      modalDialog(size = "l",
        #"height:600px; width:100%"
        div( tags$iframe(style="height:700px; width:100%", src = name_ext) ),
        footer = tagList(
          actionButton("okmodal", "Cerrar", class = "btn-primary")
        )
      )
    )
    
    
  })
  
  observeEvent(input$saveAll, {
    
    isolate(saveCounter$count <- saveCounter$count + 1)
    
    df_to_save <- values$finalTable
    df_to_save$validar <- NULL
    df_to_save$planos <- NULL
    df_to_save$saveDate <- Sys.time()
    df_to_save$id_sesion <- session$token
    #df_to_save$url <- url

    if(sum(is.na(df_to_save$tarifaValidada)) == 0){
      if( sum(df_to_save$tarifaValidada) < values$totalViviendas ){
        showModal(dataModalTarifaLow( ( values$totalViviendas - sum(df_to_save$tarifaValidada) ) ))
      }else{
        rsme_validada_api = sqrt(sum(( df_to_save$tarifaValidada - df_to_save$TarifaAPI  )**2))
        rsme_validada_prediccion = sqrt(sum(( df_to_save$tarifaValidada - df_to_save$tarifa  )**2))
        
        df_to_save$rsmeValAPI <- rsme_validada_api
        df_to_save$rsmeValPred <- rsme_validada_prediccion
        
        
        colnames(df_to_save)[which(names(df_to_save) == "Portal")] <- "portal"
        colnames(df_to_save)[which(names(df_to_save) == "Planta")] <- "planta"
        colnames(df_to_save)[which(names(df_to_save) == "Puerta")] <- "puerta"
        colnames(df_to_save)[which(names(df_to_save) == "Dormitorios")] <- "dormitorios"
        colnames(df_to_save)[which(names(df_to_save) == "T_S_U_Interior")] <- "t_s_u_interior"
        colnames(df_to_save)[which(names(df_to_save) == "T_S_U_Jardin")] <- "t_s_u_jardin"
        colnames(df_to_save)[which(names(df_to_save) == "Or_Salon")] <- "or_salon"
        colnames(df_to_save)[which(names(df_to_save) == "Or_Dormitorio")] <- "or_dormitorio"
        colnames(df_to_save)[which(names(df_to_save) == "precioMedioFinal_M2")] <- "precio_medio_final_m2"
        colnames(df_to_save)[which(names(df_to_save) == "diferencia_M2_Medio")] <- "diferencia_m2_medio"        
        colnames(df_to_save)[which(names(df_to_save) == "TarifaAPI")] <- "tarifa_api"
        colnames(df_to_save)[which(names(df_to_save) == "precioMedioM2_API")] <- "precio_medio_m2_api"        
        colnames(df_to_save)[which(names(df_to_save) == "tarifaValidada")] <- "tarifa_validada"  
        colnames(df_to_save)[which(names(df_to_save) == "rsmeValAPI")] <- "rsme_val_api"  
        colnames(df_to_save)[which(names(df_to_save) == "rsmeValPred")] <- "rsme_val_pred"  
        
        #df_to_save %>% rename( portal = Portal, planta = Planta, puerta = Puerta, dormitorios = Dormitorios, t_s_u_interior = T_S_U_Interior, t_s_u_terraza = T_S_U_Terraza, t_s_u_jardin = T_S_U_Jardin, or_salon = Or_Salon, or_dormitorio = Or_Dormitorio, precio_medio_final_m2 = precioMedioFinal_M2, diferencia_m2_medio = diferencia_M2_Medio, tarifa_api = TarifaAPI, precio_medio_m2_api = precioMedioM2_API, tarifa_validada = tarifaValidada, rsme_val_api = rsmeValAPI, rsme_val_pred = rsmeValPred )
        
        #write_df_as_table(df_to_save, "tarifa_final_tarificacion", T) 
        write_df_as_sqlserver_table(df_to_save, "tarifa_final_tarificacion", T)
        
        showModal(dataModalOk())
        values$downloadButton <- TRUE
      }

    }else{
      showModal(dataModal())
    }

  })
  
  
  # observeEvent( input$downloadTar, {
  #     
  #     df_to_save <- values$finalTable
  #     df_to_save$validar <- NULL
  #     df_to_save$planos <- NULL
  #     
  #     write.table(df_to_save, paste("tarifa_validada_",values$namePromo, ".csv" , sep = '' ), row.names = F, col.names = T, dec = '.', fileEncoding = 'UTF-8' )  
  #   
  #     downloadHandler(
  #       filename = paste("tarifa_validada_",values$namePromo, ".csv" , sep = '' ),
  #       content = write.table(df_to_save, paste("tarifa_validada_",values$namePromo, ".csv" , sep = '' ), row.names = F, col.names = T, dec = '.', fileEncoding = 'UTF-8' )
  #     )
  # })
  
  output$downloadTar <- downloadHandler(
      filename = function() {
        paste("tarifa_validada_", values$namePromo, ".csv" , sep = '' )
      },
      content = function(file) {
        write.table(take_df_to_save(), file, row.names = F, col.names = T, dec = '.', sep = ';', fileEncoding = 'UTF-8' )
      }
  )
  
  take_df_to_save <- reactive({
      df_to_save <- values$finalTable
      df_to_save$validar <- NULL
      df_to_save$planos <- NULL
      
      df_to_save 
  })
  
  
  output$downloadTube <- downloadHandler(
    filename = function() {
      paste("tubo_tarifa_", values$namePromo, ".csv" , sep = '' )
    },
    content = function(file) {
      write.table(take_df_to_save_tube(), file, row.names = F, col.names = T, dec = '.', sep = ';', fileEncoding = 'UTF-8' )
    }
  )
  
  
  observeEvent(input$sendTarifa, {  
  
    from <- "tdnh@neinorhomes.com"
    to <- c("inigo.lopez@neinorhomes.com")
    subject <- paste("Tarifa validada", values$namePromo, "- Alta Sistemas", "")
    msg <- paste("Correo automatico generado desde la herramienta de Tarificación por ", users_data$user, "")
    
    #
    nameFile <- paste("tubo_tarifa_", values$namePromo, ".csv" , sep = '' )
    write.table(take_df_to_save_tube(), nameFile, row.names = F, col.names = T, dec = '.', sep = ';', fileEncoding = 'UTF-8' )
    
    # Add an attached file
    attachmentName <- paste("./", nameFile, sep = '')
    
    send.mail(from = from,
              to = to,
              subject = subject,
              body = msg, 
              attach.files = attachmentName,
              authenticate = TRUE,
              smtp = list(host.name = "smtp.office365.com", port = 587,
                          user.name = "tdnh@neinorhomes.com", passwd = "N3in0r29!", tls = TRUE))
    file.remove(nameFile)
    showModal(dataModalSendOk())
    
  })
  
  
  
  take_df_to_save_tube <- reactive({
    df_to_tube <- values$finalTable
    df_to_tube$validar <- NULL
    df_to_tube$planos <- NULL
    df_to_tube$tarifa <- NULL
    df_to_tube$precioMedioFinal_M2 <- NULL
    df_to_tube$diferencia_M2_Medio <- NULL
    df_to_tube$tipo <- NULL
    df_to_tube$TarifaAPI <- NULL
    df_to_tube$precioMedioM2_API <- NULL
    df_to_tube$diferencia <- NULL
    
    
    df_to_tube$proyecto <- values$proyecto
    df_to_tube$promocion <- trimws(values$promocion)
    df_to_tube$tipo <- "VI"
    
    colnames(df_to_tube)[which(names(df_to_tube) == "Portal")] <- "clave_1"  
    colnames(df_to_tube)[which(names(df_to_tube) == "Planta")] <- "clave_2"  
    colnames(df_to_tube)[which(names(df_to_tube) == "Puerta")] <- "clave_3"  
    
    df_to_tube$clave_4 <- ""
    
    colnames(df_to_tube)[which(names(df_to_tube) == "T_S_U_Interior")] <- "m2utiles"
    
    df_to_tube$T_S_U_Terraza <- NULL
    df_to_tube$T_S_U_Jardin <- NULL
    df_to_tube$Or_Salon <- NULL
    df_to_tube$Or_Dormitorio <- NULL
    df_to_tube$diferencia <- NULL
    
    colnames(df_to_tube)[which(names(df_to_tube) == "tarifaValidada")] <- "imp.total"
    
    
    df_to_tube <- df_to_tube[c("proyecto", "promocion", "tipo", "clave_1", "clave_2", "clave_3", "clave_4", "m2utiles", "imp.total")]

        
    df_to_tube 
  })
  
  
  
  output$pdfviewer <- renderText({
    return(paste('<iframe style="height:100%; width:600px" src="PORTAL_A1_PLANTA_0_PUERTA_B.pdf"></iframe>', sep = ""))
  })

  
  
  observeEvent( input$viewPDF, {
    if(!values$showpdf){
      values$showpdf <- TRUE
    }else{
      values$showpdf <- FALSE
    }
    
    #return(paste('<iframe style="height:600px; width:100%" src="plano_solagua.pdf"></iframe>', sep = ""))
    #tags$iframe(style="height:100%; width:100%", src="plano_solagua.pdf")    
  })
  
  
  observeEvent( input$okmodal ,{
    removeModal()
  })
  
  # observeEvent(input$change_button, {
  #   selectedRow <- as.numeric(strsplit(input$change_button, "_")[[1]][2])
  #   temp <- values$finalTable
  #   temp$tarifaValidada[selectedRow] <- temp$tarifaValidada[selectedRow]
  #   values$finalTable <- temp
  # })
  
  
  # getTarifa <- function(prices, orDor, orSal, planta, dormitorios, superficie, jardin, terraza_in, terraza_out, salon, dormitorio ){
  #   getCoeff(as.factor(dormitorio[orDor]), as.numeric(input$dormOrientation)) * getCoeff(as.factor(salon[orSal]), as.numeric(input$salonOrientation)) * prices[ as.factor(planta), paste("V",dormitorios, sep = "") ] * ( superficie + (gardenPercent * jardin / 100) + ( terracePercent * (terraza_in + terraza_out) / 100 ) )
  # }
  
  getCoeff <- function(value, importance){
    if( value == 'B'){
      goodImportance[importance+1]
    }else if( value == 'M' ){
      badImportance[importance+1]
    }else{
      1
    }
  }
  
  
  getTarifa <- function(x){
    arg1 <- getCoeff(dormitorio[as.character(x['Or_Dormitorio'])], as.numeric(input$dormOrientation))
    arg2 <- getCoeff(salon[as.character(x['Or_Salon'])], as.numeric(input$salonOrientation))
    arg3 <- prices_m2[ x['Planta'], paste("V", x['Dormitorios'], sep = "") ] 
    arg4 <- x['T_S_U_Interior'] + ( gardenPercent * x['T_S_U_Jardin'] / 100) + ( terracePercent * x['T_S_U_Terraza'] / 100 ) 
    return (arg1 * arg2 * arg3 * arg4) 
  }
  
  
  getPrice <- function(planta, dormitorios){
    price <- values$prices_m2[as.character(planta), paste("V", dormitorios, sep = "")]
    return(price) 
  }
  
  
  # output$mymap <- renderLeaflet({
  #   leaflet() %>% 
  #     setView(lng = -3.780736, lat = 40.343159, zoom = 3) %>% 
  #     addTiles() %>% addGeoJSON(leganes_geojson)
  #   
  # })

  
  
  
  
})

