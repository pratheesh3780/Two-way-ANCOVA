library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(agricolae)
library(RColorBrewer)
library(ggpubr)


ui <- fluidPage(

  ######## BACKGROUND
  setBackgroundColor(
    color = c("#fffacc", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),

  ########
  titlePanel(tags$div(tags$b('Two-way ANCOVA',style="color:#000000"))),

  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    uiOutput('var'),
    tags$br(),
    conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
    tags$br(),
    h5(
      tags$div(
        tags$br(),
        "Developed by:",
        tags$br(),
        tags$b("Dr.Pratheesh P. Gopinath"),
        tags$br(),
        tags$b("Assistant Professor,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$br(),
        "post your queries at: pratheesh.pg@kau.in"
        ,style="color:#343aeb")
    )
  )
  , mainPanel(

    tabsetPanel(type = "tab",
                tabPanel("Analysis.Results",
                         conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                                          tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
                         tableOutput('trtmeans'),
                         tags$style(  type="text/css", "#trtmeans th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#trtmeans td {border: medium solid #000000;text-align:center}"),

                         tableOutput('aovSummary'),
                         tags$style(  type="text/css", "#aovSummary th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#aovSummary td {border: medium solid #000000;text-align:center}"),

                         tableOutput('SEM'),
                         tags$style(  type="text/css", "#SEM th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#SEM td {border: medium solid #000000;text-align:center}"),

                         h3(""),
                         htmlOutput('text'),
                         h3(""),

                         uiOutput('inference'),

                         tableOutput('multi'),
                         tags$style(  type="text/css", "#multi th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#multi td {border: medium solid #000000;text-align:center}"),


                         tableOutput('group'),
                         tags$style(  type="text/css", "#group th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#group td {border: medium solid #000000;text-align:center}"),

                         uiOutput('var1'),
                         uiOutput('start_note'),
                         tags$br(),
                         uiOutput('data_set'),# data set to test,
                         tags$br(),
                         uiOutput('start_note2'),
                         tags$br(),
                         tags$br()

                ),
                tabPanel("Plots & Graphs",
                         uiOutput('varplot'),
                         uiOutput('varboxplot'),
                         uiOutput('varbarchart'),
                         uiOutput('varbcg'),
                         tags$br(),
                         tags$br(),
                         uiOutput('start_note3'),
                         plotOutput('boxplot')%>% withSpinner(color="#0dc5c1"),
                         tags$br(),
                         uiOutput('plot_note3'),
                         tags$br(),
                         uiOutput('image_down'),#image to download
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br()
                )
    )
  )
)
######################################password
# Wrap your UI with secure_app
ui <- secure_app(ui)
##################################
