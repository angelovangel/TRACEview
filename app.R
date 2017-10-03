## TRACE-view-app.R ##

# This app reads in chromatogram files from any chromatography system (exported as csv or similar)
# and plots the traces. The app allows shifting of the traces in the y-axis.
# Tested with chromatograms from Chromeleon

# shinydashboard version
# ui and server in one file
#***********

library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(rbokeh)
library(shiny)
library(shinydashboard)
#library(clusterSim)

header <- dashboardHeader(title = "TRACE-view")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Load data", tabName = "data", icon = icon("database")),
    menuItem("TRACE", tabName = "trace", icon = icon("line-chart")),
    menuItem("simulate TLC", tabName = "simTLC", icon = icon("list-ul")),
    numericInput("trimNamesRight", "Keep last X characters from right", 50, min = 4, max = 50, step = 1)
    
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data",
            fluidRow(
              box(width = 12, h4("About this app and how to use it"),
                              h5("Load the exported text files and adjust the file input settings until the data is read into the app. **The default settings are for Chromeleon.**")),
              box(width = 12, valueBoxOutput("filesCount", width = 3), valueBoxOutput("rowsCount", width = 9)),
              box(width = 3,
                  title = "Read chromatogram files", status = "primary",
                  fileInput('files', 'Choose many files', multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")), # here the file input
                  hr(),
                  checkboxInput('header', 'Header', FALSE),
                  radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             '\t'),
                  radioButtons('decmark', 'Decimal mark',
                             c('comma'=',',
                               'point'="."),
                             ','),
                  numericInput("trimlines", "Trim lines", 43, min = 0, max = 100, step = 1) # trim 43 for Chromeleon
                  
                  
                  ),
                
              box(width = 9, title = "Preview data", status = "primary", tableOutput("data"))
              )
          ),
    tabItem(tabName = "trace",
            fluidRow(box(width = 12, h5("Move the slider to shift traces. The plot is interactive, so you can zoom, select x- and y-range etc.!"),
                selectizeInput("selectSamples", "Select samples to plot", choices =NULL, multiple = TRUE),
                sliderInput("shift", "y-shift", min = 0, max = 250, step = 1, value = 0)
                )),
            fluidRow(rbokehOutput("plot"))#, width = 1100, height = 800))
            
                ),
    tabItem(tabName = "simTLC",
              column(width = 2,
                      radioButtons("scalePerRow", "Scale per row?", choices = c("yes", "no"), selected = "no")),
            column(width = 2,
                   radioButtons("sigTrans", "Signal transformation", choices = 
                                  c("No" = "identity",
                                    "Sqrt" = "sqrt",
                                    "Log" = "log"))
                                ),
              column(width = 4,
                      sliderInput("bandsize", "Band size", min = 1, max = 20, step = 1, value = 4)),
              column(width = 4, sliderInput("xtrim", "Trim X", min = 0, max = 50, step = 0.5, value = c(0,40))),
            
                      plotOutput("tlc")
                      
                      )
          
      )
  ) # end body

ui <- (dashboardPage(header, sidebar, body, skin = "black"))


server <- function(input, output, session) {
observe({

  #***
  # define read function
  read_chrom <- function(x,y) {
    read_delim(x, delim = input$sep, col_names = input$header, skip = input$trimlines, 
               locale = locale(decimal_mark = input$decmark), trim_ws = TRUE, escape_double = FALSE, na = "NA") %>%
      mutate(filename = y)# here some work needed to simplify naming
  }  
  #**********************  
# read files to df
  
  inFiles <- input$files$datapath # critical, see example script readmultiplefiles-shiny.R or https://itsalocke.com/upload-multiple-files-in-shiny-and-consolidate-into-a-dataset/
    if(is.null(inFiles))
    {return(NULL)}  
  inNames <- input$files$name
    if(is.null(inNames))
    {return(NULL)}  
  
  
  df0 <- reactive({
    
      map2(inFiles, inNames, read_chrom) %>%
        map_df(bind_rows)
      })
  
  df <- reactive({
    tmp <- df0()        # ugly but works, testing if first column read is numeric
    validate(try(need(is.numeric(tmp$X1), 
                      "It seems your file(s) contain non-numeric characters, trim more lines using the tool on the left.
The app needs the first column to be numerics only!")))
    req(input$trimNamesRight > 2)  #this req is needed to avoid crash if set to 0
    df0() %>% rename(time = X1, sig = X3) %>%
            mutate(sample = str_trunc(filename, input$trimNamesRight, "left", ellipsis=""))
    
  })
  
  offset <- input$shift
  
  df2 <- function() {
    df() %>% dplyr::select(time, sig, sample) %>%
             filter(sample %in% input$selectSamples) %>% #sample filtering here
             mutate(time = round(time,4), 
                    sig2 = as.numeric(as.factor(sample)) * offset + sig) %>%  # shift is generated here
             dplyr::select(time, sample, sig2, sig)
             
            }
  
  # populate samples list
  sampleslist <- as.list(df() %>% distinct(sample))
  updateSelectizeInput(session, "selectSamples", 
                       choices = sampleslist$sample, 
                       selected = sampleslist$sample, server = TRUE)
  #***************
 
   #data table and counts
  output$data <- renderTable(df() %>% 
                               group_by(filename, sample) %>% 
                               summarise(datapoints = n())
                            )
                                               
                                              
  output$filesCount <- renderValueBox(valueBox(value = length(inFiles), "samples were read", color = "light-blue")) 
  output$rowsCount <- renderValueBox(valueBox(value = nrow(df()), "datapoints total", color = "light-blue"))
  
  
  

# plotting***************


 
  
output$plot <- renderRbokeh({
  figure(width = 2000, height = 800) %>% 
      ly_lines(x = time, y = sig2, color = sample, data = df2(), width = 2, alpha = 0.8)
    
      })

range01 <- function(x, ...){(x-min(x, ...))/(max(x, ...)-min(x, ...))}
if(input$scalePerRow == "no") df3 <- reactive({df2() %>% filter(time >= input$xtrim[1] & time <= input$xtrim[2])}) else
       df3 <- reactive({df2()%>% filter(time >= input$xtrim[1] & time <= input$xtrim[2]) %>% 
              group_by(sample) %>% mutate(sig = range01(sig, na.rm = TRUE))    
         })
      

output$tlc <- renderPlot({
  df3()  %>% 
    ggplot(aes(sample, time)) + 
    geom_point(aes(color= sig), size= input$bandsize, stroke = 0.5) + 
    scale_color_gradient(trans = input$sigTrans, low = "white", high = "black") + 
    
    coord_flip() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_rect(fill = "white"),
          legend.position = "none")
      
      }, height = function() {0.4 * session$clientData$output_tlc_width})
  

}) # end observe
  
} # server end


shinyApp(ui, server)
