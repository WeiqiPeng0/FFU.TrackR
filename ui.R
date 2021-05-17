# library(stats)
library(tidyverse) # this includes dplyr for data wranling, ggplot2 for plotting, and tidyr for reshaping data
library(shiny)
library(plotrix) # for standard error function
library(shinythemes)
library(gridExtra)
library(colourpicker)
library(shinyjs) 
library(shinydashboard) # install this for layour skeleton
library(shinydashboardPlus)
library(shinyWidgets)
library(egg) # for aligning graph positions
library(ggpubr) # same as above
library(hrbrthemes)
library(ggbeeswarm)   # for the beeswarm plot
# library(foreign)
# library(rhandsontable)


# In order to use extendshinyJS, you may also need to run install.packages("V8")


jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


ui <- dashboardPage(

  skin = 'purple', # Set default skin color here
  
  dashboardHeader(title='FFU.TrackR'),  # set header text
  
  
  ############# Left Sidebar -- Interactive ################
  dashboardSidebar(
    
    # tags$style(type='text/css', ".selectize-input { font-size: 13px; line-height: 10px;
    #             height: 30px;width: 190px;}
    #              .selectize-dropdown { font-size: 13px; line-height: 10px; }
    #              .form-group, .selectize-control {margin-bottom:3px;max-height: 90px !important;}
    #              .box-body {
    #       padding-bottom: 2px;
    #   }"),

    #selector for graph type
    selectInput("graph", "Choose a type of graph:",
                choice=c("Default","Bar","Density", "Swarm"),
                selected = "Default",
                multiple = FALSE),
    uiOutput("loc"),#parameter selector holder
    prettyCheckbox(
      inputId = "isFilter",
      label = "Add filter(s)",
      thick = TRUE,
      icon = icon("check"),
      shape = "curve",
      animation = "pulse",
      status = "info",
      inline = TRUE
    ),
    uiOutput("filter"),
    uiOutput("filterButton"),
    prettyCheckbox(
      inputId = "set",
      label = "Set Treatment",
      thick = TRUE,
      icon = icon("check"),
      shape = "curve",
      animation = "pulse",
      status = "info",
      inline = TRUE
    ),
    uiOutput("treatvar"),
    tags$div(
      id= "treatdiv"
    ),
    uiOutput("treat"),
    #actionButton("add","Add"),
    #actionButton("delete","Delete"),
    br(),
    actionButton('plot_button','Plot',style='background-color: #FFF; /* White */;
                             padding:5px; font-size:110%; width:100px; align:centering; border-radius: 12px;')

    # actionBttn(
    #   inputId = "plot_button",
    #   label = "Plot", 
    #   style = "gradient",
    #   color = "danger",
    #   icon = icon("check")
    # )
  ), # end of sidebar
  
  ### The right tab panel #########
  # 1. for theme selector
  controlbar = dashboardControlbar(collapsed = TRUE, 
                                   
                                   title = HTML('<h4> Select a Theme </h4>'),
                                   skin='dark',
                                   skinSelector(),
                                   # HTML('<h4> Treatment Color </h4>'),
                                   br(),
                                   tags$div(
                                     id= "colordiv"
                                   )
                                     
                                     # controlbarItem(
                                     #   "setting 2",
                                     #   "Welcome to tab 2"
                                     # )
                                   ),
                              
                                   
  

  ############### The body  ##################
  dashboardBody(

    shinyjs:::useShinyjs(),
    extendShinyjs(text = jscode, functions = c("winprint")),

    # add css to the header
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      '))),

    # Change tab coloar when active
    tags$style(HTML("
        .tabbable > .nav > li[class=active] > a {
           background-color: #9A41B6;
           color: #FFF;
        }")),

    chooseSliderSkin("Shiny", color = "#4A41B6"),
    
    #tabset for plot, summary and table
    tabsetPanel(
      
      
      tabPanel("Plot", 
               br(),
               uiOutput('colors'),
               plotOutput("plot0",inline=TRUE),
               fluidRow(
                 tags$div(
                   id= "plotdiv"
                 )
               ),
 
               # box(
               #   title = "Plot",
               #   solidHeader = FALSE,
               #   collapsible = TRUE,
               #   plotOutput(
               #     outputId = 'plot',
               #     width = "100%",
               #     height = "200px",
               #     inline = FALSE
               #   ),
               # ),
               #file upload
               box(
                 title = "Data",
                 id = 'data',
                 width = 6,
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 closable = TRUE,
                 fileInput("file1", "Import a File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),

                 radioButtons(inputId='fileFormat', label='select the file type', choices=list('png','jpeg' ,'pdf')),
                 textInput('file_name:','file name','default'),
                 br(),
                 footer = fluidRow(
                   width = 8,
                   column (
                     width = 4,
                     downloadButton('downloadData', 'Data')
                   ),
                   column (
                     width = 4,
                     downloadButton('downloadPlot', 'Plot ')
                   )
                 )
                 
                 
               ),
               
               box(
                 title = "Plot Setting",
                 id = "mybox",
                 collapsible = TRUE,
                 # fluidRow(
                 #   width = 8,
                 #   column (
                 #     width = 6,
                 #     knobInput(
                 #       inputId = "plot_width",
                 #       label = "width:",
                 #       value = 570,
                 #       min = 200,
                 #       max=700,
                 #       height = 100,
                 #       width = 100,
                 #       displayPrevious = TRUE,
                 #       lineCap = "round",
                 #       fgColor = "#428BCA",
                 #       inputColor = "#428BCA"
                 #     )
                 #   ),
                 #   column (
                 #     width = 6,
                 #     knobInput(
                 #       inputId = "plot_height",
                 #       label = "height:",
                 #       value = 350,
                 #       min = 50,
                 #       max = 1000,
                 #       height = 100,
                 #       width = 100,
                 #       displayPrevious = TRUE,
                 #       lineCap = "round",
                 #       fgColor = "#428BCA",
                 #       inputColor = "#ff8BCA"
                 #     )
                 #   ),
                 #  br(), br(), br(), br(),
                 
                 fluidRow(
                   width = 8,
                   column (
                     width = 6,
                     textInput('xlabel','x label', '')
                   ),
                   column (
                     width = 6,
                     textInput('ylabel', 'y label', '')
                   )
                 ),
                 
                
                 textInput('title', 'title', ''),
                 textInput('treatNames','treat names (separate by ;)', '' ),
          
                 sliderInput("plot_width", "Plot Width:", min = 200, max =700 , value = 570),
                 sliderInput("plot_height", "Plot height:", min = 50, max =1000 , value = 350)
                 
    
               )
               
               
      ),
      
      # tab that shows and adjusts filters
      tabPanel("Filter", br(),

                uiOutput("filterTab"),
                tags$div(
                  id= "boxdiv"
                )

               
      ),
      
      
      tabPanel("Stats", 
               HTML("<h4> All Samples: </h4>"),
               DT::dataTableOutput("stats"),
               # HTML("<h4> Stats End </h4>"),
               br(),
               uiOutput("treatCheckbox"),
               DT::dataTableOutput("groupStats"),

               tags$div(
                 id= "statshere"
               )
               # downloadButton('save_t', 'Save')
               
               ),
      tabPanel("Table",
        
               pickerInput(
                 inputId = "table_picker",
                 label = "", 
                 choices =  c("None", "Original", 
                              "Filtered", "Selected Variables", "Filtered & Selected"),
                 selected = "original",
                 options = list(
                   style = "btn-primary")
               ),
               br(),
               DT::dataTableOutput("table"),
               
               # rHandsontableOutput('table'),
               br(),
              
               downloadButton('save_t', 'Save')
               ),
      #tabPanel("Summary", tableOutput("summary"),downloadButton('save_s', 'Save')),
      
      tabPanel("Settings",
          br(),     
          fluidRow(
          box(
          title = "Text Size",
          sliderInput("font_size", "Font Size (general):", min = 80, max = 110 , value = 100),
          sliderInput("label_size", "Label Size:", min = 0, max = 30 , value = 17),
          sliderInput("title_size", "Title Size:", min = 0, max = 30 , value = 20),
          sliderInput("axis_size", "Axis text Size:", min = 0, max = 30 , value = 15)
          ),
          # box(
          # title = "Plot Size",
          # sliderInput("plot_width", "Plot Width:", min = 200, max =700 , value = 570),
          # sliderInput("plot_height", "Plot height:", min = 50, max =1000 , value = 350)
          # )
      ))
    ),
    
    
  ),#end of body
  dashboardFooter(left = "Contact: bcroker@health.ucsd.edu", right = actionButton('test','Test',style='background-color: #FF5555; /* White */;
                             padding:5px; ')),
)
