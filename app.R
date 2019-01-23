library(shiny)
library(argonR)
library(argonDash)
library(shinycssloaders)
library(plotly)
library(DT)

setwd("~/School/Atelier/")
source("atelier.R")

options(shiny.trace = F)

shiny::shinyApp(
  
  # UI ----
  ui = argonDashPage(
    title = "Sensory Data Analysis Dashboard",
    author = "Khalil",
    description = "Dash Board",
    
    # Sidebar -----
    
    sidebar = argonDashSidebar(
      skin = "light",
      background = "white",
      size = "md",
      side = "left",
      id = "sidebar",
      brand_logo = "logo.png",
      dropdownMenus =
        argonSidebarHeader(title = "Main Menu"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "data",
          icon = "single-copy-04",
          icon_color = "info",
          "Upload Datasets"
        ),
        argonSidebarItem(
          tabName = "eda",
          icon = "chart-pie-35",
          icon_color = "success",
          "Exploratory Data Analysis"
        ),
        argonSidebarItem(
          tabName = "pca",
          icon = "chart-bar-32",
          icon_color = "warning",
          "Principle Component Analysis"
        )
      )
    ),
    
    # Navbar ----
    
    navbar = NULL,
    
    # Header ----
    
    
    header = argonDashHeader(
      gradient = TRUE,
      color = "primary",
      separator = TRUE,
      separator_color = "secondary"
    ),
    
    # Body ----
    
    body = argonDashBody(argonTabItems(
      
      ## Datasets ----
      argonTabItem(
        tabName = "data",
        argonTabSet(
          id = "tab-1",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = NULL,
          
          ### Hedo dataset ----
          
          argonTab(
            tabName = "Hedo dataset",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 3,
                fileInput("fileHedo", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                checkboxInput("headerHedo", "Header", TRUE),
                radioButtons("sepHedo", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ";"),
                radioButtons("quoteHedo", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                radioButtons("dispHedo", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")
                
              ),
              argonColumn(
                center = T,
                width = 9,
                div(style = 'overflow-x: scroll',
                    dataTableOutput("contentsHedo")%>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "400px"))
              )
            )
          ),
          
          ### Sensory dataset ----
          argonTab(
            tabName = "Sensory dataset",
            active = FALSE,
            argonRow(
              argonColumn(
                width = 3,
                fileInput("fileSenso", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                checkboxInput("headerSenso", "Header", TRUE),
                radioButtons("sepSenso", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                radioButtons("quoteSenso", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                radioButtons("dispSenso", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")
                
              ),
              argonColumn(
                center = T,
                width = 9,
                div(style = 'overflow-x: scroll',
                    dataTableOutput("contentsSenso")%>% 
                      withSpinner(color="#5e72e4", type=7, proxy.height = "400px"))
              )
            )
          )
          
        )
        
      ),
      
      ## EDA ----
      argonTabItem(
        tabName = "eda",
        argonTabSet(
          id = "tab-2",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = NULL,
          
          ### Boxplot ----
          
          argonTab(
            tabName = "Boxplot",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 3,
                selectInput(
                  inputId = "boxplotVar",
                  label = "Variable: ",
                  choices = colnames(df.senso)[4:dim(df.senso)[2]]
                ),
                selectInput(
                  inputId = "boxplotFactor",
                  label = "Factor:",
                  choices = colnames(df.senso)[1:3]
                )
              ),
              argonColumn(
                center = T,
                width = 9,
                plotlyOutput("boxPlot", height = "100%")%>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "400px")
              )
            )
          ),
          
          ### ANOVA ----
          argonTab(
            tabName = "ANOVA",
            active = FALSE,
            argonRow(
              argonColumn(
                width = 3,
                selectInput(
                  inputId = "anovaVar",
                  label = "Variable: ",
                  choices = colnames(df.senso)[4:dim(df.senso)[2]]
                ),
                selectInput(
                  inputId = "anovaFactors",
                  label = "Factor:",
                  choices = colnames(df.senso)[1:3],
                  multiple = TRUE
                )
              ),
              argonColumn(
                center = T,
                width = 9,
                verbatimTextOutput("anova")%>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "400px")
              )
            )
          )
          
        )
        
      ),
      
      
      ## PCA ----
      
      argonTabItem(
        tabName = "pca",
        argonTabSet(
          id = "tab-3",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = NULL,
          
          ### Scree plot ----
          
          argonTab(
            tabName = "Scree plot",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 3,
                selectInput(
                  "choice",
                  label = "Y Axis",
                  choices = c("variance", "eigenvalue"),
                  selected = "variance"
                )
              ),
              argonColumn(
                center = T,
                width = 9,
                plotOutput("screePlot", height = "100%")%>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "600px")
              )
            )
          ),
          
          ### Variable plot ----
          
          argonTab(
            tabName = "Variables",
            active = FALSE,
            argonRow(
              argonColumn(
                width = 3,
                selectInput(
                  "axis_X",
                  label = "X Axis Dimension",
                  choices = seq(5),
                  selected = 1
                ),
                
                uiOutput("secondSelector"),
                
                
                sliderInput(
                  "n_cos2",
                  label = "cos2:",
                  min = 0.1,
                  max = 0.9,
                  value = 0.5,
                  step = 0.1
                )
              ),
              argonColumn(
                center = T,
                width = 9,
                plotOutput("varPlot", height = "100%") %>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "600px")
              )
            )
          ),
          
          ### Bi-plot ----
          
          argonTab(
            tabName = "Biplot",
            active = FALSE,
            argonRow(
              argonColumn(
                width = 3,
                selectInput(
                  "axis_X2",
                  label = "X Axis Dimension",
                  choices = seq(5),
                  selected = 1
                ),
                
                uiOutput("secondSelector2")
                
              ),
              argonColumn(
                center = T,
                width = 9,
                plotOutput("biPlot", height = "100%")%>% 
                  withSpinner(color="#5e72e4", type=7, proxy.height = "600px")
              )
            )
          )
        )
        
      )
    )),
    
    # Footer ----
    
    footer = NULL
  ),
  
  
  
  # Server ----
  server = function(input, output) {
    
    ## Dataset Hedo ----
    output$contentsHedo <- renderDataTable({
      
      req(input$fileHedo)
      
      df <- read.csv(input$fileHedo$datapath,
                     header = input$headerHedo,
                     sep = input$sepHedo,
                     quote = input$quoteHedo)
      
      if(input$dispHedo == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
    ## Dataset Senso ----
    output$contentsSenso <- renderDataTable({
      req(input$fileSenso)
      
      df <- read.csv(input$fileSenso$datapath,
                     header = input$headerSenso,
                     sep = input$sepSenso,
                     quote = input$quoteSenso)
      
      if(input$dispSenso == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
    ## ANOVA ----
    
    anovaFactors = reactive({
      req(input$anovaFactors)
    })
    
    output$anova = renderPrint({
      summary(aov(as.formula(paste(input$anovaVar," ~ ",paste(anovaFactors(),collapse="*"))),data=df.senso))
    })
    
    ## Boxplot ----
    
    output$boxPlot <-  renderPlotly({
      plot_ly(data=df.senso,
              x=df.senso[[input$boxplotVar]],
              color=df.senso[[input$boxplotFactor]],
              colors="RdYlBu",
              type="box")
    })
    
    ## Scree plot ----
    
    output$screePlot <- renderPlot({
      fviz_screeplot(obj.pca, choice = input$choice)
    }, height = 600, width = 600)
    
    
    ## Variable plot ----
    
    output$secondSelector = renderUI(selectInput(
      "axis_Y",
      label = "Y Axis Dimension",
      choices = seq(5)[which(seq(5) != as.numeric(input$axis_X))],
      selected = 2
    ))
    
    Y_axis <- reactive({
      req(input$axis_Y)
    })
    
    output$varPlot =
      renderPlot({
        fviz_pca_var(obj.pca,
                     col.var = "cos2",
                     axes = c(as.numeric(input$axis_X), as.numeric(Y_axis()))) +
          scale_color_gradient2(
            low = "white",
            mid = "blue",
            high = "red",
            midpoint = as.numeric(input$n_cos2),
            space = "Lab"
          ) + theme_light()
      }, height = 600, width = 600)
    
    ## Bi-Plot ----
    
    output$secondSelector2 = renderUI(selectInput(
      "axis_Y2",
      label = "Y Axis Dimension",
      choices = seq(5)[which(seq(5) != as.numeric(input$axis_X2))],
      selected = 2
    ))
    
    
    Y_axis2 <- reactive({
      req(input$axis_Y2)
    })
    
    
    output$biPlot =    renderPlot({
      fviz_pca_biplot(
        obj.pca,
        repel = T,
        alpha.var = "contrib",
        col.var = "cos2",
        col.ind = "#f5365c",
        axes = c(as.numeric(input$axis_X2), as.numeric(Y_axis2()))
      ) + theme_light()
    }, height = 600, width = 600)
    
  }
  
)
