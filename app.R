library(shiny)
library(argonR)
library(argonDash)
library(shinycssloaders)
library(plotly)
library(DT)
library(doBy)
library(factoextra)


setwd("~/School/Atelier/")
source("functions.R")

options(shiny.trace = F)

shiny::shinyApp(
  # UI ----
  ui = argonDashPage(
    title = "Sensory Data Analysis Dashboard",
    author = "Khalil",
    description = "Dash Board",
    
    # Sidebar ----
    
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
        ),
        argonSidebarItem(
          tabName = "pred",
          icon = "chart-bar-32",
          icon_color = "warning",
          "Score Prediction Map"
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
    
    body = argonDashBody(
      ## CSS ----
      tags$head(tags$style(
        HTML(".control-label {margin-bottom: 1.5rem;} .progress {height: 20px;} .btn{padding:0.7rem 1.25rem;} .input-group .form-control:not(:first-child){padding-left:10px;}")
      )),
      argonTabItems(
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
            tabName = "Hedonic dataset",
            active = TRUE,
            
            argonRow(
              argonColumn(
                width = 3,
                fileInput(
                  "fileHedo",
                  "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                ),
                argonRow(
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "sepHedo",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = ";"
                    )
                  ),
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "quoteHedo",
                      "Quote",
                      choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                      ),
                      selected = '"'
                    )
                  )
                ),
                
                argonRow(
                  argonColumn(
                    width = 6,
                    checkboxInput("headerHedo", "Header", TRUE)
                  ),
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "dispHedo",
                      "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"
                    )
                  )
                )
                
              ),
              argonColumn(
                center = T,
                width = 9,
                div(
                  style = 'overflow-x: scroll',
                  dataTableOutput("contentsHedo") %>%
                    withSpinner(
                      color = "#5e72e4",
                      type = 7,
                      proxy.height = "400px"
                    )
                )
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
                fileInput(
                  "fileSenso",
                  "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                ),
                argonRow(
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "sepSenso",
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "quoteSenso",
                      "Quote",
                      choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                      ),
                      selected = '"'
                    )
                  )
                ),
                
                argonRow(
                  argonColumn(width = 6,
                              checkboxInput("headerSenso", "Header", TRUE)),
                  argonColumn(
                    width = 6,
                    radioButtons(
                      "dispSenso",
                      "Display",
                      choices = c(Head = "head",
                                  All = "all"),
                      selected = "head"
                    )
                  )
                ),
                uiOutput("selectSensoSession"),
                uiOutput("selectSensoJudge"),
                uiOutput("selectSensoProduct")
              ),
              argonColumn(
                center = T,
                width = 9,
                div(
                  style = 'overflow-x: scroll',
                  dataTableOutput("contentsSenso") %>%
                    withSpinner(
                      color = "#5e72e4",
                      type = 7,
                      proxy.height = "400px"
                    )
                )
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
                uiOutput("selectBoxplotVar"),
                uiOutput("selectBoxplotFactor")
              ),
              argonColumn(
                center = T,
                width = 9,
                plotlyOutput("boxPlot", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  )
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
                uiOutput("selectAnovaVar"),
                uiOutput("selectAnovaFactors")
              ),
              argonColumn(
                center = T,
                width = 9,
                verbatimTextOutput("anova") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  )
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
                plotOutput("screePlot", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "600px"
                  )
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
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "600px"
                  )
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
                plotOutput("biPlot", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "600px"
                  )
              )
            )
          )
          
        )
        
      ),
      ## Pred ----
      argonTabItem(
        tabName = "pred",
        argonTabSet(
          id = "tab-4",
          card_wrapper = TRUE,
          horizontal = TRUE,
          circle = FALSE,
          size = "sm",
          width = 12,
          iconList = NULL,
          argonTab(
            tabName = "Score Prediction Map",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 2,
                selectInput(
                  "modelFormula",
                  label = "Formula",
                  choices = c("Vector", "Circular", "Elliptic","Quadratic"),
                  selected = "Quadratic"
                )
              ),
              argonColumn(
                width = 10,
                center=T,
                plotOutput("mapPlot", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "600px"
                  )
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
    
    df.hedoForDisplay = reactive({
      req(input$fileHedo)
      df <- read.csv(
        input$fileHedo$datapath,
        header = input$headerHedo,
        sep = input$sepHedo,
        quote = input$quoteHedo,
        row.names = 1
      )
      if (input$dispHedo == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
    })
    
    df.hedo = reactive({
      req(input$fileHedo)
      df <- read.csv(
        input$fileHedo$datapath,
        header = input$headerHedo,
        sep = input$sepHedo,
        quote = input$quoteHedo,
        row.names = 1
      )
      return(df)
    })
    
    ## Display Dataset Hedo ----
    output$contentsHedo <- renderDataTable({
      df.hedoForDisplay()
    }, options = list(processing = FALSE))
    
    ## Dataset Senso ----
    
    output$selectSensoSession = renderUI(
      selectInput(
        inputId = "sensoSession",
        label = "Session:",
        choices = colnames(df.sensoForDisplay()),
        selected = colnames(df.sensoForDisplay())[1]
      )
    )
    
    output$selectSensoJudge = renderUI(
      selectInput(
        inputId = "sensoJudge",
        label = "Judge:",
        choices = colnames(df.sensoForDisplay()),
        selected = colnames(df.sensoForDisplay())[2]
      )
    )
    
    output$selectSensoProduct = renderUI(
      selectInput(
        inputId = "sensoProduct",
        label = "Product:",
        choices = colnames(df.sensoForDisplay()),
        selected = colnames(df.sensoForDisplay())[3]
      )
    )
    
    df.sensoForDisplay = reactive({
      req(input$fileSenso)
      df <- read.csv(
        input$fileSenso$datapath,
        header = input$headerSenso,
        sep = input$sepSenso,
        quote = input$quoteSenso
      )
      if (input$dispSenso == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
    })
    
    df.senso = reactive(({
      req(input$sensoSession)
      req(input$sensoJudge)
      req(input$sensoProduct)
      df = read.csv(
        input$fileSenso$datapath,
        header = input$headerSenso,
        sep = input$sepSenso,
        quote = input$quoteSenso
      )
      df[[input$sensoSession]] = as.factor(df[[input$sensoSession]])
      df[[input$sensoJudge]] = as.factor(df[[input$sensoJudge]])
      df[[input$sensoProduct]] = as.factor(df[[input$sensoProduct]])
      return(df)
    }))
    
    ## Display Dataset Senso ----
    output$contentsSenso <- renderDataTable({
      df.sensoForDisplay()
    }, options = list(processing = FALSE))
    
    ## ANOVA ----
    output$selectAnovaVar = renderUI(selectInput(
      inputId = "anovaVar",
      label = "Variable: ",
      choices = names(Filter(is.numeric, df.senso()))
    ))
    
    output$selectAnovaFactors = renderUI(
      selectInput(
        inputId = "anovaFactors",
        label = "Factor:",
        choices = names(Filter(is.factor, df.senso())),
        multiple = TRUE
      )
    )
    
    anovaFactors = reactive({
      req(input$anovaFactors)
    })
    
    output$anova = renderPrint({
      summary(aov(as.formula(paste(
        input$anovaVar, " ~ ", paste(anovaFactors(), collapse = "*")
      )), data = df.senso()))
    })
    
    ## Boxplot ----
    
    output$selectBoxplotVar = renderUI(selectInput(
      inputId = "boxplotVar",
      label = "Variable: ",
      choices = names(Filter(is.numeric, df.senso()))
    ))
    
    output$selectBoxplotFactor = renderUI(selectInput(
      inputId = "boxplotFactor",
      label = "Factor:",
      choices = names(Filter(is.factor, df.senso()))
    ))
    
    boxplotVar = reactive({
      req(input$boxplotVar)
    })
    
    boxplotFactor = reactive({
      req(input$boxplotFactor)
    })
    
    output$boxPlot <-  renderPlotly({
      plot_ly(
        data = df.senso(),
        x = df.senso()[[boxplotVar()]],
        color = df.senso()[[boxplotFactor()]],
        colors = "RdYlBu",
        type = "box"
      )
    })
    
    ## PCA ----
    obj.pca = reactive({
      res.PCA=getPCA(df.senso())
      if(!is.null(input$fileHedo))rownames(res.PCA$ind$coord) = rownames(df.hedo())
      return(res.PCA)
    })
    
    ## Scree plot ----
    
    
    output$screePlot <- renderPlot({
      fviz_screeplot(obj.pca(), choice = input$choice)
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
        fviz_pca_var(obj.pca(),
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
    
    
    output$biPlot = renderPlot({
      fviz_pca_biplot(
        obj.pca(),
        repel = T,
        alpha.var = "contrib",
        col.var = "cos2",
        col.ind = "#f5365c",
        axes = c(as.numeric(input$axis_X2), as.numeric(Y_axis2()))
      ) + theme_light()
    }, height = 600, width = 600)
    
    ## Pred Map ----
    
    mapBisc <- reactive({
      mapWithPCA(df.senso(), df.hedo())
    })
    
    fittedModels <- reactive({
      fitModel(mapBisc(), formula = input$modelFormula)
    })
    
    discreteSpace = reactive({
      makeGrid(mapBisc(), 50)
    })
    
    predictedScores = reactive({
      sapply(fittedModels(), predict, newdata = discreteSpace()) %>% 
        as.data.frame()
      })
    
    output$mapPlot = renderPlot({
      plotMap(predictedScores()[[1]], mapBisc(), discreteSpace(), plot.contour = F)
    }, height = 600, width = 600)
  }
  
)
