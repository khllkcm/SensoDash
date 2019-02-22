library(shiny)
library(argonR)
library(argonDash)
library(shinycssloaders)
library(colourpicker)
library(DT)
library(doBy)
library(factoextra)
library(shinyalert)
library(tools)
library(tibble)

#setwd("~/School/Atelier/")
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
          icon_color = "default",
          "Upload Datasets"
        ),
        argonSidebarItem(
          tabName = "eda",
          icon = "chart-pie-35",
          icon_color = "warning",
          "Exploratory Data Analysis"
        ),
        argonSidebarItem(
          tabName = "pca",
          icon = "chart-bar-32",
          icon_color = "success",
          "Principle Component Analysis"
        ),
        argonSidebarItem(
          tabName = "maps",
          icon = "map-big",
          icon_color = "info",
          "Maps"
        ),
        argonSidebarItem(
          tabName = "clust",
          icon = "building",
          icon_color = "danger",
          "Clustering"
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
      tags$head(
        tags$style(
          HTML(
            ".control-label {margin-bottom: 1rem;} .progress {height: 20px;} .btn{padding:0.7rem 1.25rem;} .input-group .form-control:not(:first-child){padding-left:10px;} .form-group{margin-bottom: 0.75rem;}"
          )
        ),
        ## JS ----
        tags$script(
          "$(document).on('click', function(event) {
          Shiny.onInputChange('currentTab', $('.active').data().value);
          });"
)
      ),
useShinyalert(),
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
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                'csv',
                'tsv'
              )
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
              argonColumn(width = 6,
                          checkboxInput("headerHedo", "Header", TRUE)),
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
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                'csv',
                'tsv'
              )
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
  ## Maps ----
  argonTabItem(
    tabName = "maps",
    argonTabSet(
      id = "tab-4",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = NULL,
      ## Prediction map ----
      
      argonTab(
        tabName = "Score Prediction Map",
        active = TRUE,
        argonRow(
          argonColumn(
            width = 3,
            selectInput(
              "modelFormula",
              label = "Formula",
              choices = c("Vector", "Circular", "Elliptic", "Quadratic"),
              selected = "Quadratic"
            ),
            checkboxInput("pred3D", "3D Plot", FALSE),
            conditionalPanel(
              condition = "!input.pred3D",
              checkboxInput("predInterpolate", "Interpolate", TRUE),
              conditionalPanel(
                condition = "!input.predInterpolate",
                numericInput(
                  "predNbPoints",
                  "Number of points",
                  50,
                  min = 10,
                  max = 150,
                  step = 10
                )
              ),
              checkboxInput("predContour", "Plot Contour", FALSE),
              conditionalPanel(
                condition = "input.predContour",
                numericInput("predContourStep", "Contour Step", 1.5, min =
                               0.25)
              ),
              checkboxInput("predShowProds", "Show Product Names", FALSE),
              checkboxInput("predShowProdDots", "Show Product Points", FALSE),
              checkboxInput("predChangeColors", "Change Label Colors", FALSE),
              conditionalPanel(
                condition = "input.predChangeColors",
                colourInput(
                  inputId = "predContourColor",
                  label = "Contour Color:",
                  palette = "limited",
                  value = "black"
                ),
                colourInput(
                  inputId = "predProdColor",
                  label = "Product Color:",
                  palette = "limited",
                  value = "white"
                )
              )
            )
          ),
          argonColumn(
            width = 9,
            center = T,
            conditionalPanel(
              condition = "!input.pred3D",
              plotOutput("mapPlot", height = "100%") %>%
                withSpinner(
                  color = "#5e72e4",
                  type = 7,
                  proxy.height = "400px"
                )
            ),
            conditionalPanel(
              condition = "input.pred3D",
              plotlyOutput("mapPlotly", height = "627px") %>%
                withSpinner(
                  color = "#5e72e4",
                  type = 7,
                  proxy.height = "400px"
                )
            )
          )
          
        )
      ),
      ## Preference map ----
      argonTab(
        tabName = "Preference Map",
        active = FALSE,
        argonRow(
          argonColumn(
            width = 3,
            selectInput(
              "modelFormulaPref",
              label = "Formula",
              choices = c("Vector", "Circular", "Elliptic", "Quadratic"),
              selected = "Quadratic"
            ),
            checkboxInput("pref3D", "3D Plot", FALSE),
            conditionalPanel(
              condition = "!input.pref3D",
              checkboxInput("prefInterpolate", "Interpolate", TRUE),
              conditionalPanel(
                condition = "!input.prefInterpolate",
                numericInput(
                  "prefNbPoints",
                  "Number of points",
                  50,
                  min = 10,
                  max = 150,
                  step = 10
                )
              ),
              checkboxInput("prefContour", "Plot Contour", FALSE),
              conditionalPanel(
                condition = "input.prefContour",
                numericInput("prefContourStep", "Contour Step", 10, min =
                               1)
              ),
              checkboxInput("prefShowProds", "Show Product Names", FALSE),
              checkboxInput("prefShowProdDots", "Show Product Points", FALSE),
              checkboxInput("prefChangeColors", "Change Label Colors", FALSE),
              conditionalPanel(
                condition = "input.prefChangeColors",
                colourInput(
                  inputId = "prefContourColor",
                  label = "Contour Color:",
                  palette = "limited",
                  value = "black"
                ),
                colourInput(
                  inputId = "prefProdColor",
                  label = "Product Color:",
                  palette = "limited",
                  value = "white"
                )
              )
            )
          ),
          argonColumn(
            width = 9,
            center = T,
            conditionalPanel(
              condition = "!input.pref3D",
              plotOutput("mapPrefPlot", height = "100%") %>%
                withSpinner(
                  color = "#5e72e4",
                  type = 7,
                  proxy.height = "400px"
                )
            ),
            conditionalPanel(
              condition = "input.pref3D",
              plotlyOutput("mapPrefPlotly", height = "627px") %>%
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
  
  ## Clustering ----
  argonTabItem(
    tabName = "clust",
    argonCard(
      width = 12,
      src = NULL,
      icon = "ui-04",
      status = "success",
      shadow = TRUE,
      border_level = 0,
      argonRow(
        argonColumn(
          width = 2,
          selectInput(
            "clusterAlgo",
            "Clustering Algorithm",
            choices = c("Hierarchical", "K-Means")
          ),
          checkboxInput("repel", "Repel", value = F),
          conditionalPanel(
            condition = "input.clusterAlgo=='K-Means'",
            numericInput(
              "numClust",
              "Number of Clusters",
              5,
              min = 2,
              max = 10,
              step = 1
            )
          ),
          conditionalPanel(
            condition = "input.clusterAlgo=='Hierarchical'",
            selectInput(
              "hclusterDist",
              "Distance Method",
              choices = c(
                "euclidean",
                "maximum",
                "manhattan",
                "canberra",
                "binary",
                "minkowski"
              )
            ),
            selectInput(
              "hclusterAgg",
              "Aggregation Method",
              choices = c(
                "ward.D",
                "ward.D2",
                "single",
                "complete",
                "average",
                "mcquitty",
                "median",
                "centroid"
              )
            ),
            numericInput(
              "hclusterNum",
              "Number of Clusters",
              5,
              min = 2,
              max = 10,
              step = 1
            )
          )
          
        ),
        argonColumn(
          width = 10,
          argonTabSet(
            id = "tab-23",
            card_wrapper = F,
            horizontal = T,
            circle = F,
            size = "sm",
            
            width = 12,
            iconList = NULL,
            
            
            ### Inertia ----
            
            argonTab(
              tabName = "Inertia",
              active = TRUE,
              argonColumn(
                center = T,
                plotOutput("inertia", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  )
              )
            ),
            ### Clusters ----
            
            argonTab(
              tabName = "Clusters",
              active = FALSE,
              argonColumn(
                center = T,
                plotOutput("clusters", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  )
              )
            ),
            ### Dendrogram ----
            argonTab(
              tabName = "Dendrogram",
              active = FALSE,
              argonColumn(
                center = T,
                plotOutput("dendrogram", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  )
              )
            ),
            ### Class Preference ----
            argonTab(
              tabName = "Class Preference",
              active = FALSE,
              argonColumn(
                center = T,
                plotlyOutput("classPref", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  ),
                div(
                  style = 'overflow-x: scroll',
                  dataTableOutput("productCarac") %>%
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
      )
    )
    
  )
)

        ),


# Footer ----
footer = NULL
    ),

# Server ----
server = function(input, output, session) {
  ## Dataset Hedo ----
  
  df.hedoForDisplay = reactive({
    req(input$fileHedo)
    validate(need(
      file_ext(input$fileHedo$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
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
    #return(read.csv("hedo.csv", sep = ';', row.names = 1))
    req(input$fileHedo)
    validate(need(
      file_ext(input$fileHedo$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
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
    validate(need(
      file_ext(input$fileSenso$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
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
    #return(read.csv("senso.csv"))
    req(input$sensoSession)
    req(input$sensoJudge)
    req(input$sensoProduct)
    validate(need(
      file_ext(input$fileSenso$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
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
    res.PCA = getPCA(df.senso())$PCA
    if (!is.null(input$fileHedo))
      rownames(res.PCA$ind$coord) = rownames(df.hedo())
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
    req(input$currentTab)
    if (input$currentTab != "data")
      mapWithPCA(df.senso(), df.hedo())
  })
  
  fittedModels <- reactive({
    req(mapBisc())
    fitModel(mapBisc(), formula = input$modelFormula)
  })
  
  discreteSpace = reactive({
    req(mapBisc())
    makeGrid(mapBisc(), input$predNbPoints)
  })
  
  predictedScores = reactive({
    scores = sapply(fittedModels(), predict, newdata = discreteSpace()) %>%
      as.data.frame()
    return(scores)
  })
  
  qualityMessagePred = reactive({
    predictionQuality(predictedScores())
  })
  
  
  observeEvent(input$currentTab, {
    if (input$currentTab == "maps")
      shinyalert(
        title = "Warning",
        text = qualityMessagePred(),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
  })
  
  observeEvent(predictedScores(), {
    if (input$currentTab == "maps")
      shinyalert(
        title = "Warning",
        text = qualityMessagePred(),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
  })
  
  output$mapPlot = renderPlot({
    req(input$predContourStep)
    req(input$predNbPoints)
    plotMap(
      predictedScores() %>% rowMeans(na.rm = T),
      mapBisc(),
      discreteSpace(),
      plot.contour = input$predContour,
      plot.3D = input$pred3D,
      show.prods = input$predShowProds,
      prod.points = input$predShowProdDots,
      interpolate = input$predInterpolate,
      contour.step = input$predContourStep,
      nbpoints = input$predNbPoints,
      contour.col = input$predContourColor,
      prod.col = input$predProdColor
    )
  }, height = 600, width = 600)
  
  output$mapPlotly = renderPlotly({
    plotMap(
      predictedScores() %>% rowMeans(na.rm = T),
      mapBisc(),
      discreteSpace(),
      plot.3D = input$pred3D
    )
  })
  
  ## Pref Map ----
  
  preferences = reactive({
    mapply(function(x, y) {
      as.numeric(x > mean(y))
    }, predictedScores(), df.hedo()) %>% as.data.frame()
  })
  
  output$mapPrefPlot = renderPlot({
    req(input$prefContourStep)
    req(input$prefNbPoints)
    plotMap(
      100 * preferences() %>% rowMeans(na.rm = T),
      mapBisc(),
      discreteSpace(),
      plot.type = "preference",
      plot.contour = input$prefContour,
      plot.3D = input$pref3D,
      show.prods = input$prefShowProds,
      prod.points = input$prefShowProdDots,
      interpolate = input$prefInterpolate,
      contour.step = input$prefContourStep,
      nbpoints = input$prefNbPoints,
      contour.col = input$prefContourColor,
      prod.col = input$prefProdColor
    )
  }, height = 600, width = 600)
  
  
  output$mapPrefPlotly = renderPlotly({
    plotMap(
      100 * preferences() %>% rowMeans(na.rm = T),
      mapBisc(),
      discreteSpace(),
      plot.type = "preference",
      plot.3D = input$pref3D
    )
  })
  
  ## Clustering Objects----
  
  obj.pca.conso = reactive({
    PCA(t(df.hedo()), graph = F)
  })
  
  
  obj.hc = reactive({
    distance = dist(t(df.hedo()), method = input$hclusterDist)
    hc = hclust(distance, method = input$hclusterAgg)
    return(hc)
  })
  
  
  hclasses = reactive({
    req(input$hclusterNum)
    cutree(obj.hc(), k = input$hclusterNum)
  })
  
  
  classMeans = reactive({
    classMeans = NULL
    for (class in unique(hclasses())) {
      classMeans = cbind(classMeans,df.hedo()[, which(hclasses() == class)] %>% 
                           as.data.frame() %>% rowMeans())
      }
    rownames(classMeans) = rownames(df.hedo())
    return(classMeans)
  })
  
  classMeansText = reactive({
    paste("Average Score",round(classMeans(),3)) %>%matrix(nrow =nrow(classMeans()))
  })
  
  obj.kmeans = reactive({
    kmeans(t(df.hedo()), centers = input$numClust)
  })
  
  ## Inertia ----
  
  output$inertia = renderPlot({
    if (input$clusterAlgo == "Hierarchical") {
      ggplot(data.frame(
        height = rev(obj.hc()$height),
        class = seq(ncol(df.hedo()) - 1)
      ),
      aes(x = class, y = height)) +
        geom_step(direction = 'vh') +
        xlab("Number of Classes") +
        ylab("Inertia") +
        theme_minimal()
    }
  }, height = 600, width = 600)
  
  ## Clusters ----
  output$clusters = renderPlot({
    if (input$clusterAlgo == "Hierarchical")
      fviz_pca_ind(
        obj.pca.conso(),
        repel = input$repel,
        habillage = as.factor(hclasses()),
        ellipse.type = "convex",
        addEllipses = T
      )
  }, height = 600, width = 600)
  
  ## Dendogram ----
  
  output$dendrogram = renderPlot({
    if (input$clusterAlgo == "Hierarchical")
      fviz_dend(obj.hc(), color_labels_by_k = TRUE)
  }, height = 600, width = 600)
  
  ## Class Preference ----
  
  output$classPref = renderPlotly({
    plot_ly(
      x = as.factor(unique(hclasses())),
      y = rownames(classMeans()),
      z = classMeans(),
      type = "heatmap",
      source = "heatplot",
      xgap = 5,
      ygap = 1,
      hoverinfo="text",
      text=classMeansText()
    ) %>%
      layout(xaxis = list(title = "", dtick = 1),
             yaxis = list(title = ""))
  })
  
  
  output$productCarac <- renderDataTable({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      table = t(getPCA(df.senso())$X[unlist(s[["pointNumber"]])[1] + 1, -1]) %>%
        round(3)%>%
        as.data.frame() %>%
        rownames_to_column(var = paste(s[["y"]], "Characteristics"))
      colnames(table)[2] = "Average Judge Score"
      return(table)
    } else {
      
    }
  }, options = list(processing = FALSE))
  
  
}

)
