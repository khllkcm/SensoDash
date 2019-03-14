ui <- argonDashPage(
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
      tags$link(rel = "stylesheet", type = "text/css", 
                href = "style.css"),
      ## JS ----
      tags$script(
        "$(document).on('click', function(event) {
        $('li[class*=\\'active\\']').removeClass('active');
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
            argonColumn(
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
            ),
            argonColumn(
              center=T,
              actionButton("validateHedo", "Validate Hedonic Dataset")
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
            argonColumn(
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
            uiOutput("selectSensoProduct"),
            argonColumn(
              center=T,
              actionButton("validateSenso", "Validate Sensory Dataset")
            )
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
          
          ### Hierarchical Clustering inputs ----
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
          ### K-means inputs ----
          ,conditionalPanel(
            condition = "input.clusterAlgo=='K-Means'",
            selectInput(
              "kmeansAlgo",
              "Algorithm",
              choices = c(
                "Hartigan-Wong", "Lloyd", "Forgy","MacQueen"
              )
            ),
            numericInput(
              "kmeansNum",
              "Number of Clusters",
              2,
              min = 2,
              max = 10,
              step = 1
            )
          )
          
          
          
        ),
        argonColumn(
          width = 10,
          tabsetPanel(
            id = "tab-23",
            ### Inertia ----
            
            
            ### Clusters ----
            
            tabPanel(
              "Clusters",
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
            tabPanel(
              "Dendrogram",
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
            tabPanel(
              "Class Characteristics",
              argonColumn(
                center = T,
                plotlyOutput("classCharac", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
                  ),
                div(
                  style = 'overflow-x: scroll',
                  dataTableOutput("productCharac") %>%
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
    )