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
        icon = argonIcon("single-copy-04", "default"),
        "Upload Datasets"
      ),
      argonSidebarItem(
        tabName = "eda",
        icon = argonIcon("chart-pie-35", "warning"),
        "Exploratory Data Analysis"
      ),
      argonSidebarItem(
        tabName = "pca",
        icon = argonIcon("chart-bar-32", "success"),
        "Principle Component Analysis"
      ),
      argonSidebarItem(
        tabName = "maps",
        icon = argonIcon("map-big", "info"),
        "Global Maps"
      ),
      argonSidebarItem(
        tabName = "clust",
        icon = argonIcon("building", "danger"),
        "Clustering"
      ),
      argonSidebarItem(
        tabName = "validity",
        icon = argonIcon("check-bold", "success"),
        "Cluster Validation"
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
    useShinyjs(),
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
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
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
                      "decHedo",
                      "Decimal",
                      choices = c(",","."),
                      selected = "."
                    )
                  )
                ),
                argonColumn(
                  center = T,
                  actionButton("validateHedo", "Validate Hedonic Dataset")
                )
              ),
              argonColumn(
                center = T,
                width = 9,
                dataTableOutput("contentsHedo") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "400px"
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
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
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
                      "decSenso",
                      "Decimal",
                      choices = c(",","."),
                      selected = "."
                    )
                  )
                ),
                uiOutput("selectSensoSession"),
                uiOutput("selectSensoJudge"),
                uiOutput("selectSensoProduct"),
                argonColumn(
                  center = T,
                  actionButton("validateSenso", "Validate Sensory Dataset")
                )
              ),
              argonColumn(
                center = T,
                width = 9,
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
                  ),
                hidden(downloadButton('downloadScreePlot', "Download PNG"))
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
                  ),
                hidden(downloadButton('downloadVarPlot', "Download PNG"))
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
                  ),
                hidden(downloadButton('downloadBiPlot', "Download PNG"))
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
                width = 2,
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
                    colourpicker::colourInput(
                      inputId = "predContourColor",
                      label = "Contour Color:",
                      palette = "limited",
                      value = "black"
                    ),
                    colourpicker::colourInput(
                      inputId = "predProdColor",
                      label = "Product Color:",
                      palette = "limited",
                      value = "white"
                    )
                  )
                )
              ),
              argonColumn(
                width = 10,
                center = T,
                uiOutput("predWarning"),
                conditionalPanel(
                  condition = "!input.pred3D",
                  plotOutput("mapPlot", height = "100%") %>%
                    withSpinner(
                      color = "#5e72e4",
                      type = 7,
                      proxy.height = "400px"
                    ),
                  hidden(downloadButton('downloadPredPlot', "Download PNG"))
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
                width = 2,
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
                    colourpicker::colourInput(
                      inputId = "prefContourColor",
                      label = "Contour Color:",
                      palette = "limited",
                      value = "black"
                    ),
                    colourpicker::colourInput(
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
                    ),
                  hidden(downloadButton('downloadPrefPlot', "Download PNG"))
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
                "Clustering Method",
                choices = c(
                  "Hierarchical",
                  "K-Means",
                  "DIANA",
                  "CLARA",
                  "PAM",
                  "SOM",
                  "SOTA"
                )
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
                  "Number of Clusters to Display",
                  5,
                  min = 2,
                  max = 10,
                  step = 1
                )
              )
              ### K-means inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='K-Means'",
                selectInput(
                  "kmeansAlgo",
                  "Algorithm",
                  choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
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

              ### DIANA inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='DIANA'",
                selectInput("dianaMetric",
                            "Metric",
                            choices = c("euclidean", "manhattan")),
                numericInput(
                  "dianaNum",
                  "Number of Clusters to Display",
                  2,
                  min = 2,
                  max = 10,
                  step = 1
                )
              )

              ### CLARA inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='CLARA'",
                selectInput(
                  "claraMetric",
                  "Metric",
                  choices = c("euclidean", "manhattan", "jaccard")
                ),
                numericInput(
                  "claraNum",
                  "Number of Clusters",
                  2,
                  min = 2,
                  max = 10,
                  step = 1
                )
              )

              ### PAM inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='PAM'",
                selectInput("pamMetric",
                            "Metric",
                            choices = c("euclidean", "manhattan")),
                numericInput(
                  "pamNum",
                  "Number of Clusters",
                  2,
                  min = 2,
                  max = 10,
                  step = 1
                )
              )


              ### SOM inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='SOM'",
                numericInput(
                  "somx",
                  "Grid X dimension",
                  2,
                  min = 1,
                  max = 10,
                  step = 1
                ),
                numericInput(
                  "somy",
                  "Grid Y dimension",
                  2,
                  min = 1,
                  max = 10,
                  step = 1
                )
              )

              ### SOTA inputs ----
              ,
              conditionalPanel(
                condition = "input.clusterAlgo=='SOTA'",
                numericInput(
                  "sotaNum",
                  "Maximum Number of Clusters",
                  2,
                  min = 2,
                  max = 10,
                  step = 1
                )
              ),
              argonRow(center = T,
                       actionButton(inputId = "run", "Run"))

            ),
            argonColumn(
              width = 10,
              tabsetPanel(
                id = "tab-23",
                ### Inertia ----
                tabPanel(
                  "Inertia",
                  argonColumn(
                    center = T,
                    plotlyOutput("inertia", height = "100%") %>%
                      withSpinner(
                        color = "#5e72e4",
                        type = 7,
                        proxy.height = "400px"
                      )
                  )
                ),

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
                      ),
                    hidden(downloadButton('downloadClusterPlot', "Download PNG"))
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
                      ),
                    hidden(downloadButton('downloadDendroPlot', "Download PNG"))
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
                      )
                  )
                )



              )
            )
          )
        )

      ),

      # Validity ----
      argonTabItem(
        tabName = "validity",
        conditionalPanel(
          "output.tab==' '",
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
                  "validMethod",
                  "Clustering Method",
                  choices = c("Hierarchical",
                              "KMeans",
                              "DIANA",
                              "CLARA",
                              "PAM",
                              "SOTA"),
                  multiple = T
                ),
                sliderInput(
                  "validNumClust",
                  "Number of Clusters",
                  min = 2,
                  max = 10,
                  value = c(2, 4)
                ),
                selectInput(
                  "validVMethod",
                  "Validation Method",
                  choices = list("full"="internal", "stepwise"="stability")
                ),
                argonRow(
                  argonColumn(
                    center = T,
                    actionButton("validClust", "Validate"),
                    argonRow(),
                    argonRow(),
                    hr()
                  ),
                  argonRow(argonColumn(
                    center = T,
                    hidden(actionButton("optimal", "Show Optimal Scores")),
                    hidden(actionButton("optiNext", "Fit Optimal Method"))
                  ))
                )
              ),
              argonColumn(
                width = 10,
                center = T,
                plotOutput("valPlot", height = "100%") %>%
                  withSpinner(
                    color = "#5e72e4",
                    type = 7,
                    proxy.height = "600px"
                  ),
                hidden(downloadButton('downloadValPlot', "Download PNG"))
              )
            )
          )
        ),
        conditionalPanel(
          "output.tab=='  '",
          argonTabSet(
            id = "tab-6",
            card_wrapper = TRUE,
            horizontal = TRUE,
            circle = FALSE,
            size = "sm",
            width = 12,
            iconList = NULL,
            ## Optimal CLusters ----

            argonTab(
              tabName = "Optimal Cluster Map",
              active = TRUE,
              argonRow(
                argonColumn(
                  width = 3,
                  uiOutput("optimalMeasures"),
                  argonRow(),
                  argonRow(),
                  hr(),
                  argonRow(argonColumn(
                    center = T,
                    actionButton("optiPrev", "Previous")
                  ))
                ),
                argonColumn(
                  width = 9,
                  center = T,
                  plotOutput("optimalClusterPlot", height = "100%") %>%
                    withSpinner(
                      color = "#5e72e4",
                      type = 7,
                      proxy.height = "400px"
                    ),
                  hidden(downloadButton(
                    'downloadOptimalClusterPlot', "Download PNG"
                  ))
                )
              )
            ),
            ## Optimal Prediction map ----

            argonTab(
              tabName = "Class Prediction Map",
              active = F,
              argonRow(
                argonColumn(
                  width = 2,
                  uiOutput("selectClass"),
                  selectInput(
                    "optimalModelFormula",
                    label = "Formula",
                    choices = c("Vector", "Circular", "Elliptic", "Quadratic"),
                    selected = "Quadratic"
                  ),
                  checkboxInput("optimalPred3D", "3D Plot", FALSE),
                  conditionalPanel(
                    condition = "!input.optimalPred3D",
                    checkboxInput("optimalPredInterpolate", "Interpolate", TRUE),
                    conditionalPanel(
                      condition = "!input.optimalPredInterpolate",
                      numericInput(
                        "optimalPredNbPoints",
                        "Number of points",
                        50,
                        min = 10,
                        max = 150,
                        step = 10
                      )
                    ),
                    checkboxInput("optimalPredContour", "Plot Contour", FALSE),
                    conditionalPanel(
                      condition = "input.optimalPredContour",
                      numericInput("optimalPredContourStep", "Contour Step", 1.5, min =
                                     0.25)
                    ),
                    checkboxInput("optimalPredShowProds", "Show Product Names", FALSE),
                    checkboxInput("optimalPredShowProdDots", "Show Product Points", FALSE),
                    checkboxInput("optimalPredChangeColors", "Change Label Colors", FALSE),
                    conditionalPanel(
                      condition = "input.optimalPredChangeColors",
                      colourpicker::colourInput(
                        inputId = "optimalPredContourColor",
                        label = "Contour Color:",
                        palette = "limited",
                        value = "black"
                      ),
                      colourpicker::colourInput(
                        inputId = "optimalPredProdColor",
                        label = "Product Color:",
                        palette = "limited",
                        value = "white"
                      )
                    )
                  )
                ),
                argonColumn(
                  width = 10,
                  center = T,
                  uiOutput("optimalPredWarning"),
                  conditionalPanel(
                    condition = "!input.optimalPred3D",
                    plotOutput("mapOptimalPlot", height = "100%") %>%
                      withSpinner(
                        color = "#5e72e4",
                        type = 7,
                        proxy.height = "400px"
                      ),
                    hidden(downloadButton(
                      'downloadOptimalPredPlot', "Download PNG"
                    ))
                  ),
                  conditionalPanel(
                    condition = "input.optimalPred3D",
                    plotlyOutput("mapOptimalPlotly", height = "627px") %>%
                      withSpinner(
                        color = "#5e72e4",
                        type = 7,
                        proxy.height = "400px"
                      )
                  )
                )

              )
            ),
            ## Optimal Preference map ----
            argonTab(
              tabName = "Class Preference Map",
              active = FALSE,
              argonRow(
                argonColumn(
                  width = 2,
                  uiOutput("selectPrefClass"),
                  checkboxInput("optimalPref3D", "3D Plot", FALSE),
                  conditionalPanel(
                    condition = "!input.optimalPref3D",
                    checkboxInput("optimalPrefInterpolate", "Interpolate", TRUE),
                    conditionalPanel(
                      condition = "!input.optimalPrefInterpolate",
                      numericInput(
                        "optimalPrefNbPoints",
                        "Number of points",
                        50,
                        min = 10,
                        max = 150,
                        step = 10
                      )
                    ),
                    checkboxInput("optimalPrefContour", "Plot Contour", FALSE),
                    conditionalPanel(
                      condition = "input.optimalPrefContour",
                      numericInput("optimalPrefContourStep", "Contour Step", 10, min =
                                     1)
                    ),
                    checkboxInput("optimalPrefShowProds", "Show Product Names", FALSE),
                    checkboxInput("optimalPrefShowProdDots", "Show Product Points", FALSE),
                    checkboxInput("optimalPrefChangeColors", "Change Label Colors", FALSE),
                    conditionalPanel(
                      condition = "input.optimalPrefChangeColors",
                      colourpicker::colourInput(
                        inputId = "optimalPrefContourColor",
                        label = "Contour Color:",
                        palette = "limited",
                        value = "black"
                      ),
                      colourpicker::colourInput(
                        inputId = "optimalPrefProdColor",
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
                    condition = "!input.optimalPref3D",
                    plotOutput("mapOptimalPrefPlot", height = "100%") %>%
                      withSpinner(
                        color = "#5e72e4",
                        type = 7,
                        proxy.height = "400px"
                      ),
                    hidden(downloadButton(
                      'downloadOptimalPrefPlot', "Download PNG"
                    ))
                  ),
                  conditionalPanel(
                    condition = "input.optimalPref3D",
                    plotlyOutput("mapOptimalPrefPlotly", height = "627px") %>%
                      withSpinner(
                        color = "#5e72e4",
                        type = 7,
                        proxy.height = "400px"
                      )
                  )
                )

              )
            ),
            argonTab(
              tabName = "Class Scores",
              active = FALSE,
              argonColumn(
                center = T,
                plotlyOutput("optimalClassCharac", height = "100%") %>%
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

  ),


  # Footer ----
  footer = verbatimTextOutput("tab")
)
