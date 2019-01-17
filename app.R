library(shiny)
library(argonR)
library(argonDash)
library(shinycssloaders)

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
      
      ## EDA ----
      
      argonTabItem(tabName = "eda",
                   argonRow(
                     br(),
                     br(),
                     argonCard(
                       width = 12,
                       title = "Argon Card",
                       src = NULL,
                       hover_lift = FALSE,
                       shadow = TRUE,
                       shadow_size = NULL,
                       hover_shadow = FALSE,
                       border_level = 0,
                       icon = "atom",
                       status = "primary",
                       background_color = NULL,
                       gradient = FALSE,
                       floating = FALSE,
                       argonRow(
                         argonColumn(width = 6,
                                     radioButtons(
                                       "dist",
                                       "Distribution type:",
                                       c(
                                         "Normal" = "norm",
                                         "Uniform" = "unif",
                                         "Log-normal" = "lnorm",
                                         "Exponential" = "exp"
                                       )
                                     )),
                         argonColumn(width = 6, plotOutput("plot"))
                       )
                     )
                   )),
      
      
      ## PCA ----
      
      argonTabItem(
        tabName = "pca",
        argonTabSet(
          id = "tab-1",
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
                plotOutput("screePlot", height = "100%")
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
  
  
  
  # Server
  server = function(input, output) {
    
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
