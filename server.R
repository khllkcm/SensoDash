server <- function(input, output) {
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
  
  df.hedo = eventReactive(input$validateHedo,{
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
  
  df.senso = eventReactive(input$validateSenso,{
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
  })
  
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
  
  predDiscreteSpace = reactive({
    req(mapBisc())
    makeGrid(mapBisc(), input$predNbPoints)
  })
  
  predictedScores = reactive({
    scores = sapply(fittedModels(), predict, newdata = predDiscreteSpace()) %>%
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
      predDiscreteSpace(),
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
      predDiscreteSpace(),
      plot.3D = input$pred3D
    )
  })
  
  ## Pref Map ----
  
  prefPredictedScores = reactive({
    scores = sapply(fittedModels(), predict, newdata = prefDiscreteSpace()) %>%
      as.data.frame()
    return(scores)
  })
  
  prefDiscreteSpace = reactive({
    req(mapBisc())
    makeGrid(mapBisc(), input$prefNbPoints)
  })
  
  
  preferences = reactive({
    mapply(function(x, y) {
      as.numeric(x > mean(y))
    }, prefPredictedScores(), df.hedo()) %>% as.data.frame()
  })
  
  output$mapPrefPlot = renderPlot({
    req(input$prefContourStep)
    req(input$prefNbPoints)
    plotMap(
      100 * preferences() %>% rowMeans(na.rm = T),
      mapBisc(),
      prefDiscreteSpace(),
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
      prefDiscreteSpace(),
      plot.type = "preference",
      plot.3D = input$pref3D
    )
  })
  
  ## Clustering Objects----
  
  ### General Objects ----
  
  obj.pca.conso = reactive({
    PCA(t(df.hedo()), graph = F)
  })
  
  classMeans = reactive({
    classMeans = NULL
    for (class in unique(obj.classes())) {
      classMeans = cbind(classMeans,df.hedo()[, which(obj.classes() == class)] %>% 
                           as.data.frame() %>% rowMeans())
    }
    rownames(classMeans) = rownames(df.hedo())
    return(classMeans)
  })
  
  classMeansText = reactive({
    paste("Average Score",round(classMeans(),3)) %>%matrix(nrow =nrow(classMeans()))
  })
  
  ### Classes ----
  obj.classes = reactive({
    if (input$clusterAlgo == "Hierarchical"){
      req(input$hclusterNum)
      classes= cutree(obj.hc(), k = input$hclusterNum)
    }
    if (input$clusterAlgo == "K-Means"){
      classes = obj.kmeans()$cluster
    }
    if (input$clusterAlgo == "DIANA"){
      req(input$dianaNum)
      classes= cutree(obj.diana(), k = input$dianaNum)
    }
    return(classes)
  })
  
  ### Hierarchical CLustering ----
  obj.hc = reactive({
    distance = dist(t(df.hedo()), method = input$hclusterDist)
    hc = hclust(distance, method = input$hclusterAgg)
    return(hc)
  })
  
  ### K-means ----
  obj.kmeans = reactive({
    kmeans(t(df.hedo()), centers = input$kmeansNum, algorithm=input$kmeansAlgo)
  })
  
  ### DIANA ----
  obj.diana = reactive({
    diana(t(df.hedo()),diss = input$dianaDiss, metric = input$dianaMetric, stand = input$dianaStand)
  })
  
  ### CLARA ----
  
  ## Tabs ----
  
  observe({if (input$clusterAlgo=='Hierarchical') {
    showTab(inputId = "tab-23",target = "Dendrogram",select=F)
  }
  else{
    hideTab(inputId = "tab-23", target = "Dendrogram")
  }
  if (input$clusterAlgo=='Hierarchical' | input$clusterAlgo=='DIANA') {
    showTab(inputId = "tab-23",target = "Inertia",select=T)
    
  }
  else{
    hideTab(inputId = "tab-23", target = "Inertia")
  }})
  
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
    if (input$clusterAlgo == "DIANA") {
      ggplot(data.frame(
        height = rev(obj.diana()$height),
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
      fviz_pca_ind(
        obj.pca.conso(),
        repel = input$repel,
        habillage = as.factor(obj.classes()),
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
  
  output$classCharac = renderPlotly({
    plot_ly(
      x = as.factor(unique(obj.classes())),
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
  
  
  output$productCharac <- renderDataTable({
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

