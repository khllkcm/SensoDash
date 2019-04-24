server <- function(input, output, session) {
  ## Dataset Hedo ----
  test = F
  df.hedoForDisplay = reactive({
    req(input$fileHedo)
    validate(need(
      file_ext(input$fileHedo$name) %in% c('text/csv',
                                           'text/comma-separated-values',
                                           'csv'),
      "Wrong file format. Try again!"
    ))
    df <- read.csv(
      input$fileHedo$datapath,
      header = input$headerHedo,
      sep = input$sepHedo,
      quote = input$quoteHedo,
      dec = input$decHedo,
      row.names = 1
    )
    return(df)
  })
  
  df.hedo = eventReactive(input$validateHedo, {
    if (test)
      return(read.csv("hedo.csv", sep = ';', row.names = 1))
    req(input$fileHedo)
    validate(need(
      file_ext(input$fileHedo$name) %in% c('text/csv',
                                           'text/comma-separated-values',
                                           'csv'),
      "Wrong file format. Try again!"
    ))
    df <- read.csv(
      input$fileHedo$datapath,
      header = input$headerHedo,
      sep = input$sepHedo,
      quote = input$quoteHedo,
      dec = input$decHedo,
      row.names = 1
    )
    return(df)
  })
  
  ## Display Dataset Hedo ----
  output$contentsHedo <- renderDataTable({
    datatable(df.hedoForDisplay(),
              options = list(scrollX = TRUE, processing = FALSE))
  })
  
  ## Dataset Senso ----
  
  output$selectSensoSession = renderUI(
    selectInput(
      inputId = "sensoSession",
      label = "Session:",
      choices = c("NA",colnames(df.sensoForDisplay())),
      selected = "NA"
    )
  )
  
  output$selectSensoJudge = renderUI(
    selectInput(
      inputId = "sensoJudge",
      label = "Judge:",
      choices = c("NA",colnames(df.sensoForDisplay())),
      selected = "NA"
    )
  )
  
  output$selectSensoProduct = renderUI(
    selectInput(
      inputId = "sensoProduct",
      label = "Product:",
      choices = colnames(df.sensoForDisplay()),
      selected = colnames(df.sensoForDisplay())[1]
    )
  )
  
  df.sensoForDisplay = reactive({
    req(input$fileSenso)
    validate(need(
      file_ext(input$fileSenso$name) %in% c('text/csv',
                                            'text/comma-separated-values',
                                            'csv'),
      "Wrong file format. Try again!"
    ))
    df <- read.csv(
      input$fileSenso$datapath,
      header = input$headerSenso,
      sep = input$sepSenso,
      quote = input$quoteSenso,
      dec = input$decSenso
    )
    return(df)
  })
  
  df.senso = eventReactive(input$validateSenso, {
    if (test)
      return(read.csv("senso.csv"))
    req(input$sensoSession)
    req(input$sensoJudge)
    req(input$sensoProduct)
    validate(need(
      file_ext(input$fileSenso$name) %in% c('text/csv',
                                            'text/comma-separated-values',
                                            'csv'),
      "Wrong file format. Try again!"
    ))
    df = read.csv(
      input$fileSenso$datapath,
      header = input$headerSenso,
      sep = input$sepSenso,
      quote = input$quoteSenso,
      dec = input$decSenso
    )
    if (input$sensoSession != "NA")
      df[[input$sensoSession]] = as.factor(df[[input$sensoSession]])
    if (input$sensoJudge != "NA")
      df[[input$sensoJudge]] = as.factor(df[[input$sensoJudge]])
    df[[input$sensoProduct]] = as.factor(df[[input$sensoProduct]])
    return(df)
  })
  
  ## Display Dataset Senso ----
  output$contentsSenso <- renderDataTable({
    datatable(df.sensoForDisplay(),
              options = list(scrollX = TRUE, processing = FALSE))
  })
  
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
    req(input$sensoProduct)
    req(input$sensoJudge)
    req(input$sensoSession)
    res.PCA = getPCA(df.senso(),
                     input$sensoProduct,
                     input$sensoJudge,
                     input$sensoSession)$PCA
    if (!is.null(input$fileHedo))
      rownames(res.PCA$ind$coord) = rownames(df.hedo())
    return(res.PCA)
  })
  
  ## Scree plot ----
  
  screePlot = reactive(fviz_screeplot(obj.pca(), choice = input$choice))
  output$screePlot <- renderPlot({
    req(screePlot())
    on.exit(showElement("downloadScreePlot"))
    screePlot()
  }, height = 600, width = 600)
  
  output$downloadScreePlot <- downloadHandler(
    filename = function() {
      'screeplot.png'
    },
    content = function(file) {
      ggsave(file, plot = screePlot(), device = "png")
    }
  )
  
  
  ## Variable plot ----
  varPlot = reactive(
    fviz_pca_var(
      obj.pca(),
      col.var = "cos2",
      axes = c(as.numeric(input$axis_X), as.numeric(Y_axis()))
    ) +
      scale_color_gradient2(
        low = "white",
        mid = "blue",
        high = "red",
        midpoint = as.numeric(input$n_cos2),
        space = "Lab"
      ) + theme_light()
  )
  
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
      req(varPlot())
      on.exit(showElement("downloadVarPlot"))
      varPlot()
    }, height = 600, width = 600)
  
  output$downloadVarPlot <- downloadHandler(
    filename = function() {
      'variableplot.png'
    },
    content = function(file) {
      ggsave(file, plot = varPlot(), device = "png")
    }
  )
  
  ## Bi-Plot ----
  biPlot = reactive(
    fviz_pca_biplot(
      obj.pca(),
      repel = T,
      alpha.var = "contrib",
      col.var = "cos2",
      col.ind = "#f5365c",
      axes = c(as.numeric(input$axis_X2), as.numeric(Y_axis2()))
    ) + theme_light()
  )
  
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
    req(biPlot())
    on.exit(showElement("downloadBiPlot"))
    biPlot()
  }, height = 600, width = 600)
  
  output$downloadBiPlot <- downloadHandler(
    filename = function() {
      'biplot.png'
    },
    content = function(file) {
      ggsave(file, plot = biPlot(), device = "png")
    }
  )
  
  ## Pred Map ----
  
  mapBisc <- reactive({
    req(input$currentTab)
    if (input$currentTab != "data")
      mapWithPCA(
        df.senso(),
        df.hedo(),
        input$sensoProduct,
        input$sensoJudge,
        input$sensoSession
      )
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
  
  output$predWarning <-
    renderUI(argonAlert(
      qualityMessagePred(),
      closable = T,
      status = "info"
    ))
  
  
  mapPredPlot = reactive({
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
  })
  
  output$mapPlot = renderPlot({
    req(mapPredPlot())
    on.exit(showElement("downloadPredPlot"))
    mapPredPlot()
  }, height = 600, width = 600)
  
  output$mapPlotly = renderPlotly({
    plotMap(
      predictedScores() %>% rowMeans(na.rm = T),
      mapBisc(),
      predDiscreteSpace(),
      plot.3D = input$pred3D
    )
  })
  
  output$downloadPredPlot <- downloadHandler(
    filename = function() {
      'predmap.png'
    },
    content = function(file) {
      ggsave(file, plot = mapPredPlot(), device = "png")
    }
  )
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
  
  mapPrefPlot = reactive({
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
  })
  
  output$mapPrefPlot = renderPlot({
    req(mapPrefPlot())
    on.exit(showElement("downloadPrefPlot"))
    mapPrefPlot()
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
  
  output$downloadPrefPlot <- downloadHandler(
    filename = function() {
      'prefmap.png'
    },
    content = function(file) {
      ggsave(file, plot = mapPrefPlot(), device = "png")
    }
  )
  
  ## Clustering Objects----
  
  ### General Objects ----
  
  obj.pca.conso = reactive({
    PCA(t(df.hedo()), graph = F)
  })
  
  classMeans = reactive({
    classMeans = NULL
    for (class in unique(obj.classes())) {
      classMeans = cbind(classMeans, df.hedo()[, which(obj.classes() == class)] %>%
                           as.data.frame() %>% rowMeans())
    }
    rownames(classMeans) = rownames(df.hedo())
    return(classMeans)
  })
  
  classMeansText = reactive({
    paste("Average Score", round(classMeans(), 3)) %>% matrix(nrow = nrow(classMeans()))
  })
  
  observeEvent(input$clusterAlgo, invalidateLater(1000, session))
  
  ### Classes ----
  obj.classes = reactive({
    if (input$clusterAlgo == "Hierarchical") {
      req(input$hclusterNum)
      classes = cutree(obj.hc(), k = input$hclusterNum)
    }
    if (input$clusterAlgo == "K-Means") {
      classes = obj.kmeans()$cluster
    }
    if (input$clusterAlgo == "DIANA") {
      req(input$dianaNum)
      classes = cutree(obj.diana(), k = input$dianaNum)
    }
    if (input$clusterAlgo == "CLARA") {
      classes = obj.clara()$clustering
    }
    if (input$clusterAlgo == "PAM") {
      classes = obj.pam()$clustering
    }
    if (input$clusterAlgo == "SOM") {
      classes = obj.som()$unit.classif
    }
    if (input$clusterAlgo == "SOTA") {
      classes = obj.sota()$clust
    }
    return(sort(classes))
  })
  
  ### Hierarchical CLustering ----
  obj.hc = reactive({
    distance = dist(t(df.hedo()), method = input$hclusterDist)
    hc = hclust(distance, method = input$hclusterAgg)
    return(hc)
  })
  
  ### K-means ----
  obj.kmeans = reactive({
    kmeans(t(df.hedo()),
           centers = input$kmeansNum,
           algorithm = input$kmeansAlgo)
  })
  
  ### DIANA ----
  obj.diana = reactive({
    diana(t(df.hedo()),
          diss = F,
          metric = input$dianaMetric)
  })
  
  ### CLARA ----
  obj.clara = reactive({
    clara(t(df.hedo()),
          metric = input$claraMetric,
          k = input$claraNum)
  })
  
  ### PAM ----
  obj.pam = reactive({
    pam(t(df.hedo()),
        metric = input$pamMetric,
        k = input$pamNum)
  })
  
  ### SOM ----
  obj.som = reactive({
    som(t(df.hedo()),
        grid = somgrid(input$somx, input$somy, "hexagonal"))
  })
  
  ### SOTA ----
  obj.sota = reactive({
    sota(t(df.hedo()),
         maxCycles = input$sotaNum - 1)
  })
  
  
  ## Tabs ----
  
  observe({
    if (input$clusterAlgo == 'Hierarchical') {
      showTab(inputId = "tab-23",
              target = "Dendrogram",
              select = F)
    }
    else{
      hideTab(inputId = "tab-23", target = "Dendrogram")
    }
    if (input$clusterAlgo == 'Hierarchical' |
        input$clusterAlgo == 'DIANA') {
      showTab(inputId = "tab-23",
              target = "Inertia",
              select = T)
      
    }
    else{
      hideTab(inputId = "tab-23", target = "Inertia")
    }
  })
  
  ## Inertia ----
  
  inertia = eventReactive(input$run, {
    if (input$clusterAlgo == "Hierarchical") {
      return(
        plot_ly(data.frame(
          height = rev(isolate(obj.hc())$height),
          class = seq(ncol(df.hedo()) - 1)
        ),
        x = ~ class) %>% add_lines(
          y = ~ height,
          name = "hv",
          line = list(shape = "hv")
        )
      )
    }
    if (input$clusterAlgo == "DIANA") {
      return(
        plot_ly(data.frame(
          height = rev(isolate(obj.diana())$height),
          class = seq(ncol(df.hedo()) - 1)
        ),
        x = ~ class) %>% add_lines(
          y = ~ height,
          name = "hv",
          line = list(shape = "hv")
        )
      )
    }
  })
  observeEvent(input$clusterAlgo, {
    output$inertia <- renderPlotly(NULL)
  })
  observeEvent(input$run, {
    output$inertia <- renderPlotly(inertia())
  })
  
  ## Clusters ----
  clusterPlot = reactive({
    fviz_pca_ind(
      obj.pca.conso(),
      repel = input$repel,
      habillage = as.factor(obj.classes()),
      ellipse.type = "convex",
      addEllipses = T
    )
  })
  clusters = eventReactive(input$run, {
    clusterPlot()
  })
  observeEvent(input$clusterAlgo, {
    output$clusters <- renderPlot(NULL, height = 100, width = 100)
  })
  observeEvent(input$run, {
    output$clusters <-
      renderPlot({
        on.exit(showElement("downloadClusterPlot"))
        clusters()
      }, height = 600, width = 600)
  })
  output$downloadClusterPlot <- downloadHandler(
    filename = function() {
      'clusterplot.png'
    },
    content = function(file) {
      ggsave(file, plot = clusterPlot(), device = "png")
    }
  )
  
  ## Dendogram ----
  dendroPlot = reactive({
    fviz_dend(isolate(obj.hc()), color_labels_by_k = TRUE)
  })
  dendrogram = eventReactive(input$run, {
    input$run
    if (input$clusterAlgo == "Hierarchical")
      dendroPlot()
  })
  
  observeEvent(input$clusterAlgo, {
    output$dendrogram <- renderPlot(NULL, height = 100, width = 100)
  })
  observeEvent(input$run, {
    output$dendrogram <-
      renderPlot({
        on.exit(showElement("downloadDendroPlot"))
        dendrogram()
      }, height = 600, width = 600)
  })
  output$downloadDendroPlot <- downloadHandler(
    filename = function() {
      'dendroplot.png'
    },
    content = function(file) {
      ggsave(file, plot = dendroPlot(), device = "png")
    }
  )
  
  ## Class Preference ----
  classes = eventReactive(input$run, {
    plot_ly(
      x = as.factor(unique(obj.classes())),
      y = rownames(isolate(classMeans())),
      z = isolate(classMeans()),
      type = "heatmap",
      source = "heatplot",
      xgap = 5,
      ygap = 3,
      hoverinfo = "text",
      text = isolate(classMeansText())
    ) %>%
      layout(xaxis = list(title = "", dtick = 1),
             yaxis = list(title = ""))
  })
  
  output$classCharac = renderPlotly(classes())
  observeEvent(input$clusterAlgo, {
    output$classCharac = renderPlotly({
      plotly_empty(type = "scatter", mode =
                     "markers")
    })
  })
  observeEvent(input$run, {
    output$classCharac = renderPlotly(classes())
  })
  
  clicked <- reactive({
    event_data("plotly_click", source = "heatplot")
  })
  
  observeEvent(clicked(), {
    table = t(
      getPCA(
        df.senso(),
        input$sensoProduct,
        input$sensoJudge,
        input$sensoSession
      )$X[unlist(clicked()[["pointNumber"]])[1] + 1, -1]
    ) %>%
      round(3) %>%
      as.data.frame() %>%
      rownames_to_column(var = paste(clicked()[["y"]], "Characteristics"))
    colnames(table)[2] = "Average Judge Score"
    showModal(modalDialog(easyClose = T, renderDataTable(table[order(table[, 2], decreasing =
                                                                       T),])))
  })
  
  
  observeEvent(input$clusterAlgo, {
    hideElement("downloadClusterPlot")
    hideElement("downloadDendroPlot")
  })
  
  #Validity ----
  clvalid <- reactive({
    req(input$validMethod)
    suppressWarnings(
      clValid(
        t(df.hedo()),
        clMethods = input$validMethod,
        nClust = seq(input$validNumClust[1], input$validNumClust[2]),
        validation = input$validVMethod,
        maxitems = ncol(df.hedo())
      )
    )
  })
  
  valMeasures <- reactive({
    clvalid() %>% measures() %>% melt(
      varnames = c("Measure", "Number of Clusters", "Method"),
      value.name = "Score"
    ) %>% drop_na()
  })
  
  valPlot <- reactive({
    suppressWarnings(
      ggplot(
        valMeasures(),
        aes(x = `Number of Clusters`, y = Score, color = Method)
      ) +
        geom_line() +
        geom_point() +
        facet_wrap(~ Measure, scales = "free") +
        xlab("Number of Clusters") +
        ylab("Measure") +
        scale_x_continuous(breaks = unique(valMeasures()$`Number of Clusters`)) +
        theme_minimal()
    )
  })
  
  output$valPlot <- renderPlot(NULL, width = 100, height = 100)
  
  observeEvent(input$validClust, {
    req(input$validMethod)
    disable('validClust')
    output$valPlot <- renderPlot({
      on.exit({
        enable('validClust')
        showElement('optimal')
        showElement('optiNext')
        showElement('downloadValPlot')
      })
      isolate(valPlot())
    }, height = 600)
  })
  
  #Optimal ----
  
  observeEvent(input$optimal, {
    showModal(
      modalDialog(
        size = "s",
        title = "Optimal Scores",
        renderTable(optimalScores(isolate(clvalid(
          
        ))),
        rownames = T),
        easyClose = T,
        footer = NULL
      )
    )
  })
  
  output$downloadValPlot <- downloadHandler(
    filename = function() {
      'validationplot.png'
    },
    content = function(file) {
      ggsave(file, plot = valPlot(), device = "png")
    }
  )
  
  output$optimalMeasures <- renderUI({
    req(input$validVMethod)
    if (input$validVMethod == "internal")
      return(selectInput(
        "optimalMeasure",
        "Measure",
        choices = c("Connectivity", "Dunn", "Silhouette")
      ))
    else
      return(selectInput(
        "optimalMeasure",
        "Measure",
        choices = c("APN", "AD", "ADM", "FOM")
      ))
  })
  
  
  optimalscores <- reactive({
    req(input$validMethod)
    suppressWarnings(
      clValid(
        t(df.hedo()),
        clMethods = input$validMethod,
        nClust = seq(input$validNumClust[1], input$validNumClust[2]),
        validation = input$validVMethod,
        maxitems = ncol(df.hedo())
      )
    ) %>% optimalScores()
  })
  
  optimalMethod <- reactive({
    optimalscores()[which(rownames(optimalscores()) == input$optimalMeasure), 2] %>%
      as.character()
  })
  
  optimalNum <- reactive({
    optimalscores()[which(rownames(optimalscores()) == input$optimalMeasure), 3] %>%
      as.character() %>%
      as.numeric()
  })
  
  optimalClasses <- reactive({
    req(optimalMethod())
    getOptimalClasses(optimalMethod(), t(df.hedo()), optimalNum())
  })
  
  optimalClusterPlot <- reactive({
    req(optimalMethod())
    fviz_pca_ind(
      obj.pca.conso(),
      repel = F,
      habillage = as.factor(isolate(optimalClasses())),
      ellipse.type = "convex",
      addEllipses = T
    ) + ggtitle(optimalMethod())
  })
  
  output$optimalClusterPlot <- renderPlot({
    on.exit(showElement("downloadOptimalClusterPlot"))
    optimalClusterPlot()
  }, height = 600, width = 600)
  
  
  output$downloadOptimalClusterPlot <- downloadHandler(
    filename = function() {
      'optimalclusterplot.png'
    },
    content = function(file) {
      ggsave(file, plot = optimalClusterPlot(), device = "png")
    }
  )
  
  
  #Pred per class ----
  output$selectClass <-
    renderUI({
      selectInput("optimalClass",
                  "Class",
                  choices = unique(optimalClasses() %>% sort()))
    })
  
  optiFittedModels <- reactive({
    req(mapBisc())
    req(optimalClasses())
    req(input$optimalClass)
    fitModel(mapBisc()[, c(which(optimalClasses() == input$optimalClass),
                           ncol(mapBisc()) - 1,
                           ncol(mapBisc()))], formula = input$optimalModelFormula)
  })
  
  optimalPredDiscreteSpace = reactive({
    req(mapBisc())
    req(optimalClasses())
    req(input$optimalClass)
    makeGrid(mapBisc()[, c(which(optimalClasses() == input$optimalClass),
                           ncol(mapBisc()) - 1,
                           ncol(mapBisc()))], input$optimalPredNbPoints)
  })
  
  optimalPredictedScores = reactive({
    scores = sapply(optiFittedModels(), predict, newdata = optimalPredDiscreteSpace()) %>%
      as.data.frame()
    return(scores)
  })
  
  qualityMessageOptiPred = reactive({
    predictionQuality(optimalPredictedScores())
  })
  
  output$optimalPredWarning <-
    renderUI(argonAlert(
      qualityMessageOptiPred(),
      closable = T,
      status = "info"
    ))
  
  mapoptimalPredPlot = reactive({
    req(input$optimalPredContourStep)
    req(input$optimalPredNbPoints)
    plotMap(
      optimalPredictedScores() %>% rowMeans(na.rm = T),
      mapBisc()[, c(which(optimalClasses() == input$optimalClass),
                    ncol(mapBisc()) - 1,
                    ncol(mapBisc()))],
      optimalPredDiscreteSpace(),
      plot.contour = input$optimalPredContour,
      plot.3D = input$optimalPred3D,
      show.prods = input$optimalPredShowProds,
      prod.points = input$optimalPredShowProdDots,
      interpolate = input$optimalPredInterpolate,
      contour.step = input$optimalPredContourStep,
      nbpoints = input$optimalPredNbPoints,
      contour.col = input$optimalPredContourColor,
      prod.col = input$optimalPredProdColor
    )
  })
  
  output$mapOptimalPlot = renderPlot({
    req(mapoptimalPredPlot())
    on.exit(showElement("downloadOptimalPredPlot"))
    mapoptimalPredPlot()
  }, height = 600, width = 600)
  
  output$mapOptimalPlotly = renderPlotly({
    plotMap(
      optimalPredictedScores() %>% rowMeans(na.rm = T),
      mapBisc()[, c(which(optimalClasses() == input$optimalClass),
                    ncol(mapBisc()) - 1,
                    ncol(mapBisc()))],
      optimalPredDiscreteSpace(),
      plot.3D = input$optimalPred3D
    )
  })
  
  output$downloadOptimalPredPlot <- downloadHandler(
    filename = function() {
      'optimalPredmap.png'
    },
    content = function(file) {
      ggsave(file, plot = mapoptimalPredPlot(), device = "png")
    }
  )
  
  # Pref per class ----
  output$selectPrefClass <-
    renderUI({
      selectInput("optimalPrefClass",
                  "Class",
                  choices = unique(optimalClasses() %>% sort()))
    })
  
  optiPrefFittedModels <- reactive({
    req(mapBisc())
    req(optimalClasses())
    req(input$optimalPrefClass)
    fitModel(mapBisc()[, c(
      which(optimalClasses() == input$optimalPrefClass),
      ncol(mapBisc()) - 1,
      ncol(mapBisc())
    )], formula = input$optimalModelFormula)
  })
  
  optimalPrefPredictedScores = reactive({
    scores = sapply(optiPrefFittedModels(), predict, newdata = optimalPrefDiscreteSpace()) %>%
      as.data.frame()
    return(scores)
  })
  
  optimalPrefDiscreteSpace = reactive({
    req(mapBisc())
    makeGrid(mapBisc()[, c(
      which(optimalClasses() == input$optimalPrefClass),
      ncol(mapBisc()) - 1,
      ncol(mapBisc())
    )], input$optimalPrefNbPoints)
  })
  
  
  optimalPreferences = reactive({
    mapply(function(x, y) {
      as.numeric(x > mean(y))
    }, optimalPrefPredictedScores(), df.hedo()[, which(optimalClasses() ==
                                                         input$optimalPrefClass)]) %>% as.data.frame()
  })
  
  mapOptimalPrefPlot = reactive({
    req(input$optimalPrefContourStep)
    req(input$optimalPrefNbPoints)
    plotMap(
      100 * optimalPreferences() %>% rowMeans(na.rm = T),
      mapBisc()[, c(
        which(optimalClasses() == input$optimalPrefClass),
        ncol(mapBisc()) - 1,
        ncol(mapBisc())
      )],
      optimalPrefDiscreteSpace(),
      plot.type = "preference",
      plot.contour = input$optimalPrefContour,
      plot.3D = input$optimalPref3D,
      show.prods = input$optimalPrefShowProds,
      prod.points = input$optimalPrefShowProdDots,
      interpolate = input$optimalPrefInterpolate,
      contour.step = input$optimalPrefContourStep,
      nbpoints = input$optimalPrefNbPoints,
      contour.col = input$optimalPrefContourColor,
      prod.col = input$optimalPrefProdColor
    )
  })
  
  output$mapOptimalPrefPlot = renderPlot({
    req(mapOptimalPrefPlot())
    on.exit(showElement("downloadOptimalPrefPlot"))
    mapOptimalPrefPlot()
  }, height = 600, width = 600)
  
  
  output$mapOptimalPrefPlotly = renderPlotly({
    plotMap(
      100 * optimalPreferences() %>% rowMeans(na.rm = T),
      mapBisc()[, c(
        which(optimalClasses() == input$optimalPrefClass),
        ncol(mapBisc()) - 1,
        ncol(mapBisc())
      )],
      optimalPrefDiscreteSpace(),
      plot.type = "preference",
      plot.3D = input$optimalPref3D
    )
  })
  
  output$downloadOptimalPrefPlot <- downloadHandler(
    filename = function() {
      'optimalPrefmap.png'
    },
    content = function(file) {
      ggsave(file, plot = mapOptimalPrefPlot(), device = "png")
    }
  )
  
  
  output$tab <- renderText({
    " "
  })
  observeEvent(input$optiNext, {
    output$tab <- renderText({
      "  "
    })
  })
  observeEvent(input$optiPrev, {
    output$tab <- renderText({
      " "
    })
  })
  
  
  # Optimal Class Scores ----
  
  optimalClassMeans = reactive({
    optimalClassMeans = NULL
    for (class in unique(optimalClasses())) {
      optimalClassMeans = cbind(optimalClassMeans,
                                df.hedo()[, which(optimalClasses() == class)] %>%
                                  as.data.frame() %>% rowMeans())
    }
    rownames(optimalClassMeans) = rownames(df.hedo())
    return(optimalClassMeans)
  })
  
  optimalClassMeansText = reactive({
    paste("Average Score", round(optimalClassMeans(), 3)) %>% matrix(nrow = nrow(optimalClassMeans()))
  })
  output$optimalClassCharac = renderPlotly({
    plot_ly(
      x = as.factor(unique(isolate(
        optimalClasses()
      ))),
      y = rownames(isolate(optimalClassMeans())),
      z = isolate(optimalClassMeans()),
      type = "heatmap",
      source = "heatplot",
      xgap = 5,
      ygap = 3,
      hoverinfo = "text",
      text = isolate(optimalClassMeansText())
    ) %>%
      layout(xaxis = list(title = "", dtick = 1),
             yaxis = list(title = ""))
  })
  
  optiClicked <- reactive({
    event_data("plotly_click", source = "heatplot")
  })
  
  observeEvent(optiClicked(), {
    table = t(
      getPCA(
        df.senso(),
        input$sensoProduct,
        input$sensoJudge,
        input$sensoSession
      )$X[unlist(optiClicked()[["pointNumber"]])[1] + 1, -1]
    ) %>%
      round(3) %>%
      as.data.frame() %>%
      rownames_to_column(var = paste(optiClicked()[["y"]], "Characteristics"))
    colnames(table)[2] = "Average Judge Score"
    showModal(modalDialog(easyClose = T, renderDataTable(table[order(table[, 2], decreasing =
                                                                       T),])))
    
  })
}
