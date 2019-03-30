library(shiny)
library(argonR)
library(argonDash)
library(shinycssloaders)
library(DT)
library(factoextra)
library(shinyjs)
library(shinyalert)
library(tools)
library(tibble)
library(colourpicker)
library(ggplot2)
library(viridis)
library(doBy)
library(FactoMineR)
library(reshape2)
library(plyr)
library(tibble)
library(metR)
library(fields)
library(plotly)
library(cluster)
library(kohonen)

getPCA = function(df.senso, product, judge, session) {
  df.X = summaryBy(
    as.formula(paste(".", product, sep = "~")),
    data =  df.senso[, -which(names(df.senso) %in% c(judge, session))],
    FUN = c(mean),
    na.rm = T,
    keep.names = T
  )
  res.pca = PCA(df.X[, -1], graph = F)
  return(list(PCA = res.pca, X = df.X))
}

mapWithPCA = function(df.senso, df.hedo, product, judge, session) {
  obj.pca = getPCA(df.senso, product, judge, session)$PCA
  pcs = obj.pca$ind$coord[, 1:2]
  colnames(pcs) = paste0('PC', seq(2))
  map = cbind.data.frame(df.hedo, pcs)
  return(map)
}

fitModel = function(map,
                    x = 2,
                    model = "lm",
                    formula = "Quadratic") {
  switch(
    formula,
    "Vector" = {
      formula = "PC1+PC2"
    },
    "Circular" = {
      formula = "PC1+PC2+I(PC1*PC1+PC2*PC2)"
    },
    "Elliptic" = {
      formula = "PC1+PC2+I(PC1*PC1)+I(PC2*PC2)"
    },
    "Quadratic" = {
      formula = "PC1+PC2+I(PC1*PC1)+I(PC2*PC2)+PC1:PC2"
    },
    stop("Invalid formula.")
  )
  
  modelFormula = as.formula(paste("Score", formula, sep = "~"))
  
  mapTidy = map %>% melt(
    id = paste0('PC', seq(x)),
    value.name = "Score",
    variable.name = "Consumer"
  )
  mapTidy$Consumer = factor(mapTidy$Consumer, levels = unique(mapTidy$Consumer))
  if (model == "lm") {
    fittedModels = dlply(mapTidy, .(Consumer), function(x) {
      lm(modelFormula, data = x)
    })
  } else{
    if (model == "glm") {
      fittedModels = dlply(mapTidy, .(Consumer), function(x) {
        glm(modelFormula, data = x)
      })
    }
    else{
      stop("Invalid model")
    }
  }
  return(fittedModels)
}



predictionQuality = function(predictedScores) {
  if (!is.null(dim(predictedScores))) {
    outOfBoundValueCount = predictedScores %>% apply(
      MARGIN = 2,
      FUN = function(x) {
        length(na.exclude(x[x > 9])) + length(na.exclude(x[x < 1]))
      }
    ) %>% sum()
    total = ncol(predictedScores) * nrow(predictedScores)
  }
  else{
    outOfBoundValueCount = length(predictedScores[predictedScores > 9]) + length(predictedScores[predictedScores < 1])
    total = length(predictedScores)
  }
  
  msg = paste(
    "There are",
    outOfBoundValueCount,
    "predicted values outside the score range, i.e.",
    paste0(round(100 * outOfBoundValueCount / (total), 2), "%.")
  )
  message(msg)
  return(msg)
}


makeGrid = function(X, nbpoints = 50) {
  xMin = floor(min(X$PC1))
  yMin = floor(min(X$PC2))
  xMax = ceiling(max(X$PC1))
  yMax = ceiling(max(X$PC2))
  
  pc1 = seq(from = xMin,
            to = xMax,
            length.out = nbpoints)
  pc2 = seq(from = yMin,
            to = yMax,
            length.out = nbpoints)
  
  return(expand.grid(PC1 = pc1, PC2 = pc2))
  
}

plotMap = function(predictedScore,
                   mapBisc,
                   discreteSpace,
                   plot.type = "prediction",
                   plot.contour = F,
                   contour.step = 0.25,
                   contour.col = "white",
                   prod.col = "white",
                   show.prods = F,
                   prod.points = F,
                   interpolate = T,
                   nbpoints = 50,
                   plot.3D = F) {
  legendBreaks =  if (max(predictedScore) == 1)
    c(0, 1)
  else
    seq(0, 100, by = 10)
  contourBreaks = seq(to = 100, by = contour.step)
  if (plot.type != "preference") {
    legendBreaks = if (diff(range(predictedScore)) < 2)
      seq(0, 10)
    else
      seq(0, 10, by = 2)
    contourBreaks = seq(to = 10, by = contour.step)
  }
  plot = ggplot(cbind(discreteSpace, predictedScore), aes(x = PC1, y = PC2)) +
    geom_raster(aes(fill = predictedScore), interpolate = interpolate) +
    scale_fill_viridis(breaks = legendBreaks) +
    scale_x_continuous(breaks = seq(min(discreteSpace$PC1), max(discreteSpace$PC1))) +
    scale_y_continuous(breaks = seq(min(discreteSpace$PC2), max(discreteSpace$PC2))) +
    coord_fixed(expand = FALSE) +
    theme(legend.title = element_blank())
  
  if (plot.contour) {
    plot = plot +
      geom_contour(aes(z = predictedScore),
                   colour = contour.col,
                   breaks = contourBreaks) +
      geom_text_contour(
        aes(z = predictedScore),
        colour = contour.col,
        vjust = -0.5,
        breaks = contourBreaks
      )
  }
  
  vjust = 0
  
  if (prod.points) {
    plot = plot + geom_point(data = mapBisc,
                             aes(x = PC1,
                                 y = PC2),
                             color = prod.col)
    vjust = -0.5
  }
  if (show.prods) {
    plot = plot + geom_text(
      data = mapBisc,
      aes(
        x = PC1,
        y = PC2,
        label = rownames(mapBisc)
      ),
      color = prod.col,
      size = 5,
      vjust = vjust
    )
  }
  if (plot.3D) {
    image = as.image(Z = predictedScore,
                     x = discreteSpace,
                     nx = nbpoints,
                     ny = nbpoints)
    plot = plot_ly(
      x = image$x,
      y = image$y,
      z = image$z,
      type = "surface",
      height = 600
    )
  }
  
  return(plot)
}