library(ggplot2)
library(viridis)
library(doBy)
library(FactoMineR)
library(reshape2)
library(plyr)
library(tibble)
library(metR)
library(fields)

getPCA = function(df.senso) {
  df.X = summaryBy(
    . ~ produit,
    data = df.senso[, -c(1:2)],
    FUN = c(mean),
    na.rm = T,
    keep.names = T
  )
  res.pca = PCA(df.X[, -1], graph = F)
  return(res.pca)
}

mapWithPCA = function(df.senso, df.hedo, x = 2) {
  obj.pca = getPCA(df.senso)
  pcs = obj.pca$ind$coord[, 1:x]
  colnames(pcs) = paste0('PC', seq(x))
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

#refactor with outer
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
                   trim.values=T,
                   type = "prediction",
                   plot.contour = F,
                   contour.step = 0.25,
                   contour.col = "white",
                   prod.col = "white",
                   show.prods = F,
                   prod.points = F,
                   interpolate=T,
                   nbpoints=50,
                   plot.3D = F) {
  legendBreaks =  if (max(predictedScore) == 1) c(0, 1) else seq(0, 100, by = 10)
  contourBreaks = seq(to = 100, by = contour.step)
  if (type != "preference") {
    if(trim.values){
      predictedScore[predictedScore > 10] = 10
      predictedScore[predictedScore < 0] = 0
    }
    legendBreaks = if(diff(range(predictedScore))<2) seq(0,10) else seq(0,10, by=2)
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
  
  if(prod.points){
    plot = plot + geom_point(
      data = mapBisc,
      aes(
        x = PC1,
        y = PC2
      ),
      color = prod.col
    )
    vjust=-0.5
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
      vjust=vjust
    )
  }
  if(plot.3D){
    image=as.image(Z=predictedScore,x=discreteSpace,nx=nbpoints,ny=nbpoints)
    plot = plot_ly(x=image$x,y=image$y,z = image$z, type="surface",height = 600)
  }
  
  return(plot)
}



df.hedo = read.csv("hedo.csv", sep = ';', row.names = 1)
df.senso = read.csv("senso.csv")
mapBisc = mapWithPCA(df.senso, df.hedo)
fittedModels = fitModel(mapBisc, formula = "Quadratic")
discreteSpace = makeGrid(mapBisc, 50)



predictedScores = sapply(fittedModels, predict, newdata = discreteSpace) %>% as.data.frame()
preferences = mapply(function(x, y) {
  as.numeric(x > mean(y))
}, predictedScores, df.hedo) %>% as.data.frame()


plotMap(predictedScores[[1]], mapBisc, discreteSpace, plot.contour = F)
plotMap(preferences[[1]], mapBisc, discreteSpace, type="preference",interpolate=F)

averagePredictedScores = predictedScores %>% rowMeans()
percentagePreferences = 100 * preferences %>% rowMeans()

plotMap(
  averagePredictedScores,
  mapBisc,
  discreteSpace,
  plot.contour = T,
  show.prods = F
)


plotMap(
  percentagePreferences,
  mapBisc,
  discreteSpace,
  type = "preference",
  plot.contour = T,
  contour.step = 6,
  show.prods = T,
  interpolate = T
)


