# Clean enviroment ----
rm(list = ls())
gc()

# Load packages ----
library(broom) # Better summaries
library(FactoMineR) # PCA
library(factoextra) # ggplot2 PCA plots
library(magrittr) # %>%

# Load data ----
df.hedo = read.csv("hedo.csv",sep=';')
df.senso = read.csv("sens.csv")

# Convert seance, juge, produit to factors ----
df.senso$seance = as.factor(df.senso$seance)
df.senso$juge = as.factor(df.senso$juge)
df.senso$produit = as.factor(df.senso$produit)

# Consumer data will remain untouched for now ----
df.Y = df.hedo

# Agregate sensory data by averaging across session and judge by product. ----

# First column contains the product names
df.X = data.frame(produit = levels(df.senso$produit))
for (i in 4:dim(df.senso)[2]) {
  # Calculate the average score across different judges and sessions
  col = tapply(df.senso[, i], df.senso$produit, mean, na.rm = T)
  # Append the column to df.X
  df.X = cbind(df.X, col)
  # Rename the added column to that of the criterion used
  colnames(df.X)[i - 2] = colnames(df.senso)[i]
}
row.names(df.X) = df.Y$X

# Perform PCA on df.X and add x components to df.Y ----
x = 2
# Store the results
obj.pca = PCA(df.X[, -1], scale.unit = F,graph = F)
# Append the first x PCA components to df.Y
df.Y = cbind(df.Y, obj.pca$ind$coord[, 1:x])
# Clean added rownames
rownames(df.Y) = NULL



# Fit a linear model for each consumer using the x added components ----

# Get the names of the added componnets
exp_vars = colnames(df.Y)[(dim(df.Y)[2] - x + 1):dim(df.Y)[2]]
# Placeholder for the fitted models
objs.lm = NULL
for (i in 2:(dim(df.Y)[2] - x)) {
  # Get the name of the response variable, i.e. the consumer at hand
  res_var = colnames(df.Y)[i]
  # Use the names to create a formula
  formula = as.formula(paste(res_var, 
    paste(
    exp_vars = colnames(df.Y)[(dim(df.Y)[2] - x + 1):dim(df.Y)[2]],
    collapse = "+"),
                      sep = "~"))
  # Use the formula to fit the model
  model = lm(formula, data = df.Y)
  # Store the model as a list for easy access
  objs.lm = c(objs.lm, list(model))
  # Clear local variables
  rm(list=c("res_var", "formula", "model"))
}

glance(objs.lm[[1]])

sapply(objs.lm, function(x){return(x$coefficients)}) %>% t
