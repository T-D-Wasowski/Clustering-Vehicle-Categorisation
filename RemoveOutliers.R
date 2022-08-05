library("readxl")
library("ggplot2")

#----------------------- Functions -----------------------------

normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#This function displays the z-score distribution and displays outliers
plotZScoreOutliers <- function(df, zscore) {
  
  ggplot(df, aes(zscore)) +
    geom_histogram(aes(x=zscore, y=..count..), bins = 50) +
    geom_vline(xintercept = 3, color = "red") +
    geom_vline(xintercept = -3, color = "red")
  
}

removeOutliers <- function(df, dfz) {
  
  for (i in 1:18) {
    df <- df[dfz[[i]] < 3 & dfz[[i]] > -3, ]
    dfz <- dfz[dfz[[i]] < 3 & dfz[[i]] > -3, ]
  }
  
  return(df)
  
}

#--------------------- Initial Data Frames ------------------------------------

vehicles = read_excel("vehicles.xlsx")   

#Calculate Z scores for each value
vehiclesZScores <- as.data.frame(scale(vehicles[2:19]))

#MinMax Normalisation
vehiclesNormalised <- as.data.frame(lapply(vehicles[2:19], normalise))

#--------------------- Calling method to remove outliers ----------------------

#First iteration
vehiclesOR <- removeOutliers(vehicles[2:19], vehiclesZScores)
vehiclesORZ <- as.data.frame(scale(vehiclesOR))

#Second iteration
vehiclesOR2 <- removeOutliers(vehiclesOR, vehiclesORZ)
vehiclesORZ2 <- as.data.frame(scale(vehiclesOR2))

#--------------------- Plots --------------------------------------------------

boxplot(vehicles[2:19])
boxplot(vehiclesZScores)
boxplot(vehiclesORZ)
boxplot(vehiclesORZ2)

plotZScoreOutliers(vehiclesORZ2, vehiclesORZ2$Sc.Var.Maxis)
plotZScoreOutliers(vehiclesORZ, vehiclesORZ$Max.L.Ra)

plot(vehicles$Max.L.Ra, type = "p")
plot(vehiclesORZ$Max.L.Ra, type = "p")

plotZScoreOutliers(vehiclesZScores, vehiclesZScores$Comp)
plotZScoreOutliers(vehiclesZScores, vehiclesZScores$Circ)
plotZScoreOutliers(vehiclesZScores, vehiclesZScores$Max.L.Ra)
plotZScoreOutliers(vehiclesZScores, vehiclesZScores$Sc.Var.Maxis)
plotZScoreOutliers(vehiclesZScores, vehiclesZScores$Max.L.Rect)

plot(vehiclesZScores$Max.L.Rect, type="o", col="blue")

plot(vehicles$Max.L.Ra, type = "p")
plot(vehiclesZScores$Max.L.Ra, type = "p")

plot(vehiclesNormalised$Comp, type = "p")
boxplot(vehicles$Comp, ylab = "Max.L.Ra")
