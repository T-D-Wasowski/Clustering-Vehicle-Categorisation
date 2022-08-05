library("readxl")
library("ggplot2")
library("factoextra")
library("NbClust")
library("fpc")
library("MASS")
library("flexclust")
library("caret") 

#----------------------- Functions -----------------------------

#Function used for min-max normalisation
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

#A method which uses scaled data to remove outliers from original, NON-SCALED data
removeOutliers <- function(df, dfz, x) {
  
  
  for (i in 1:18) {
    df <- df[dfz[[i]] < x & dfz[[i]] > -x, ]
    dfz <- dfz[dfz[[i]] < x & dfz[[i]] > -x, ]
  }

  return(df)
  
}

#A method which removes outliers from already SCALED data
scaleAndRemoveOutliers <- function(dfz, x) {
  
  for (i in 1:18) {
    dfz <- dfz[dfz[[i]] < x & dfz[[i]] > -x, ]
  }
  
  return(dfz)
  
}

#The elbow method used to determine optimal no. of clusters
elbowMethod <- function(df) {
  
  wss <- 0
  for (i in 1:10){
    wss[i] <-
      sum(kmeans(df, centers=i)$withinss)
  }
  plot(1:10,
       wss,
       type="b",
       xlab="Number of Clusters",
       ylab="Within Groups Sum of Squares")
  
}


createBoxplot <- function(df, mtitle, ytitle, xtitle) {
  
  boxplot(df,
          main = mtitle,
          ylab = ytitle,
          xlab = xtitle)
  
}

#--------------------- Initial Data Frames ------------------------------------

vehicles = read_excel("vehicles2.xlsx")   

#Calculate Z scores for each value
vehiclesZScores <- as.data.frame(scale(vehicles[2:19]))
vehiclesZScoresWithClass <- vehiclesZScores
vehiclesZScoresWithClass$Class <- vehicles$Class

#MinMax Normalisation
vehiclesNormalised <- as.data.frame(lapply(vehicles[2:19], normalise))

#--------------------- Calling method to remove outliers ----------------------

#--- Scaling BEFORE removing outliers:

#2 standard deviations
vehiclesORZ_sd2_as <- scaleAndRemoveOutliers(vehiclesZScores, 2)

#3 standard deviations
vehiclesORZ_sd3_as <- scaleAndRemoveOutliers(vehiclesZScores, 3)

#--- Scaling AFTER removing outliers:

#2 standard deviations
vehiclesOR_sd2 <- removeOutliers(vehicles[2:20], vehiclesZScoresWithClass, 2)
vehiclesORZ_sd2 <- as.data.frame(scale(vehiclesOR_sd2[1:18]))

#3 standard deviations - iteration 1
vehiclesOR_sd3 <- removeOutliers(vehicles[2:20], vehiclesZScoresWithClass, 3)
vehiclesORZ_sd3 <- as.data.frame(scale(vehiclesOR_sd3[1:18]))
vehiclesORZ_sd3_WithClass <- vehiclesORZ_sd3
vehiclesORZ_sd3_WithClass$Class <- vehiclesOR_sd3$Class

#3 standard deviations - iteration 2
vehiclesOR2_sd3 <- removeOutliers(vehiclesOR_sd3, vehiclesORZ_sd3_WithClass, 3)
vehiclesORZ2_sd3 <- as.data.frame(scale(vehiclesOR2_sd3[1:18]))

#--------------------- Defining K (No. of Clusters) ---------------------------

#--- No Outliers Removed:

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesZScores)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesZScores, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  geom_vline(xintercept = 7, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesZScores, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesZScores, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indices Method
set.seed(100)
ncZ <- NbClust(data = vehiclesZScores, distance = "euclidean",
                        min.nc = 2, max.nc = 15, method = "kmeans")

#--- Outliers Removed AFTER Scaling 2sd

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesORZ_sd2_as)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesORZ_sd2_as, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesORZ_sd2_as, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesORZ_sd2_as, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indices Method
set.seed(100)
ncORZ_sd2_as <- NbClust(data = vehiclesORZ_sd2_as, distance = "euclidean",
                      min.nc = 2, max.nc = 15, method = "kmeans")

#--- Outliers Removed AFTER Scaling 3sd

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesORZ_sd3_as)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesORZ_sd3_as, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesORZ_sd3_as, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesORZ_sd3_as, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indices Method
set.seed(100)
ncORZ_sd3_as <- NbClust(data = vehiclesORZ_sd3_as, distance = "euclidean",
                      min.nc = 2, max.nc = 15, method = "kmeans")

#--- Outliers Removed BEFORE Scaling 2sd

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesORZ_sd2)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesORZ_sd2, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesORZ_sd2, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesORZ_sd2, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indices Method
set.seed(100)
ncORZ_sd2 <- NbClust(data = vehiclesORZ_sd2, distance = "euclidean",
                      min.nc = 2, max.nc = 15, method = "kmeans")

#--- Outliers Removed BEFORE Scaling 3sd (1 Iteration)

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesORZ_sd3)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesORZ_sd3, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesORZ_sd3, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesORZ_sd3, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indices Method
set.seed(100)
ncORZ_sd3 <- NbClust(data = vehiclesORZ_sd3, distance = "euclidean",
                      min.nc = 2, max.nc = 15, method = "kmeans")

#--- Outliers Removed BEFORE Scaling 3sd (2 Iterations)

#- Manual Methods:

#Elbow method (Written by me)
elbowMethod(vehiclesORZ2_sd3)
#Eblow method alternate (Imported)
fviz_nbclust(vehiclesORZ2_sd3, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

#Silhouette method
fviz_nbclust(vehiclesORZ2_sd3, kmeans, method = "silhouette")

#- Automated Methods:

#Gap Statistic method
set.seed(100)
fviz_nbclust(vehiclesORZ2_sd3, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)

#30 Indecies Method
set.seed(100)
ncORZ2_sd3 <- NbClust(data = vehiclesORZ2_sd3, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

#--------------------- K-Means Analysis & Visualisation ---------------------------------------

#--- No Outliers Removed:

#- K = 2
set.seed(100)
fit2_ORZ <- kmeans(vehiclesZScores, 2)
plotcluster(vehiclesZScores, fit2_ORZ$cluster)
parcoord(vehiclesZScores, fit2_ORZ$cluster)

#- K = 3
set.seed(100)
fit3_ORZ <- kmeans(vehiclesZScores, 3)
plotcluster(vehiclesZScores, fit3_ORZ$cluster)
parcoord(vehiclesZScores, fit3_ORZ$cluster)

#- K = 4
set.seed(100)
fit4_ORZ <- kmeans(vehiclesZScores, 4)
plotcluster(vehiclesZScores, fit4_ORZ$cluster)
parcoord(vehiclesZScores, fit4_ORZ$cluster)

#--- Outliers Removed AFTER Scaling 2sd

#- K = 2
set.seed(100)
fit2_ORZ_2sd_as <- kmeans(vehiclesORZ_sd2_as, 2)
plotcluster(vehiclesORZ_sd2_as, fit2_ORZ_2sd_as$cluster)
parcoord(vehiclesORZ_sd2_as, fit2_ORZ_2sd_as$cluster)

#- K = 3
set.seed(100)
fit3_ORZ_2sd_as <- kmeans(vehiclesORZ_sd2_as, 3)
plotcluster(vehiclesORZ_sd2_as, fit3_ORZ_2sd_as$cluster)
parcoord(vehiclesORZ_sd2_as, fit3_ORZ_2sd_as$cluster)

#- K = 4
set.seed(100)
fit4_ORZ_2sd_as <- kmeans(vehiclesORZ_sd2_as, 4)
plotcluster(vehiclesORZ_sd2_as, fit4_ORZ_2sd_as$cluster)
parcoord(vehiclesORZ_sd2_as, fit4_ORZ_2sd_as$cluster)

#--- Outliers Removed AFTER Scaling 3sd

#- K = 2
set.seed(100)
fit2_ORZ_3sd_as <- kmeans(vehiclesORZ_sd3_as, 2)
plotcluster(vehiclesORZ_sd3_as, fit2_ORZ_3sd_as$cluster)
parcoord(vehiclesORZ_sd3_as, fit2_ORZ_3sd_as$cluster)

#- K = 3
set.seed(100)
fit3_ORZ_3sd_as <- kmeans(vehiclesORZ_sd3_as, 3)
plotcluster(vehiclesORZ_sd3_as, fit3_ORZ_3sd_as$cluster)
parcoord(vehiclesORZ_sd3_as, fit3_ORZ_3sd_as$cluster)

#- K = 4
set.seed(100)
fit4_ORZ_3sd_as <- kmeans(vehiclesORZ_sd3_as, 4)
fit4_ORZ_3sd_as
fit4_ORZ_3sd_as$centers
fit4_ORZ_3sd_as$size

plotcluster(vehiclesORZ_sd3_as, fit4_ORZ_3sd_as$cluster)
parcoord(vehiclesORZ_sd3_as, fit4_ORZ_3sd_as$cluster)

#--- Outliers Removed BEFORE Scaling 2sd

#- K = 2
set.seed(100)
fit2_ORZ_2sd <- kmeans(vehiclesORZ_sd2, 2)
plotcluster(vehiclesORZ_sd2, fit2_ORZ_2sd$cluster)
parcoord(vehiclesORZ_sd2, fit2_ORZ_2sd$cluster)

#- K = 3
set.seed(100)
fit3_ORZ_2sd <- kmeans(vehiclesORZ_sd2, 3)
plotcluster(vehiclesORZ_sd2, fit3_ORZ_2sd$cluster)
parcoord(vehiclesORZ_sd2, fit3_ORZ_2sd$cluster)

#- K = 4
set.seed(100)
fit4_ORZ_2sd <- kmeans(vehiclesORZ_sd2, 4)
plotcluster(vehiclesORZ_sd2, fit4_ORZ_2sd$cluster)
parcoord(vehiclesORZ_sd2, fit4_ORZ_2sd$cluster)

#--- Outliers Removed BEFORE Scaling 3sd (1 Iteration)

#- K = 2
set.seed(100)
fit2_ORZ_3sd <- kmeans(vehiclesORZ_sd3, 2)
plotcluster(vehiclesORZ_sd3, fit2_ORZ_3sd$cluster)
parcoord(vehiclesORZ_sd3, fit2_ORZ_3sd$cluster)

#- K = 3
set.seed(100)
fit3_ORZ_3sd <- kmeans(vehiclesORZ_sd3, 3)
plotcluster(vehiclesORZ_sd3, fit3_ORZ_3sd$cluster)
parcoord(vehiclesORZ_sd3, fit3_ORZ_3sd$cluster)

#- K = 4
set.seed(100)
fit4_ORZ_3sd <- kmeans(vehiclesORZ_sd3, 4)
plotcluster(vehiclesORZ_sd3, fit4_ORZ_3sd$cluster)
parcoord(vehiclesORZ_sd3, fit4_ORZ_3sd$cluster)

#--- Outliers Removed BEFORE Scaling 3sd (2 Iterations)

#- K = 2
set.seed(100)
fit2_ORZ2_3sd <- kmeans(vehiclesORZ2_sd3, 2)
plotcluster(vehiclesORZ2_sd3, fit2_ORZ2_3sd$cluster)
parcoord(vehiclesORZ2_sd3, fit2_ORZ2_3sd$cluster)

#- K = 3
set.seed(100)
fit3_ORZ2_3sd <- kmeans(vehiclesORZ2_sd3, 3)
plotcluster(vehiclesORZ2_sd3, fit3_ORZ2_3sd$cluster)
parcoord(vehiclesORZ2_sd3, fit3_ORZ2_3sd$cluster)

#- K = 4
set.seed(100)
fit4_ORZ2_3sd <- kmeans(vehiclesORZ2_sd3, 4)
plotcluster(vehiclesORZ2_sd3, fit4_ORZ2_3sd$cluster)
parcoord(vehiclesORZ2_sd3, fit4_ORZ2_3sd$cluster)

#--------------------- Cluster Evaluation -------------------------------------

#--- No Outliers Removed:

#- K = 2
ct_fit2_ORZ <- table(vehicles$Class, fit2_ORZ$cluster)
ct_fit2_ORZ
randIndex(ct_fit2_ORZ)

#- K = 3
ct_fit3_ORZ <- table(vehicles$Class, fit3_ORZ$cluster)
ct_fit3_ORZ
randIndex(ct_fit3_ORZ)

#- K = 4
ct_fit4_ORZ <- table(fit4_ORZ$cluster, vehicles$Class)
ct_fit4_ORZ
randIndex(ct_fit4_ORZ)

confusionMatrix(ct_fit4_ORZ, mode = "everything") 
  

#--- Outliers Removed AFTER Scaling 2sd

#- K = 2
ct_fit2_ORZ_2sd_as <- table(vehiclesOR_sd2$Class, fit2_ORZ_2sd_as$cluster)
ct_fit2_ORZ_2sd_as
randIndex(ct_fit2_ORZ_2sd_as)

#- K = 3
ct_fit3_ORZ_2sd_as <- table(vehiclesOR_sd2$Class, fit3_ORZ_2sd_as$cluster)
ct_fit3_ORZ_2sd_as
randIndex(ct_fit3_ORZ_2sd_as)

#- K = 4
ct_fit4_ORZ_2sd_as <- table(vehiclesOR_sd2$Class, fit4_ORZ_2sd_as$cluster)
ct_fit4_ORZ_2sd_as
randIndex(ct_fit4_ORZ_2sd_as)

confusionMatrix(ct_fit4_ORZ_2sd_as,  mode = "everything") 

#--- Outliers Removed AFTER Scaling 3sd

#- K = 2
ct_fit2_ORZ_3sd_as <- table(vehiclesOR_sd3$Class, fit2_ORZ_3sd_as$cluster)
ct_fit2_ORZ_3sd_as
randIndex(ct_fit2_ORZ_3sd_as)

fviz_cluster(fit2_ORZ_3sd_as, vehiclesOR_sd3[,-1], geom = "point") + ggtitle("K = 2")

#- K = 3
ct_fit3_ORZ_3sd_as <- table(vehiclesOR_sd3$Class, fit3_ORZ_3sd_as$cluster)
ct_fit3_ORZ_3sd_as
randIndex(ct_fit3_ORZ_3sd_as)

fviz_cluster(fit3_ORZ_3sd_as, vehiclesOR_sd3[,-1], geom = "point") + ggtitle("K = 3")

#- K = 4
ct_fit4_ORZ_3sd_as <- table(vehiclesOR_sd3$Class, fit4_ORZ_3sd_as$cluster)
ct_fit4_ORZ_3sd_as
randIndex(ct_fit4_ORZ_3sd_as)

confusionMatrix(ct_fit4_ORZ_3sd_as,  mode = "everything")

fviz_cluster(fit4_ORZ_3sd_as, vehiclesOR_sd3[,-1], geom = "point") + ggtitle("K = 4")

#--- Outliers Removed BEFORE Scaling 2sd

#- K = 2
ct_fit2_ORZ_2sd <- table(vehiclesOR_sd2$Class, fit2_ORZ_2sd$cluster)
ct_fit2_ORZ_2sd
randIndex(ct_fit2_ORZ_2sd)

#- K = 3
ct_fit3_ORZ_2sd <- table(vehiclesOR_sd2$Class, fit3_ORZ_2sd$cluster)
ct_fit3_ORZ_2sd
randIndex(ct_fit3_ORZ_2sd)

#- K = 4
ct_fit4_ORZ_2sd <- table(vehiclesOR_sd2$Class, fit4_ORZ_2sd$cluster)
ct_fit4_ORZ_2sd
randIndex(ct_fit4_ORZ_2sd)

confusionMatrix(ct_fit4_ORZ_2sd,  mode = "everything")

#--- Outliers Removed BEFORE Scaling 3sd (1 Iteration)

#- K = 2
ct_fit2_ORZ_3sd <- table(vehiclesOR_sd3$Class, fit2_ORZ_3sd$cluster)
ct_fit2_ORZ_3sd
randIndex(ct_fit2_ORZ_3sd)

#- K = 3
ct_fit3_ORZ_3sd <- table(vehiclesOR_sd3$Class, fit3_ORZ_3sd$cluster)
ct_fit3_ORZ_3sd
randIndex(ct_fit3_ORZ_3sd)

#- K = 4
ct_fit4_ORZ_3sd <- table(vehiclesOR_sd3$Class, fit4_ORZ_3sd$cluster)
ct_fit4_ORZ_3sd
randIndex(ct_fit4_ORZ_3sd)

confusionMatrix(ct_fit4_ORZ_3sd,  mode = "everything")

#--- Outliers Removed BEFORE Scaling 3sd (2 Iterations)


#- K = 2
ct_fit2_ORZ2_3sd <- table(vehiclesOR2_sd3$Class, fit2_ORZ2_3sd$cluster)
ct_fit2_ORZ2_3sd
randIndex(ct_fit2_ORZ2_3sd)

#- K = 3
ct_fit3_ORZ2_3sd <- table(vehiclesOR2_sd3$Class, fit3_ORZ2_3sd$cluster)
ct_fit3_ORZ2_3sd
randIndex(ct_fit3_ORZ2_3sd)

#- K = 4
ct_fit4_ORZ2_3sd <- table(vehiclesOR2_sd3$Class, fit4_ORZ2_3sd$cluster)
ct_fit4_ORZ2_3sd
randIndex(ct_fit4_ORZ2_3sd)

confusionMatrix(ct_fit4_ORZ2_3sd, mode = "everything")

#--------------------- Plots --------------------------------------------------

#--- Outlier Removal:

#Standard & Scaled data
createBoxplot(vehicles[2:19], "Original Vehicle Data", "Observation Count", "Variables")
createBoxplot(vehiclesZScores, "Scaled Vehicle Data (Z-Score)", "Z-Score", "Variables")

#Outliers removed after scaling
createBoxplot(vehiclesORZ_sd2_as, "Outliers Removed AFTER Scaling (+/- 2sd)", "Z-Score", "Variables")
createBoxplot(vehiclesORZ_sd3_as, "Outliers Removed AFTER Scaling (+/- 3sd)", "Z-Score", "Variables")

#Outliers removed before scaling
createBoxplot(vehiclesORZ_sd2, "Outliers Removed BEFORE Scaling (+/- 2sd)", "Z-Score", "Variables")
createBoxplot(vehiclesORZ_sd3, "Outliers Removed BEFORE Scaling (+/- 3sd)", "Z-Score", "Variables")
createBoxplot(vehiclesORZ2_sd3, "Outliers Removed BEFORE Scaling (+/- 3sd, 2i)", "Z-Score", "Variables")


#--- Model Fitting (Determine no. of Clusters):

#30 Indices for scaled with no outliers removed.
barplot(table(ncZ$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "No. of Clusters Chosen by 30 Criteria - No Outlier Removal")

#30 Indices for 2sd (Outliers removed AFTER scaling)
barplot(table(ncORZ_sd2_as$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "No. of Clusters Chosen by 30 Criteria - Outliers Removed AFTER scaling at +/- 2sd")

#30 Indices for 3sd (Outliers removed AFTER scaling)
barplot(table(ncORZ_sd3_as$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "No. of Clusters Chosen by 30 Criteria - Outliers Removed AFTER scaling at +/- 3sd")

#30 Indices for 2sd (Outliers removed BEFORE scaling)
barplot(table(ncORZ_sd2$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "No. of Clusters Chosen by 30 Criteria - Outliers Removed BEFORE scaling at +/- 2sd")

#30 Indices for 3sd (i1) (Outliers removed BEFORE scaling)
barplot(table(ncORZ_sd3$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "K Chosen by 30 Indices - Outliers Removed BEFORE scaling, +/- 3sd")

#30 Indices for 3sd (i2) (Outliers removed BEFORE scaling)
barplot(table(ncORZ2_sd3$Best.n[1,]),
        xlab = "No. of Clusters",
        ylab = "No. of Criteria",
        main = "No. of Clusters Chosen by 30 Criteria - Outliers Removed BEFORE scaling at +/- 3sd, i2")


#Misc:

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
