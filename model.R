setwd("~/Documents/MSC/sem2/DM/program/phase3")
install.packages("MASS")
install.packages("plot3D")
install.packages("lattice")
install.packages("scatterplot3d")
library(scatterplot3d)
library(lattice)
library("plot3D")
library(MASS)
normalized_data <- read.csv("CENSUS_DATASET_NORMALISED.csv" , header = T , sep = ",")
View(normalized_data)

#====================================================================================================
y <- normalized_data$UrbanLess10F..Married.
y_test <- y[1:25]
y_predict <- y[26:35]

x <- normalized_data$Literate.Females.Urban.30.
x_test <- x[1:25]
x_predict <- x[26:35]

z <- normalized_data$Literate.Males.Urban.30.
z_test <- z[1:25]
z_predict <- z[26:35]

plot1 <- scatterplot3d(x_test, y_test, z_test, type = "h", color = "blue",
                       angle=55, pch = 16,
                       xlab = "Literate Female (30+ age)",
                       ylab = "Maarriage of female (under 10)",
                       zlab = "Literate Male (30+ age)")

#====================================================================================================
x_sqr_test <- seq(1,25,1)
x_sqr_predict <- seq(1,10,1)
for(i in 1:25)
  x_sqr_test[i] <- x_test[i]*x_test[i]
for(i in 1:10)
  x_sqr_predict[i] <- x_predict[i]*x_predict[i]
  
#////////////////calc : xz //////////////////////////////
x_z_test <- seq(1,25,1)
x_z_predict <- seq(1,10,1)
for(i in 1:25)
  x_z_test[i] <- x_test[i]*z_test[i]
for(i in 1:10)
  x_z_predict[i] <- x_predict[i]*z_predict[i]

#////////////////calc : z^2 //////////////////////////////
z_sqr_test <- seq(1,25,1)
z_sqr_predict <- seq(1,10,1)
for(i in 1:25)
  z_sqr_test[i] <- z_test[i]*z_test[i]
for(i in 1:10)
  z_sqr_predict[i] <- z_predict[i]*z_predict[i]

#////////////////calc : xz^2 //////////////////////////////
x_z_sqr_test <- seq(1,25,1)
x_z_sqr_predict <- seq(1,10,1)
for(i in 1:25)
  x_z_sqr_test[i] <- x_test[i]*z_sqr_test[i]
for(i in 1:10)
  x_z_sqr_predict[i] <- x_predict[i]*z_sqr_predict[i]

#////////////////calc : x^2z //////////////////////////////
x_sqr_z_test <- seq(1,25,1)
x_sqr_z_predict <- seq(1,10,1)
for(i in 1:25)
  x_sqr_z_test[i] <- x_sqr_test[i]*z_test[i]
for(i in 1:10)
  x_sqr_z_predict[i] <- x_sqr_predict[i]*z_predict[i]

#====================================================================================================
model1 <- lm(formula = y_test ~ x_test + z_test + x_sqr_z_test + x_z_sqr_test + x_z_test,
             data = normalized_data, drop.unused.levels = TRUE)

anova(model1)
result <- predict(model1, list(x_test = x_predict,
                               z_test = z_predict,
                               x_sqr_z_test = x_sqr_z_predict,
                               x_z_sqr_test = x_z_sqr_predict,
                               x_z_test = x_z_predict))

plot2 <- scatterplot3d(x_predict, y_predict, z_predict, type = "h", color = "green",
                       angle=55, pch = 16,
                       xlab = "Literate Female (30+ age)",
                       ylab = "Maarriage of female (under 10)",
                       zlab = "Literate Male (30+ age)")
# Add supplementary points
plot2$points3d(x_predict, result, z_predict,
             col = "red", type = "h", pch = 8)











