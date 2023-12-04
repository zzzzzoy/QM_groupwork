### import required packages
library(tidyverse)
library(dplyr)
library(janitor)  # the use of clean_names to clean the column name
library(car)  # linear regression plot
library(corrplot)  # correlation matrix plot
library(plotly)  # more subplots


### load the data
mycsv <- read_csv("AoLR Data collection 2017.csv", skip = 1)
### remove unnecessary rows and select topic columns, tidy the data
# drop the last n/a row
mycsv <- mycsv[-nrow(mycsv),]
# select the required columns
mycsv <- mycsv %>%
  clean_names() %>%
  mutate(fires_total = x1_pump_fires + x2_pump_fires) %>%
  dplyr::select(contains("ward_code"), contains("ward_name"), contains("fires_total"),
                contains("density"), contains("x65_age"), contains("hr_buildings"),
                contains("student"), contains("smokers"), contains("over_crowding"))



### linear regression
# linear regression model
reg <- lm(fires_total ~ density + x65_age +
            hr_buildings + student +
            smokers + over_crowding, mycsv)
summary(reg)

# linear regression plot
avPlots(reg, col='blue', main = 'Linear regression model for fires_total')
#plot(mycsv$density, mycsv$fires_total, col='green')


# linear regression prediction (80% 20% prediction)
mycsv8 <- mycsv[1:(nrow(mycsv)*0.8),]
mycsv2 <- mycsv[(nrow(mycsv)*0.8 + 1):nrow(mycsv),]
mycsv2_x <- mycsv2[,c(4:9)]  # mycsv2 x variables
# using mycsv8 (80% data) to do the regression
lm8 <- lm(fires_total ~ density + x65_age +
            hr_buildings + student +
            smokers + over_crowding, mycsv8)
summary(lm8)
# do the prediction
prediction <- predict(lm8, mycsv2_x, interval = "predict", level = 0.95)
# use the t-test to do a hypothesis test
t.test(mycsv2[,3], prediction[,1])



### correlation
# correlation matrix plot
# identify the numeric columns
numeric_col <- sapply(mycsv, is.numeric)
# make the correlation matrix
cormatrix <- mycsv[, numeric_col] %>%
  cor(use="pairwise.complete")
# the correlation clusters to make correlation matrix plot
corrplot(cormatrix, order="hclust",
         # the text style color and size
         tl.cex=0.8,
         tl.col="black",
         # remove the diagonal (the 1 entries)
         diag=FALSE,
         # illustrate the upper triangle
         type="upper",
         # margins size
         mar=c(0,0,0,0),
         # title name
         title="Correlation matrix")


# correlation analysis, select the moderate abs correlation pairs > 0.5
triangle <- upper.tri(cormatrix)  # locate the upper triangle of the matrix
high_cor_pairs <- which(abs(cormatrix) > 0.5 & triangle, arr.ind=TRUE)
corcoefficent <- cormatrix[high_cor_pairs]
# make a table
pairstable <- data.table(
  variable_1 = rownames(cormatrix)[high_cor_pairs[,1]],
  variable_2 = colnames(cormatrix)[high_cor_pairs[,2]],
  cor_coefficient = corcoefficent
)


# correlation scatter plot with regression line (abline, correlation)
# concerns vs the total fires
par(mfrow=c(2,3))
plot(x = mycsv$density, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$density), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")  
text(x = 20000, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$density, mycsv$fires_total), 3)))

plot(x = mycsv$x65_age, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$x65_age), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")
text(x = 2600, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$x65_age, mycsv$fires_total), 3)))

plot(x = mycsv$hr_buildings, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$hr_buildings), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")
text(x = 230, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$hr_buildings, mycsv$fires_total), 3)))

plot(x = mycsv$student, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$student), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")
text(x = 3500, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$student, mycsv$fires_total), 3)))

plot(x = mycsv$smokers, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$smokers), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")
text(x = 4000, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$smokers, mycsv$fires_total), 3)))

plot(x = mycsv$over_crowding, y = mycsv$fires_total, col="blue", cex=0.5, pch=19)
abline(lm(mycsv$fires_total ~ mycsv$over_crowding), col = "red", lwd = 3)
grid(nx = NULL, ny = NULL, lwd = 0.5, lty = 2, col = "gray")
text(x = 400, y = 200, cex=1.5, col='brown',
     paste("Correlation:", round(cor(mycsv$over_crowding, mycsv$fires_total), 3)))