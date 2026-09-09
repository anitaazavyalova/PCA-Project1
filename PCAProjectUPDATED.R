library(ISLR)
library(tidyverse)

# Read the file
my_data <- read.csv("mstar_data.csv")

# Preview the first few rows
head(my_data)

# Getting the means and variances
apply(my_data, 2, mean)
apply(my_data, 2, var)

# Standardizing the data, the computing the covariance matrix
data_std=scale(my_data)

cov_matrix = cov(data_std)
cov_matrix

<<<<<<< HEAD
# Computing eigenvalues and vectors to be able to see how the variables
# move and at what scale
=======
#This is where we get the eigenvalues and vectors for the data
>>>>>>> 902e2bd0e6fe8155094a79fb3013573b4b3eb19f
eigen_decomp = eigen(cov_matrix)
Eigenvalues = eigen_decomp$values
Eigenvectors = eigen_decomp$vectors

#Eigen-proportion
Eigenprop = Eigenvalues /sum(Eigenvalues)
round(Eigenprop,3)

Eigenvectors

<<<<<<< HEAD
# Computing loadings, which is the weight at which variables 
# contribute to a PC 
=======
#Loadings are the coefficients that give linear combinations from the original data
>>>>>>> 902e2bd0e6fe8155094a79fb3013573b4b3eb19f
loadings = Eigenvectors %>% 
  data.frame(row.names = colnames(my_data)) %>%
  rename("PC1" = X1, "PC2" = X2, "PC3" = X3, "PC4" = X4,
         "PC5" = X5, "PC6" = X6, "PC7" = X7, "PC8" = X8,
         "PC9" = X9, "PC10" = X10, "PC11" = X11, "PC12" = X12,
         "PC13" = X13, "PC14" = X14, "PC15" = X15, "PC16" = X16) %>%
  round(digits = 3)

loadings

<<<<<<< HEAD
# Computing the Proportion of Variance which is the percentage of 
# the original MSTAR data that is retained by each principal component. 

pve = 100 * Eigenprop / sum(Eigenprop)

# Finding the minimum number of PC's needed to account for 85% of the
# variability in the data 
min(which(cumsum(pve) >= 85))


# Creating plots for visibility
=======
#Proportion of Variance Explained
#Captures total variance within the dataset.

pve = 100 * Eigenprop / sum(Eigenprop)
# We can look at a plot to get the percentages of variance/unique values in the dataset
>>>>>>> 902e2bd0e6fe8155094a79fb3013573b4b3eb19f
par(mfrow = c(1, 2))
plot(pve, type = "b", ylab = "PVE",
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "b", ylab = "Cumulative PVE",
     xlab = "Principal Component", col = "brown3")


PCscores = data_std %*% Eigenvectors 
head(PCscores)

# PC scores are orthogonal to each other:
cor(PCscores)

biplot(PCscores[,1:2], loadings[,1:2], 
       xlab = "PC1 (62.0%)", ylab="PC2 (24.7%)", main="Recast data with loadings",
       cex=.5)

biplot(-PCscores[,1:2], -loadings[,1:2],
       xlab = sprintf("PC1: %.1f%%", pve[1]), ylab = sprintf("PC2: %.1f%%", pve[2]),
       main="Recast data with loadings", cex=.5)