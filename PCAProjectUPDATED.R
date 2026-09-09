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

#This is where we get the eigenvalues and vectors for the data
eigen_decomp = eigen(cov_matrix)
Eigenvalues = eigen_decomp$values
Eigenvectors = eigen_decomp$vectors

#Eigen-proportion
Eigenprop = Eigenvalues /sum(Eigenvalues)
round(Eigenprop,3)

Eigenvectors

#Loadings are the coefficients that give linear combinations from the original data
loadings = Eigenvectors %>% 
  data.frame(row.names = colnames(my_data)) %>%
  rename("PC1" = X1, "PC2" = X2, "PC3" = X3, "PC4" = X4) %>%
  round(digits = 3)

loadings

#Proportion of Variance Explained
#Captures total variance within the dataset.

pve = 100 * Eigenprop / sum(Eigenprop)
# We can look at a plot to get the percentages of variance/unique values in the dataset
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