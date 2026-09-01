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

eigen_decomp = eigen(cov_matrix)
Eigenvalues = eigen_decomp$values
Eigenvectors = eigen_decomp$vectors

Eigenprop = Eigenvalues /sum(Eigenvalues)
round(Eigenprop,3)

Eigenvectors

loadings = Eigenvectors %>% 
  data.frame(row.names = colnames(my_data)) %>%
  rename("PC1" = X1, "PC2" = X2, "PC3" = X3, "PC4" = X4) %>%
  round(digits = 3)

loadings

pve = 100 * Eigenprop / sum(Eigenprop)
# this can be much more informative by looking at plots
par(mfrow = c(1, 2))
plot(pve, type = "b", ylab = "PVE",
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "b", ylab = "Cumulative PVE",
     xlab = "Principal Component", col = "brown3")


PCscores = data_std %*% Eigenvectors 
row.names(PCscores) = states
head(PCscores)

# PC scores are orthogonal to (i.e., uncorrelated with) each other:
cor(PCscores)