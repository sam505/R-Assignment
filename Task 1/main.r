# installing required packages
install.packages("ggplot2")
install.packages("GGally")
install.packages("ISLR")
install.packages("openssl")
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggfortify")
install.packages("factoextra")


# importing required packages
library(ggplot2)
library(GGally) 
library(ISLR)
library(tidyverse)
library(ggfortify)
library(factoextra)
library(FactoMineR)

# read in the csv file
data <- read_csv("EWCS_2016.csv")

# visualize data
View(data)

# remove anomalies
data[,][data[, ,] == -999] = NA
kk=complete.cases(data)
data=data[kk,]

# visualize the number of males and females
data %>% count(Q2a, sort = TRUE)

scaled.data = scale(data) # scale for PCA

# visualize correlation plot
ggcorr(scaled.data, label = TRUE, label_alpha = TRUE)

# visualize the distribution of age plot
ggplot(data, aes(x=Q2b)) + geom_histogram(stat="count", 
                                          fill="green")


# visualize values distributions in columns
options(repr.plot.width=6, repr.plot.height=6)
ggplot(gather(data), aes(value)) + 
  geom_histogram(stat="count", 
                 fill="green")+
  facet_wrap(~key, scales = 'free_x')

#### Principal Components Analysis ####
pca = prcomp(scaled.data, scale=T)
summary(pca) 
fviz_eig(pca) # screen plot with percentage variance

# PCA matrix
pca.mat = function(loading, comp.sdev){
  loading*comp.sdev
}

# define the loading and standard deviation
loading = pca$rotation
comp.sdev = pca$sdev

# Principal components variables
pca.var = t(apply(loading, 1, pca.mat, comp.sdev)) # apply the function with the loading and std dev


pca.var[,1:3] # display the PCA matrix, PC1, PC2 and PC3
# All factor loading values > 0.75 are strong
# Factor loading values ranging between 0.50-0.75 are moderate
# while factor loading values ranging from 0.30 to 0.49 are weak

# In PCA 1 Q87a ,b , c, d and e have the highest factor loading values
# On the other hand, for the second PCA 2, the Q90a, b ,c and f have the highest loadings



# Principal Component Analysis Bi-plot
autoplot(pca, 
         colour ='Q90f', 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 5)

autoplot(pca, 
         colour ='Q87d', 
         loadings = TRUE,
         loadings.label = TRUE, 
         loadings.label.size = 5)

# The vectors on Q87 explain the observations dispersion in response to Q90f 
# responses. Positive responses 1 and 2 are densely packed in comparison to the
# negative responses 5 and 6 which have a wider dispersion.

# If a individual was to respond positively to Q87a, b, c, d, e and f, they are
# more likely to respond positively to Q90a, b, c, f. This is because the vectors 
# for Q87 are in the same direction as the dispersion of the responses