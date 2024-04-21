# Project 2 Proposal Code
# Sam Knisely

library(tidyverse)
library(GGally)
setwd("/Users/samknisely/Documents/Linear Models Class")
Data<-read.csv("kc_house_data.csv", sep=",", header=TRUE)
Data$pricesqft <- Data$price / Data$sqft_living
median(Data$pricesqft)
mean(Data$pricesqft)
set.seed(6021)
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F)
train<-Data[sample.data, ]
test<-Data[-sample.data, ]

q25 <- quantile(train$pricesqft, 0.25)
q75 <-quantile(train$pricesqft, 0.75)

# Create a boxplot
boxplot(train$pricesqft, 
        main = "Boxplot of Price per Living Square Foot",
        ylab = "Price per Living Square Foot",
        col = "navy",
        border = "orange")

# Add grid
grid()

train$pricesqft_cat <- ifelse(train$pricesqft < q25, 0, ifelse(train$pricesqft > q75, 1, NA))
pie(table(train$pricesqft_cat),
    main = "Pie Chart of pricesqft_cat Frequencies",
    labels = c("Low Grade", "High Grade"),
    col = c("navy", "orange"))


train2 <- train %>% select(-id, -date, -zipcode, -lat, -long, -sqft_living15, -sqft_lot15,
                           -waterfront, -view, -yr_renovated)
#install.packages("corrplot")
library(corrplot)

train_corr <- train2 %>% select(-pricesqft_cat)
# Compute the correlation matrix
correlation_matrix <- cor(train_corr)
correlation_matrix

# Create the correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8)


# Plot some of the variables with the most correlation with price
ggplot2::ggplot(train2, aes(x=bathrooms,y=price))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Amount of Bathrooms", y="Price", title="Scatterplot of the Price of a House against the Amount of Bathrooms")

ggplot2::ggplot(train2, aes(x=grade,y=price))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Grade", y="Price", title="Scatterplot of Price against Grade by King County")

ggplot2::ggplot(train2, aes(x=sqft_living,y=price))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Living Square Feet", y="Price", title="Scatterplot of the Price of a House against the Living Square Feet")

ggplot2::ggplot(train2, aes(x=bedrooms,y=price))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(x="Bedrooms", y="Price", title="Scatterplot of the Price of a House against the Amount of Bedrooms")


