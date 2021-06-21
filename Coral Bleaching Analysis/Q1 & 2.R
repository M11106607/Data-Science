library(ggplot2)
library(tidyverse)

# Reading the data file
dat <- read.csv("assignment-02-data-formated.csv")

# Creating a data frame
bleach <- data.frame(dat)
head(bleach)

# Conversion of bleaching % into numeric for better processing and visualisation
bleach$bleaching <- as.numeric(sub("%","",bleach$value))

# Ordering the sites based on latitude, rev is used to order the sites in desending order
bleach$location <- factor(bleach$location,levels = rev(unique(bleach$location[order(bleach$latitude)])))

summary(bleach)

#ggplot(data = bleach) +
 # geom_line(mapping = aes(x = year, y = value, color = coralType, size = location))

#qplot(year,bleaching, data = subset(bleach, location == "site01"), geom = 'smooth', color = coralType)

#ggplot(aes(x=year, y=bleaching, color = coralType), data = subset(bleach, location == "site01"), geom = 'smooth')+
 # geom_point() + facet_grid(~coralType) + ggtitle("Site 01") + xlab("Years") + ylab("Bleaching Percentage")

# Plotting Sea Pens for different Sites
ggplot(data = subset(bleach, coralType == "sea pens"), aes(x=year, y=bleaching,color = location, size = bleaching)) +
  geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
  facet_grid(~location) + ggtitle("Sea Pens at different sites") + geom_smooth(method = lm , span = 0.5)

# Plotting Hard Corals for different Sites
ggplot(data = subset(bleach, coralType == "hard corals"), aes(x=year, y=bleaching,color = location, size = bleaching)) +
  geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
  facet_grid(~location) + ggtitle("Hard Corals at different sites") + geom_smooth(method = lm , span = 0.5)

# Plotting Blue Corals for different Sites
ggplot(data = subset(bleach, coralType == "blue corals"), aes(x=year, y=bleaching,color = location, size = bleaching)) +
  geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
  facet_grid(~location) + ggtitle("Blue Corals at different sites") + geom_smooth(method = lm , span = 0.5)

# Plotting Soft Corals for different Sites
ggplot(data = subset(bleach, coralType == "soft corals"), aes(x=year, y=bleaching,color = location, size = bleaching)) +
  geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
  facet_grid(~location) + ggtitle("Soft Corals at different sites") + geom_smooth(method = lm , span = 0.5)

# Plotting Sea Fans for different Sites
ggplot(data = subset(bleach, coralType == "sea fans"), aes(x=year, y=bleaching,color = location, size = bleaching)) +
  geom_jitter() + xlab("Years") + ylab("Bleaching Percentage") +
  facet_grid(~location) + ggtitle("Sea Fans at different sites") + geom_smooth(method = lm , span = 0.5)


#unique(bleach$latitude)



