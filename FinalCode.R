
library(rgdal)
library(tmap)

# Set the working directory to the location of the shapefile
setwd("~/Downloads/cb_2018_us_state_500k")

# Read the shapefile into R
states <- readOGR(".", "cb_2018_us_state_500k")

# Load the broadband availability data into R
broadband <- read.csv("~/Documents/Plan-Final/broadband_access.csv")

# Aggregate the broadband availability data by state
broadband_by_state <- aggregate(broadband$broad_avail, by = list(broadband$state_abr), FUN = mean)
names(broadband_by_state) <- c("STUSPS", "broad_avail")

# Merge the broadband data with the shapefile
merged_data <- merge(states, broadband_by_state, by = "STUSPS")

# Set tmap to view mode
tmap_mode("view")

# Create a choropleth map using tmap
tm_shape(merged_data) +
  tm_polygons("broad_avail", title = "Broadband Availability by State (percentage)", palette = "Greens") +
  tm_layout(title = "Broadband Availability by State") +
  tm_view()



library(ggplot2)

broadband <- read.csv("~/Documents/Plan-Final/broadband_access.csv")
#heatmap with percentage without health insurance on x-axis and percentage with broadband availability on y-axis.
ggplot(broadband, aes(x = health_ins, y = broad_avail)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient(low = "#FFFFCC", high = "#FF6600") +
  labs(title = "Relationship between Health Insurance and Broadband Availability", 
       x = "Percent Without Health Insurance", y = "Broadband Availability (%)")









# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(car)

# Read in the data
broadband <- read.csv("~/Documents/Plan-Final/broadband_access.csv")

# Check the structure of the data
str(broadband)

# Run regression analysis
broadband_reg <- lm(broad_avail ~ population + unemp + health_ins + poverty + SNAP + no_comp + no_internet + home_broad, data = broadband)

# Check the summary of the regression analysis
summary(broadband_reg)

# Plot the relationship between broadband availability and health insurance
ggplot(broadband, aes(x = health_ins, y = broad_avail)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Percent without health insurance", y = "Broadband Availability") +
  ggtitle("Relationship Between Broadband Availability and Health Insurance")

# Plot the residual plot
plot(broadband_reg, which = 1)

# Check for multicollinearity
vif(broadband_reg)



library(ggplot2)

# Load the data
data <- read.csv("~/Documents/Plan-Final/broadband_access.csv")

# Aggregate the broadband availability data by state
broadband_by_state <- aggregate(data$broad_avail, by = list(data$state_abr), FUN = mean)
names(broadband_by_state) <- c("state_abr", "broad_avail")

# Aggregate the poverty data by state
poverty_by_state <- aggregate(data$poverty, by = list(data$state_abr), FUN = mean)
names(poverty_by_state) <- c("state_abr", "poverty_rate")

# Merge the broadband and poverty data by state
merged_data <- merge(broadband_by_state, poverty_by_state, by = "state_abr")
merged_data <- na.omit(merged_data)

# Create a stacked bar chart
ggplot(merged_data, aes(x = state_abr)) +
  geom_bar(aes(y = broad_avail, fill = "Broadband Availability"), stat = "identity", position = "dodge", width = 0.4) +
  geom_bar(aes(y = poverty_rate, fill = "Poverty Rate"), stat = "identity", position = "dodge", width = 0.4) +
  scale_fill_manual(values = c("blue", "red"), name = "") + # Set the name of the legend to blank
  labs(x = "State", y = "Percentage", title = "Poverty Rate and Broadband Availability by State") +
  theme_bw()







