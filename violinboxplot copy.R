library(data.table)
library(ggplot2)
data <- data.table(fread("/Users/griffinball/stat405project/taxi_trips_2024.csv"))

data$trip_start_timestamp <- as.POSIXct(data$`Trip Start Timestamp`, format="%m/%d/%Y %I:%M:%S %p")

data[, month := as.numeric(format(trip_start_timestamp, "%m"))]
data[, season := ifelse(month %in% c(12, 1, 2), "Winter",
                        ifelse(month %in% c(3, 4, 5), "Spring",
                               ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))]

head(data$season)

data[, hour := as.integer(format(trip_start_timestamp, "%I"))]

head(data$hour)

ggplot(data, aes(x = season, y = `Trip Miles`, fill = season)) +
  geom_violin(trim = FALSE, scale = "width") +  
  geom_boxplot(width = 0.1, color = "black", alpha = 0.6) +  
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Season", y = "Trip Distance (Miles)", 
       title = "Distribution of Trip Distance by Season (Log Scale)") +
  theme_minimal() +
  scale_fill_manual(values = c("Fall" = "#E69F00", "Spring" = "#56B4E9", 
                               "Summer" = "#009E73", "Winter" = "#F0E442"))



