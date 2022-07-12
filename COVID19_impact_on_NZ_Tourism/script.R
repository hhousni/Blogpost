library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(grid)
library(tidyverse)


########### graph1 ######
intArrival <- read_excel("COVID19_impact_on_NZ_Tourism/RawData/InternationaArrival2010-Apr2022.xlsx")


# data prep 
intArrivalClean0 <- as.data.frame(intArrival[2:149,])
names(intArrivalClean0) <- c("date","seasonallyAdjusted","trend")
intArrivalClean0$date <- as.Date(paste(intArrivalClean0$date, "01", sep = "-"), "%YM%m-%d")
intArrivalClean0$date <- as.POSIXct(intArrivalClean0$date)

intArrivalClean1 <- intArrivalClean0 %>%
  mutate(SeasonallyAdjusted = as.numeric(seasonallyAdjusted), 
         Trend = as.numeric(trend),
         Year = year(date),
         Month = month(date)) %>%
  filter(Year %in% c(2018:2022))



# visualisation 

intArrivalClean1$Year <- factor(intArrivalClean1$Year)

ggplot() +
  geom_line(data = intArrivalClean1, aes (x = Month, y = SeasonallyAdjusted, color = Year)) + 
  scale_y_continuous(name = "Visitor Arrival",limits = c(0,400000),labels = comma) +
  scale_x_continuous( breaks = seq_along(month.name),
                      labels = month.name) 


 # annotate("segment", x = 2, y = 275000, yend = 274000, xend = 2,15
  #         color = "blue", size = 2, arrow = arrow()) +
  #annotate("text", x = 2,275000,
   #        label = "This Job is mine",
    #       col = "black",
     #      size = 5)

########### graphe 2 employment market######

employment <- read_excel("COVID19_impact_on_NZ_Tourism/RawData/employment.xlsx")

employment0 <- as.data.frame(employment[2:13,1:4])
names(employment0) <- c("Year","DirectlyEmployed","IndirectlyEmployed","TotalEmployed")


employment1 <- employment0 %>%
  mutate(DirectlyEmployed = as.numeric(DirectlyEmployed),
         IndirectlyEmployed = as.numeric(IndirectlyEmployed),
         TotalEmployed = as.numeric(TotalEmployed),
         Year = as.numeric(Year)) %>%
  mutate(DirectlyEmployedGrowthRate = 
           round((DirectlyEmployed- lag(DirectlyEmployed))/lag(DirectlyEmployed)*100, digits = 2),
         IndirectlyEmployedGrowthRate = 
           round((IndirectlyEmployed- lag(IndirectlyEmployed))/lag(IndirectlyEmployed)*100, digits = 2),
         TotalEmployedGrowthRate = 
           round((TotalEmployed- lag(TotalEmployed))/lag(TotalEmployed)*100, digits = 2)) %>%
  filter(Year %in% c(2018:2021)) %>%
  select(c("Year","DirectlyEmployedGrowthRate","IndirectlyEmployedGrowthRate","TotalEmployedGrowthRate"))

names(employment1) <- c("Year","Directly Employed", "Indirectly Employed", "Total Employed")


# visualization

emplViz <- employment1 %>%
  gather("Directly Employed", "Indirectly Employed", "Total Employed", 
         key = EmployementType, value = AnnualGrowthRate) %>%
  filter(EmployementType %in% c("Directly Employed","Indirectly Employed"))

ggplot(data = emplViz, aes (x = Year, y = AnnualGrowthRate, fill = EmployementType )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label=AnnualGrowthRate),color="black",hjust = 1, position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  coord_flip()


##### 3 Domestic expenditure #####
  



