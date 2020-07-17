# Need to import Excel Files into data frames
# Need readxl packages
install.packages("readxl")
library(readxl)

# Importing all excel files into data frames
bledsoe93 <- read_excel('BLEDSOE93Daily.xls')
bledsoe94 <- read_excel('BLEDSOE94Daily.xls')                                  
bledsoe95 <- read_excel('BLEDSOE95Daily.xls')                                  
bledsoe96 <- read_excel('BLEDSOE96Daily.xls')                                  
bledsoe97 <- read_excel('BLEDSOE97Daily.xls')                                  
bledsoe98 <- read_excel('BLEDSOE98Daily.xls')                                  
bledsoe99 <- read_excel('BLEDSOE99Daily.xls')
bledsoe00 <- read_excel('BLEDSOE00Daily.xls') 
bledsoe01 <- read_excel('BLEDSOE01Daily.xls')                                   
bledsoe02 <- read_excel('BLEDSOE02Daily.xls')                                  
bledsoe03 <- read_excel('BLEDSOE03Daily.xls')                                  
bledsoe04 <- read_excel('BLEDSOE04Daily.xls')                                  
bledsoe05 <- read_excel('BLEDSOE05Daily.xls')                                  
bledsoe06 <- read_excel('BLEDSOE06Daily.xls')                                  
bledsoe07 <- read_excel('BLEDSOE07Daily.xls')                                  
bledsoe08 <- read_excel('BLEDSOE08Daily.xls')                                  
bledsoe09 <- read_excel('BLEDSOE09Daily.xls')                                  
bledsoe10 <- read_excel('BLEDSOE10Daily.xls')                                  
bledsoe11 <- read_excel('BLEDSOE11Daily.xls')                                  
bledsoe12 <- read_excel('BLEDSOE12Daily.xls')                                  
bledsoe13 <- read_excel('BLEDSOE13Daily.xls')                                  
bledsoe14 <- read_excel('BLEDSOE14Daily.xls')                                  
bledsoe15 <- read_excel('BLEDSOE15Daily.xls')                                  
bledsoe16 <- read_excel('BLEDSOE16Daily.xls')                                  
bledsoe17 <- read_excel('BLEDSOE17Daily.xls')                                  
bledsoe18 <- read_excel('BLEDSOE18Daily.xls')                                   
bledsoe19 <- read_excel('BLEDSOE19Daily.xls')

# Now clean up the data

install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
install.packages("skimr") # a package to facilitate data summaries
install.packages("Hmisc")
install.packages("cowplot")
install.packages("date")

library(tidyverse)
library(skimr)
library(Hmisc)
library(cowplot)
library(date)

# Combining all the data frames into 1
bledsoeTotal <- rbind(bledsoe93,bledsoe94,bledsoe95,bledsoe96,bledsoe97,bledsoe98,bledsoe99,bledsoe00,bledsoe01,bledsoe02,bledsoe03,bledsoe04,bledsoe06,bledsoe07,bledsoe08,bledsoe09,bledsoe10
                        ,bledsoe11,bledsoe12,bledsoe13,bledsoe14,bledsoe15,bledsoe16,bledsoe17,bledsoe18,bledsoe19)

#Removing the data variables that we wont use by selecting the ones that we will use
bledsoeTotal <- select(bledsoeTotal, 'Year', 'Julian Day', 'Max Air Temperature(C)', 'Min Air Temperature(C)', 'Avg 5cm Soil Temperature(C)', 'Avg Soil Moisture(.001%)', 'Total Solar Radiation(MJ/m^2)','Total Rain(mm)')

# Install some more packages for use
install.packages("naniar")

library(naniar)

# need to replace all -99 with NA. -99 is used to denote missing data. Years 93-03 do not have soil moisture data
bledsoeTotal <- replace_with_na(bledsoeTotal, replace = list('Avg Soil Moisture(.001%)' = -99))

# now to average all the data grouping by Julian Day
bledsoeTotalAvg <- aggregate(bledsoeTotal, by = list(bledsoeTotal$`Julian Day`), FUN = mean, na.rm = TRUE)

# aggregrate creates a new column called Group.1 which need to be removed and now that we have the average across the year, the year is useless
bledsoeTotalAvg <- select(bledsoeTotalAvg, -Group.1, -Year)

#_____________________________________________________________________________________________
# Exploritory anaylsis 

Day_vs_Temp = data.frame(x=bledsoeTotalAvg$`Julian Day`,y=bledsoeTotalAvg$`Avg 5cm Soil Temperature(C)`)

Day_vs_Moist = data.frame(x=bledsoeTotalAvg$`Julian Day`,y+bledsoeTotalAvg$`Avg Soil Moisture(.001%)`)

# Plotting Soil Moisture vs Julian Day
ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  geom_line()+
  geom_point()

#Plotting Soil Temp vs Julian Day
ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()

#Plotting 
ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Total Rain(mm)`, group=1)) +
  geom_line()+
  geom_point()
#_______________________________________________________________________________________________

# Marshall Annual Ryegrass Germination temp. Optimal is 10-30 degrees C
# Plotting Soil Moisture vs Julian Day


MarshalMoistPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  ggtitle("Marshall Annual Ryegrass")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 340)
  
  
MarshalTempPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=10, color = "blue")+
  geom_hline(yintercept=30, color = "red")+
  geom_vline(xintercept = 340)
  

cowplot::plot_grid(MarshalMoistPlot, MarshalTempPlot, align = "v", ncol = 1, rel_heights = c(0.33, 0.66))
date.mmddyy(21915+340)
 #_______________________________________________________________________________________________
 
 # Heavy Grazer Oats Germination temp. Optimal is 15-20 degrees C
OatsMoistPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  ggtitle("Heavy Grazer Oats")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 287)+
  geom_vline(xintercept = 313)

OatsTempPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=15, color = "blue")+
  geom_hline(yintercept=20, color = "red")+
  geom_vline(xintercept = 287)+
  geom_vline(xintercept = 313)

cowplot::plot_grid(OatsMoistPlot, OatsTempPlot, align = "v", ncol = 1, rel_heights = c(0.33, 0.66))
date.mmddyy(21915+287)
date.mmddyy(21915+313)
#_______________________________________________________________________________________________

# Crimson Clover Germination temp. Optimal is 11-17 degrees C
CrimsonMoistPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  ggtitle("Crimson Clover")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 300)+
  geom_vline(xintercept = 338)

CrimsonTempPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=11, color = "blue")+
  geom_hline(yintercept=17, color = "red")+
  geom_vline(xintercept = 300)+
  geom_vline(xintercept = 338)

cowplot::plot_grid(CrimsonMoistPlot, CrimsonTempPlot, align = "v", ncol = 1, rel_heights = c(0.33, 0.66))
date.mmddyy(21915+300)
date.mmddyy(21915+338)
#_______________________________________________________________________________________________

# Sorghum-Sundangrass Germination temp. Optimal is 18-24 degrees C 
SorghumMoistPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  ggtitle("Sorghum-Sundangrass")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 100)+
  geom_vline(xintercept = 140)

SorghumTempPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=18, color = "blue")+
  geom_hline(yintercept=24, color = "red")+
  geom_vline(xintercept = 100)+
  geom_vline(xintercept = 140)

cowplot::plot_grid(SorghumMoistPlot, SorghumTempPlot, align = "v", ncol = 1, rel_heights = c(0.33, 0.66))
date.mmddyy(21915+100) 
date.mmddyy(21915+140)
#_______________________________________________________________________________________________

# Pearl Millet Germination temp. Optimal is 22-34 degrees C 
PearlMoistPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg Soil Moisture(.001%)`, group=1)) +
  ggtitle("Pearl Millet")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 125)+
  geom_vline(xintercept = 275)

PearlTempPlot <- ggplot(data=bledsoeTotalAvg, aes(x=`Julian Day`, y=`Avg 5cm Soil Temperature(C)`, group=1)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=22, color = "blue")+
  geom_hline(yintercept=34, color = "red")+
  geom_vline(xintercept = 125)+
  geom_vline(xintercept = 275)



cowplot::plot_grid(PearlMoistPlot, PearlTempPlot, align = "v", ncol = 1, rel_heights = c(0.33, 0.66))
date.mmddyy(21915+125)
date.mmddyy(21915+275)

#_______________________________________________________________________________________________
#_______________________________________________________________________________________________
# Data Models

# Multivariable Linear Model, dependent variable of Soil Temperature, 
# independent variables Julian Day, Solar Radiation, Max Air Temp

SoilTempModel <- lm(bledsoeTotalAvg$`Avg 5cm Soil Temperature(C)`~  bledsoeTotalAvg$`Julian Day` + bledsoeTotalAvg$`Total Solar Radiation(MJ/m^2)` + bledsoeTotalAvg$`Max Air Temperature(C)` )

summary(SoilTempModel)

# Data Correlation 
install.packages("corrplot")
library(corrplot)
corrplot(cor(bledsoeTotalAvg))

