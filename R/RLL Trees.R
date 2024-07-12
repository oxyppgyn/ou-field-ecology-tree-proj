#Ridges Land Lab Tree Landscape Variables
##Field Ecology Final Project
#Tanner Hammond
#data = XXX.csv; XXX.csv

#Init ----
#Working Directory
setwd("/ou-field-ecology-tree-proj/R")

#Import Packages
library(readr)
library(ggplot2)
library(psych)
library(vegan)

#Import Data
trees.total <- read.csv("/ou-field-ecology-tree-proj/R/trees.total.csv")
View(trees.total)

trees.div <- read.csv("/ou-field-ecology-tree-proj/R/trees.div.csv")
View(trees.div)

#Split Data Set
##Diversity Measures/Calculations
trees.plot <- trees.div[,1:9]

##Diversity Data
trees.div <- trees.div[,10:20]

#Data Analysis ----
#Shannon Diversity
trees.plot$shan.div <- diversity(trees.div)

#DBH
trees.total$DBH_cm <- trees.total$Circum_cm/pi

#Hypothesis Tests & Plots ----
#General species diversity
  ##Histogram Test with All Spp.
  ggplot(trees.total, aes(x = DBH_cm, color = ComName)) + geom_histogram(binwidth = 5)
  
  ##Official Histogram & Spp-DBH-Shade_Tolerance Tests
  ggplot(trees.total, aes(x = DBH_cm, fill = ComName)) + geom_histogram(binwidth = 5, position = "identity", color = "black", alpha = 0.5, data = subset(trees.total, ComName %in% c("Sugar maple", "White oak"))) + labs(x = "DBH (centimeters)", y = "Count") + theme_classic() + scale_fill_manual(values = c("Sugar maple" = "red", "White oak" = "blue"))
  
  ##Species with Size
  DBH_Spp.lm <- lm(DBH_cm~ComName, data = trees.total)
  summary(DBH_Spp.lm)
  ###Results: P = <2.2e-16***, adjR2 = 0.6896, F = 49.88, df = 11,231

#Shade Tolerance with Size
  ##Linear Model
  DBH_ShTol.lm <- lm(DBH_cm~Shade_Tolerance, data = trees.total)
  summary(DBH_ShTol.lm)
  ###Results: p = <2.2e-16***, Adj R2 = 0.543, F = 96.85, df = 3,239

  ##Summary Stats
  DBH_ShTol.sumstats <- describeBy(trees.total$DBH_cm, group = trees.total$Shade_Tolerance, mat = TRUE)
  DBH_ShTol.sumstats

  ##Plot (Summary Stats)
  DBH_ShTol.sumstats$group1 <- factor(DBH_ShTol.sumstats$group1, levels = c("Intolerant","Intermediate", "Tolerant", "Very Tolerant"), ordered = TRUE)
  ggplot(DBH_ShTol.sumstats, aes(x = group1, y= mean)) + theme_classic() + geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + labs(x = "Shade Tolerance", y = "DBH (centimeters)")

#Does diversity correlate with density?
  ##Linear Models
  shan.div_Dens.lm <-lm(shan.div~DensBA, data = trees.plot)
  summary(shan.div_Dens.lm)
  ###Results: p = 0.2228, adjR2 = 0.07661, F = 1.747, df = 1,8, slope = 0.1452
  
  ##Plot
  ggplot(trees.plot, aes(x = DensBA, y = shan.div)) + theme_classic() + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(x = "Absolute Basal Area Density", y = "Shannon Diversity")
  
#Does diversity & density relate to slope?
  ##Linear Models
  shan.div_Slope.lm <- lm(shan.div~Slope, data = trees.plot)
  summary(shan.div_Slope.lm)
  ###Results: p = 0.8697, adjR2 = -0.121, F = 0.02868, df = 1,8, slope = -0.003472
  Dens_Slope.lm <- lm(DensBA~Slope, data = trees.plot)
  summary(Dens_Slope.lm)
  ###Results: p = 0.6067, adjR2 = -0.08602, F = 0.2871, df = 1,8, Slope = -0.03153
  
  ##Plots
  ggplot(trees.plot, aes(x = Slope, y = shan.div)) + theme_classic() + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(y = "Shannon Diversity")
  ggplot(trees.plot, aes(x = Slope, y = DensBA)) + theme_classic() + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(y = "Absolute Basal Area Density")

#Does diversity & density relate to aspect?
  ##Linear Models
  shan.div_Facing.lm <- lm(shan.div~Facing, data = trees.plot)
  summary(shan.div_Facing.lm)
  ###Results: p = 0.05122, adjR2 = 0.4499, F = 4.681, df = 2,7
  Dens_Facing.lm <- lm(DensBA~Facing, data = trees.plot)
  summary(Dens_Facing.lm)
  ###Results: P = 0.3950, adjR2 = 0.01396, F = 1.064, df = 2,7

  ##Summary Stats
  shan.div_Facing.sumstats <- describeBy(trees.plot$shan.div, group = trees.plot$Facing, mat = TRUE)
  shan.div_Facing.sumstats
  Dens_Facing.sumstats <- describeBy(trees.plot$DensBA, group = trees.plot$Facing, mat = TRUE)
  Dens_Facing.sumstats
  ##Plots
  ggplot(shan.div_Facing.sumstats, aes(x = group1, y = mean)) + theme_classic() + 
    geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + 
    labs(x = "Facing Direction", y = "Mean Shannon Diversity")
  ggplot(Dens_Facing.sumstats, aes(x = group1, y = mean)) + theme_classic() + 
    geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + 
    labs(x = "Facing Direction", y = "Mean Absolute Basal Area Density")

#Does diversity & density relate to elevation?
  ##Linear Models
  shan.div_Elev.lm <- lm(shan.div~Elevation, data = trees.plot)
  summary(shan.div_Elev.lm)
  ###Results: P = 0.581, adjR2 = -0.08043, F = 0.33, df = 1,8, Slope = 0.005374
  Dens_Elev.lm <- lm(DensBA~Elevation, data = trees.plot)
  summary(Dens_Elev.lm)
  ###Results: P = 0.2794, adjR2 = 0.03707, F = 1.346, df = 1,8, Slope = -0.02988

  ##Plots
  ggplot(trees.plot, aes(x = Elevation, y = DensBA)) + theme_classic() + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(y = "Absolute Basal Area Density")
  ggplot(trees.plot, aes(x = Elevation, y = shan.div)) + theme_classic() + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(y = "Shannon Diversity")


  
