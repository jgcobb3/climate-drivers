############### Climate perception project ########## 
## Plot development for manuscript
##
##Data used in this project comes from three sources: (1) A survey of NRCS and FSA employees conducted by the USDA Climate Hubs in 2016/2017, (2) Crop idemnity payments made by FSA to farmers for weather-related 
##crop loss between 2013-2016, and (3) mean and standard deviations of drought data over 1, 3, 5, 10, and 15 year periods.
##
##
## DATE CREATED: 11/19/2018
## DATE MODIFIED: xxx
## AUTHORS: Rachel Schattman, Benoit Parmentier  
## Version: 1
## PROJECT: Climate Percecption
###################################### Libraries used #################################################

library("ggpubr")

######################################## Plots #######################################################

# Create data frame with column headings as you would want them to appear in figures
data_years <- data_subset
colnames(data_years)[colnames(data_years)=="PDSI_MEAN_2016"] <- "Mean 1-year"
colnames(data_years)[colnames(data_years)=="PDSI_MEAN_2014"] <- "Mean 3-years"
colnames(data_years)[colnames(data_years)=="PDSI_MEAN_2012"] <- "Mean 5-years"
colnames(data_years)[colnames(data_years)=="PDSI_MEAN_2007"] <- "Mean 10-years"
colnames(data_years)[colnames(data_years)=="PDSI_MEAN_2002"] <- "Mean 15-years"
colnames(data_years)[colnames(data_years)=="PDSI_STD_2016"] <- "Standard deviation 1-year"
colnames(data_years)[colnames(data_years)=="PDSI_STD_2014"] <- "Standard deviation 3-years"
colnames(data_years)[colnames(data_years)=="PDSI_STD_2012"] <- "Standard deviation 5-years"
colnames(data_years)[colnames(data_years)=="PDSI_STD_2007"] <- "Standard deviation 10-years"
colnames(data_years)[colnames(data_years)=="PDSI_STD_2002"] <- "Standard deviation 15-years"

# Means
ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd", 
            color = "black",
            palette = "npg",
            title = "Level of concern and mean PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            legend = "bottom",
            legend.title = "Concern", 
            xlab = "level of concern",
            ylab = "PDSI",
            orientation = "vertical", 
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4")
# SDs
ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Standard deviation 1-year", "Standard deviation 3-years", "Standard deviation 5-years", "Standard deviation 10-years", "Standard deviation 15-years"),
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd", 
            color = "black",
            palette = "npg",
            title = "Level of concern and standard deviation of PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            legend = "bottom",
            legend.title = "Concern", 
            xlab = "level of concern",
            ylab = "PDSI",
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4")

kruskal.test(data_years$Concern_DryDrought ~ data_years$`Mean 1-year`)
kruskal.test(data_years$Concern_DryDrought ~ data_years$`Mean 3-years`)
kruskal.test(data_years$Concern_DryDrought ~ data_years$`Mean 5-years`)
kruskal.test(data_years$Concern_DryDrought ~ data_years$`Mean 10-years`)
kruskal.test(data_years$Concern_DryDrought ~ data_years$`Mean 15-years`)

pairwise.wilcox.test(data_years$`Mean 1-year`,data_years$Concern_DryDrought,
                     p.adjust.method = "BH")

## These plots are switched... they are grouped by level of concern (dependent on Y axis) - this has been unsuccessful so far
ggerrorplot(data_years, x = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
            y = data_years$Concern_DryDrought,
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd",
            color = "black",
            palette = "npg",
            title = "Level of concern and mean PDSI",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            legend = "bottom",
            legend.title = "Concern",
            xlab = "PDSI",
            ylab = "level of concern",
            caption = "Level of concern about drought: Not concerned = 1, Slightly concerned = 2, Concerned = 3, Very concerned = 4")

################# End Script ####################################################################################