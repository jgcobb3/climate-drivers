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

######################################## Dataframe #######################################################

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

my_comparisons <- list(c("1", "2"), c("1", "3"), c("1", "4"), c("2","3"), c("2","4"), c("3","4"))


# reminder about PDSI values: 
# 0 to -.5 = normal; 
# -0.5 to -1.0 = incipient drought; 
# -1.0 to -2.0 = mild drought; 
# -2.0 to -3.0 = moderate drought; 
# -3.0 to -4.0 = severe drought; 
# greater than - 4.0 = extreme drought.

######################################### Plots ########################################################
# Means
# dashed red line is at the point where drought begins.

ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd",  
            color = "black",
            palette = "npg",
            title = "Level of concern and mean PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            ylim = c(-10, 12),
            legend = "bottom",
            legend.title = "Concern", 
            xlab = "level of concern",
            ylab = "PDSI",
            orientation = "vertical", 
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4") +
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = -10, label.x = 1.5) +
  geom_hline(yintercept=-0.5, linetype="dashed", color = "red")

# SDs
ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Standard deviation 1-year", "Standard deviation 3-years", "Standard deviation 5-years", "Standard deviation 10-years", "Standard deviation 15-years"),
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd", 
            color = "black",
            palette = "npg",
            title = "Level of concern and standard deviation of PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            ylim = c(-2, 8),
            legend = "bottom",
            legend.title = "Concern", 
            xlab = "level of concern",
            ylab = "PDSI",
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4")+
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = -1, label.x = 1.5)



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