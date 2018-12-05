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

# add colume with PDSI categories
data_years_cat <- data_years
#data_years["Mean 1-year"] <- NA
data_years_cat$"Mean 1-year" <- ifelse(data_years_cat$"Mean 1-year" < 0 & data_years_cat$"Mean 1-year" > -0.5,"normal",
                       ifelse(data_years_cat$"Mean 1-year" <= -0.5 & data_years_cat$"Mean 1-year" > -1,"incipient drought",
                          ifelse(data_years_cat$"Mean 1-year" <= -1 & data_years_cat$"Mean 1-year" > -2,"mild drought",
                            ifelse(data_years_cat$"Mean 1-year" <= -2 & data_years_cat$"Mean 1-year" > -3,"moderate drought",
                              ifelse(data_years_cat$"Mean 1-year" <= -3 & data_years_cat$"Mean 1-year" > -4,"severe drought", 'extreme drought')
                            )
                          )
                       )
                    )
data_years_cat$"Mean 3-years" <- ifelse(data_years_cat$"Mean 3-years" < 0 & data_years_cat$"Mean 3-years" > -0.5,"normal",
                                       ifelse(data_years_cat$"Mean 3-years" <= -0.5 & data_years_cat$"Mean 3-years" > -1,"incipient drought",
                                              ifelse(data_years_cat$"Mean 3-years" <= -1 & data_years_cat$"Mean 3-years" > -2,"mild drought",
                                                     ifelse(data_years_cat$"Mean 3-years" <= -2 & data_years_cat$"Mean 3-years" > -3,"moderate drought",
                                                            ifelse(data_years_cat$"Mean 3-years" <= -3 & data_years_cat$"Mean 3-years" > -4,"severe drought", 'extreme drought')
                                                     )
                                              )
                                       )
)
data_years_cat$"Mean 5-years" <- ifelse(data_years_cat$"Mean 5-years" < 0 & data_years_cat$"Mean 5-years" > -0.5,"normal",
                                        ifelse(data_years_cat$"Mean 5-years" <= -0.5 & data_years_cat$"Mean 5-years" > -1,"incipient drought",
                                               ifelse(data_years_cat$"Mean 3-years" <= -1 & data_years_cat$"Mean 3-years" > -2,"mild drought",
                                                      ifelse(data_years_cat$"Mean 5-years" <= -2 & data_years_cat$"Mean 5-years" > -3,"moderate drought",
                                                             ifelse(data_years_cat$"Mean 5-years" <= -3 & data_years_cat$"Mean 5-years" > -4,"severe drought", 'extreme drought')
                                                      )
                                               )
                                        )
)
data_years_cat$"Mean 10-years" <- ifelse(data_years_cat$"Mean 10-years" < 0 & data_years_cat$"Mean 10-years" > -0.5,"normal",
                                        ifelse(data_years_cat$"Mean 10-years" <= -0.5 & data_years_cat$"Mean 10-years" > -1,"incipient drought",
                                               ifelse(data_years_cat$"Mean 10-years" <= -1 & data_years_cat$"Mean 10-years" > -2,"mild drought",
                                                      ifelse(data_years_cat$"Mean 10-years" <= -2 & data_years_cat$"Mean 10-years" > -3,"moderate drought",
                                                             ifelse(data_years_cat$"Mean 10-years" <= -3 & data_years_cat$"Mean 10-years" > -4,"severe drought", 'extreme drought')
                                                      )
                                               )
                                        )
)
data_years_cat$"Mean 15-years" <- ifelse(data_years_cat$"Mean 15-years" < 0 & data_years_cat$"Mean 15-years" > -0.5,"normal",
                                        ifelse(data_years_cat$"Mean 15-years" <= -0.5 & data_years_cat$"Mean 15-years" > -1,"incipient drought",
                                               ifelse(data_years_cat$"Mean 15-years" <= -1 & data_years_cat$"Mean 15-years" > -2,"mild drought",
                                                      ifelse(data_years_cat$"Mean 15-years" <= -2 & data_years_cat$"Mean 15-years" > -3,"moderate drought",
                                                             ifelse(data_years_cat$"Mean 15-years" <= -3 & data_years_cat$"Mean 15-years" > -4,"severe drought", 'extreme drought')
                                                      )
                                               )
                                        )
)
data_years_cat$"Mean 3-years" <- ifelse(data_years_cat$"Mean 3-years" < 0 & data_years_cat$"Mean 3-years" > -0.5,"normal",
                                        ifelse(data_years_cat$"Mean 3-years" <= -0.5 & data_years_cat$"Mean 3-years" > -1,"incipient drought",
                                               ifelse(data_years_cat$"Mean 3-years" <= -1 & data_years_cat$"Mean 3-years" > -2,"mild drought",
                                                      ifelse(data_years_cat$"Mean 3-years" <= -2 & data_years_cat$"Mean 3-years" > -3,"moderate drought",
                                                             ifelse(data_years_cat$"Mean 3-years" <= -3 & data_years_cat$"Mean 3-years" > -4,"severe drought", 'extreme drought')
                                                      )
                                               )
                                        )
)

# correct factor order

data_years_cat$`Mean 1-year` <- factor(data_years_cat$`Mean 1-year`, levels = c("normal", "incipient drought", "mild drought",
                                                    "moderate drought", "severe drought", "extreme drought"))
data_years_cat$`Mean 3-years` <- factor(data_years_cat$`Mean 3-years`, levels = c("normal", "incipient drought", "mild drought",
                                                                                "moderate drought", "severe drought", "extreme drought"))
data_years_cat$`Mean 5-years` <- factor(data_years_cat$`Mean 5-years`, levels = c("normal", "incipient drought", "mild drought",
                                                                                  "moderate drought", "severe drought", "extreme drought"))
data_years_cat$`Mean 10-years` <- factor(data_years_cat$`Mean 10-years`, levels = c("normal", "incipient drought", "mild drought",
                                                                                  "moderate drought", "severe drought", "extreme drought"))
data_years_cat$`Mean 15-years` <- factor(data_years_cat$`Mean 15-years`, levels = c("normal", "incipient drought", "mild drought",
                                                                                  "moderate drought", "severe drought", "extreme drought"))

# set comparisons for error plots 
my_comparisons <- list(c("1", "2"), c("1", "3"), c("1", "4"), c("2","3"), c("2","4"), c("3","4"))


# reminder about PDSI values: 
# 0 to -.5 = normal; 
# -0.5 to -1.0 = incipient drought; 
# -1.0 to -2.0 = mild drought; 
# -2.0 to -3.0 = moderate drought; 
# -3.0 to -4.0 = severe drought; 
# greater than - 4.0 = extreme drought.

######################################### Plots ########################################################
# Means - concern on X axis
# dashed red line is at the point where drought begins.
# all years combined into one plot - this makes it a little easier to see that people who experience high levels
# of drought have higher levels of concern.
ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
            combine = FALSE, merge = TRUE,
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

#years broken out into different plots
gplot1 <- ggerrorplot(data_years, x = "Concern_DryDrought", 
            y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
            combine = TRUE, merge = FALSE,
            desc_stat = "mean_sd",  
            color = "black",
            palette = "npg",
            title = "Level of concern and mean PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            ylim = c(-6, 12),
            common.legend = TRUE,
            legend = "top", top = 12,
            legend.title = "drought severity", 
            xlab = "level of concern",
            ylab = "PDSI",
            orientation = "vertical",
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4")+
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = -5, label.x = 1.5) +
  geom_hline(yintercept= -0.5, linetype="dashed", color = "red", show.legend = TRUE)+
  geom_hline(yintercept= -1, linetype="dotdash", color = "blue", show.legend = TRUE)+
  geom_hline(yintercept= -2, linetype="dotted", color = "black", show.legend = TRUE)+
  geom_hline(yintercept= -3, linetype="longdash", color = "red", show.legend = TRUE)+
  geom_hline(yintercept= -4, linetype="twodash", color = "blue", show.legend = TRUE)
  
  
gplot1 <- ggerrorplot(data_years, x = "Concern_DryDrought", 
                      y = c("Mean 1-year", "Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years"),
                      combine = TRUE, merge = FALSE,
                      desc_stat = "mean_sd",  
                      color = "black",
                      palette = "npg",
                      title = "Level of concern and mean PDSI over 5-time scales",
                      add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
                      ylim = c(-6, 12),
                      common.legend = TRUE,
                      legend = "top", top = 12,
                      legend.title = "drought severity", 
                      xlab = "level of concern",
                      ylab = "PDSI",
                      orientation = "vertical",
                      caption = "Level of concern about drought: Not concerned = 1, 
                      Slightly concerned = 2, Concerned = 3, Very concerned = 4")+
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = -5, label.x = 1.5) +
  geom_hline(yintercept= -0.5, linetype="dashed", color = "red", show.legend = TRUE)+
  geom_hline(yintercept= -1, linetype="dotdash", color = "blue", show.legend = TRUE)+
  geom_hline(yintercept= -2, linetype="dotted", color = "black", show.legend = TRUE)+
  geom_hline(yintercept= -3, linetype="longdash", color = "red", show.legend = TRUE)+
  geom_hline(yintercept= -4, linetype="twodash", color = "blue", show.legend = TRUE)

#annotate_figure(gplot1, bottom = text_grob("Level of concern about drought: Not concerned = 1, 
#            Slightly concerned = 2, Concerned = 3, Very concerned = 4", color = "black",
#           hjust = 1, x = 1, face = "italic", size = 10))

#leg <- get_legend(gplot1)

#as_ggplot(leg)

# SDs - concern on x axis
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

# Means - concern on Y axis
# this works, but it's ugly
ggplot(data_years_cat, aes(x = data_years_cat$`Mean 1-year`, y = data_years_cat$Concern_DryDrought)) + 
  geom_violin() #+
  #geom_errorbar(data=data_years_cat,
                #aes(data_years_cat$Concern_DryDrought, mean, ymin=mean-sd, ymax = mean-sd),
                #colour = "red",
                #width = 0.4)
ggplot(data_years_cat, aes(x = data_years_cat$`Mean 3-years`, y = data_years_cat$Concern_DryDrought)) + 
  geom_violin()
ggplot(data_years_cat, aes(x = data_years_cat$`Mean 5-years`, y = data_years_cat$Concern_DryDrought)) + 
  geom_violin()
ggplot(data_years_cat, aes(x = data_years_cat$`Mean 10-years`, y = data_years_cat$Concern_DryDrought)) + 
  geom_violin()
ggplot(data_years_cat, aes(x = data_years_cat$`Mean 15-years`, y = data_years_cat$Concern_DryDrought)) + 
  geom_violin()

## I'd like to reproduce the violin plots from the above section, but with categorical drought categies. Have not succeeded using code below.
ggerrorplot(data_years_cat, x = data_years_cat$`Mean 1-year`, 
            y = data_years_cat$Concern_DryDrought),
            combine = FALSE, merge = TRUE,
            desc_stat = "mean_sd",  
            color = "black",
            palette = "npg",
            title = "Level of concern and mean PDSI over 5-time scales",
            add = "violin", add.params = list(color = "darkgray", fill="lightgray"),
            ylim = c('normal', 'extreme drought'),
            legend = "bottom",
            legend.title = "Concern", 
            xlab = "PDSI",
            ylab = "level of concern",
            orientation = "vertical", 
            caption = "Level of concern about drought: Not concerned = 1, 
            Slightly concerned = 2, Concerned = 3, Very concerned = 4") #+
  #stat_compare_means(comparisons = my_comparisons) +
  #stat_compare_means(label.= -10, label.x = 1.5) +
  y #geom_hline(yintercept=-0.5, linetype="dashed", color = "red")

"Mean 3-years", "Mean 5-years", "Mean 10-years", "Mean 15-years")

#

################# End Script ####################################################################################