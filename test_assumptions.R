############################## Testing the Parellel Regression Assumption of POLR ###################
# References:

## Plotting:
##https://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html 

##### Testing the Parellel Regression Assumption of POLR ############

# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s_mean2016 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2016, fun=sf)))
(s_mean2014 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2014, fun=sf)))
(s_mean2012 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2012, fun=sf)))
(s_mean2007 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2007, fun=sf)))
(s_mean2002 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_MEAN_2002, fun=sf)))
(s_std2016 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2016, fun=sf)))
(s_std2014 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2014, fun=sf)))
(s_std2012 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2012, fun=sf)))
(s_std2007 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2007, fun=sf)))
(s_std2002 <- with(data_subset, summary(as.numeric(Concern_DryDrought) ~ PDSI_STD_2002, fun=sf)))

# The above functions display the linear predicted variables we would get if we regressed Concern (the predicted variable)
# on our predictor variables without the parallel slopes assumption. To evaluate the parallel slops assumption, we now will
# run a series of binary logistic regressesions with varying cutpoints on the dependent variable and checking the equality
# of coefficients across cutpoints.

#PDSI_MEAN_2016 - parallel slopes assumption holds.
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2016, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2016, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.648
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2016, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.648

plot(s_mean2016, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2016[,3:4]))

#PDSI_MEAN_2014 - parallel slopes assumption holds.
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2014, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2014, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.658
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2014, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.655

plot(s_mean2014, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2014[,3:4]))

#PDSI_MEAN_2012 - does the parallel slopes assumption hold? The plot looks similar to the 2014 and 2016 plots, so I would say yes...ish?
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2012, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2012, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.623
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2012, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.700

plot(s_mean2012, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2012[,3:4]))

#PDSI_MEAN_2007 - does the parallel slopes assumption hold? Same as above
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2007, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2007, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.618
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2007, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.693

plot(s_mean2007, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2007[,3:4]))

#PDSI_MEAN_2002 - does the parallel slopes assumption hold? Same as above
glm(I(as.numeric(Concern_DryDrought) >= 2) ~ PDSI_MEAN_2002, family="binomial", data = data_subset)
glm(I(as.numeric(Concern_DryDrought) >= 3) ~ PDSI_MEAN_2002, family="binomial", data = data_subset) # dif between intercept 2/3 = 1.618
glm(I(as.numeric(Concern_DryDrought) >= 4) ~ PDSI_MEAN_2002, family="binomial", data = data_subset)# dif between intercept 3/4 = 1.70

plot(s_mean2002, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_mean2002[,3:4]))

# All STD model plots are reasonable - I think the parrallet slopes assumptions holds on all of our models.
plot(s_std2016, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2016[,3:4]))
plot(s_std2014, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2014[,3:4]))
plot(s_std2012, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2012[,3:4]))
plot(s_std2007, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2007[,3:4]))
plot(s_std2002, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s_std2002[,3:4]))



################################# End of script ######################################
