################################### Check for chain covergence ###########################################
# Resources:
# http://mc-stan.org/bayesplot/articles/plotting-mcmc-draws.html 
# http://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html 

# Can we lapply this so we can check all mods at once?
# Mod 1
posterior <- as.array(list_mod[1]) 
dim(posterior)
dimnames(posterior)
log_posterior(list_mod[[1]])
rhats <- rhat(list_mod[[1]])
color_scheme_set("brightblue")
mcmc_rhat(rhats)                 #rhat values close to 1, so we assume chains have converged.  

