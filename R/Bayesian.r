# Bayesian functions # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
# Bayesian onesam_tfun # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
layout.Bayes_onesam_t<-function(e){
  e$alternative <- tclVar("two.sided"); 
  #e$paired<- tclVar(FALSE);
  #e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  e$mu <- tclVar(0);
  
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "Bayesian One Sample t-test")
  
  #Mu Entry
  put_label(e$layout, "Mu:",2,0,sticky="e")
  data_labels2 <- ttkentry(e$layout,
                           textvariable = e$mu, width=5)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(e$layout, "cred.mass:", 3, 0,sticky="e")
  conf_level_frame <- ttkframe(e$layout)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #  #Function Radiobutton
  #  put_label(label_frame, "Alternative:",4,0)
  #  rb_frame <- ttkframe(label_frame)
  #  sapply(c("two.sided","less","greater"), function(i) {
  #    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
  #    tkpack(radio_button, side = "left")
  #  })
  #  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
}

run.Bayes_onesam_t <- function(e) {
    varName <- getVarName(e, 1)
    variable1 <- get(varName,e$dataFrame,inherits=TRUE)
    assign("variable1", variable1, envir = e)	
	
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<- bayes.t.test(data= e$dataFrame, x=variable1, mu=as.numeric(tclvalue(e$mu)), cred.mass = e$conf.level)
    out$data.name<-paste(varName)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(e$wnd)
}

# Bayes_2tfun
layout.Bayes_2t<-function(e){
  e$alternative <- tclVar("two.sided"); 
  e$paired<- tclVar(FALSE);
  e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "2-Sample Bayesian t-test")  
  tkconfigure(e$varLabel[[1]], text = "Variable 1:")
  tkconfigure(e$varLabel[[2]], text = "Variable 2:")
  
  #Conf.level Slider
  put_label(e$layout, "cred. mass:", 3, 0, sticky = "e")
  conf_level_frame <- ttkframe(e$layout)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 60, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #  #Function Radiobutton
  #  put_label(label_frame, "Alternative:",4,0)
  #  rb_frame <- ttkframe(label_frame)
  #  sapply(c("two.sided","less","greater"), function(i) {
  #    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
  #    tkpack(radio_button, side = "left")
  #  })
  #  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Equal Variance Checkbox
  #  put_label ( label_frame , "var.equal:" , 5 , 0)
  #  var_equal_check <-ttkcheckbutton ( label_frame , variable = e$var)
  #  tkgrid ( var_equal_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Paired or Independent Checkbox
  put_label ( e$layout , "paired:" , 6 , 0, sticky = "e")
  paired_check <-ttkcheckbutton (e$layout , variable = e$paired)
  tkgrid (paired_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
}

run.Bayes_2t <- function(e) {
    varName1 <- getVarName(e, 1)   
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)  	
    varName2 <- getVarName(e, 2)    
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)
	
    paired <- as.numeric(tclvalue(e$paired))
    assign("paired", paired, envir = e)
    var<-as.numeric(tclvalue(e$var))
    assign("var", var, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-bayes.t.test(data= e$dataFrame, x=varValue1, y=varValue2, cred.mass = e$conf.level, paired = e$paired, var.equal = e$var)
    out$data.name<-paste(varName1, "and", varName2)
    pos<-1
    envir <- as.environment(pos)
    assign("Bayes_t_Results", out, envir = envir) 
    print(out)
    tkdestroy(e$wnd)
}
  
# Bayes_binom_fun
layout.Bayes_binom<-function(e){

  e$x <- tclVar("0"); 
  e$n <- tclVar("0"); 
  e$p <- tclVar(.5);
  e$conf.level <- tclVar(95); 

  #Begin GUI Setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "Bayesian binomial test")
  columnConfig(e$layout)

  #x Entry
  put_label(e$layout, "Successes:",2,0, sticky = "e")
  data_labels2 <- ttkentry(e$layout,
                           textvariable = e$x, width=5)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #n Entry
  put_label(e$layout, "Trials:",3,0, sticky = "e")
  data_labels2 <- ttkentry(e$layout,
                           textvariable = e$n, width=5)
  tkgrid(data_labels2, row = 3, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2)
  
  #Comp Entry
  put_label(e$layout, "Comp prop:",4,0, sticky = "e")
  data_labels2 <- ttkentry(e$layout,
                           textvariable = e$p, width=5)
  tkgrid(data_labels2, row = 4, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2)
  
  #Conf.level Slider
  put_label(e$layout, "cred.mass:", 5, 0, sticky = "e")
  conf_level_frame <- ttkframe(e$layout)
  tkgrid(conf_level_frame, row = 5, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 60, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
}

run.Bayes_binom <- function(e) {
	conf.level<-NULL
	conf.level<-as.numeric(tclvalue(e$conf.level))*.01
	assign("conf.level", conf.level, envir = e)
	out<- bayes.binom.test(x=as.numeric(tclvalue(e$x)), n=as.numeric(tclvalue(e$n)), comp.theta = as.numeric(tclvalue(e$p)),
						   cred.mass = e$conf.level, n.iter = 15000, progress.bar = "none")
	out$data_name<-"binomial test"
	pos<-1
	envir <- as.environment(pos)
	assign("Results", out, envir = envir) 
	print(out)
	tkdestroy(e$wnd)
}
# Bayes helper functions: # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
HDIofMCMC<-function( sampleVec , credMass=0.95 ) {
  # code from Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}
reorder_coda<-function(s, param_order) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  s <- lapply(s, function(chain) {
    chain[, order(match(gsub("\\[.+\\]$", "", colnames(chain)), param_order))]
  })
  mcmc.list(s)
}
diagnostics.bayes_binom_test<-function(fit) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  cat("  Model parameters and generated quantities\n")
  cat("theta: The relative frequency of success\n")
  cat("x_pred: Predicted number of successes in a replication\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.6) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_one_sample_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_one_sample_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_paired_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_paired_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_two_sample_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_two_sample_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
binom_model_string<-"model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(1, 1)\n  x_pred ~ dbinom(theta, n)\n}"
mcmc_stats<-function(samples, cred_mass = 0.95, comp_val = 0) {
  ## code Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  samples_mat <- as.matrix(samples)
  stats <- data.frame(mean = colMeans(samples_mat))
  stats$sd <- apply(samples_mat, 2, sd)
  cred_mass <- rep(cred_mass, length.out = ncol(samples_mat))
  comp_val <- rep(comp_val, length.out = ncol(samples_mat))
  stats$"HDI%" <- cred_mass * 100
  stats$comp <- comp_val
  stats$HDIlo <- NA
  stats$HDIup <- NA
  for(i in 1:ncol(samples_mat)){
    hdi_lim <- HDIofMCMC(samples_mat[,i], credMass=cred_mass[i])
    stats$HDIlo[i] <- hdi_lim[1]
    stats$HDIup[i] <- hdi_lim[2]  
    stats$"%>comp"[i] <- mean(c(samples_mat[,i] > comp_val[i], 0, 1))
    stats$"%<comp"[i] <- mean(c(samples_mat[,i] < comp_val[i], 0, 1))
  }
  stats$"q2.5%" <- apply(samples_mat, 2, quantile,  probs= 0.025)
  stats$"q25%" <- apply(samples_mat, 2, quantile,  probs= 0.25)
  stats$median <- apply(samples_mat, 2, median)
  stats$"q75%" <- apply(samples_mat, 2, quantile,  probs= 0.75)
  stats$"q97.5%" <- apply(samples_mat, 2, quantile,  probs= 0.975)
  stats$mcmc_se <- NA
  stats$Rhat <- NA
  stats$n_eff <- NA
  if(is.mcmc.list(samples)) {
    stats$mcmc_se <- summary(samples)$statistics[,"Time-series SE"]
    stats$Rhat <- gelman.diag(samples, multivariate = FALSE)$psrf[, 1]
    stats$n_eff <- as.integer(effectiveSize(samples))
  }
  as.matrix(stats) # 'cause it's easier to index
}
run_jags<-function(model_string, data, inits, params, n.chains, n.adapt, n.update, n.iter, thin, progress.bar) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  if(!interactive()) {
    progress.bar <- "none"
  }
  # Set the random number generator and seed based on R's random state (through runif)
  if(is.null(inits$.RNG.seed) & is.null(inits$.RNG.name)) {
    RNGs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", 
              "base::Super-Duper", "base::Mersenne-Twister")
    init_list <- inits
    inits <- function(chain) {
      chain_init_list <- init_list
      chain_init_list$.RNG.seed <- as.integer(runif(1, 0, .Machine$integer.max))
      chain_init_list$.RNG.name <- RNGs[ ((chain - 1) %% 4) +  1 ]
      chain_init_list
    }
  }
  jags_model <- jags.model(textConnection(model_string) , data=data , inits=inits , 
                           n.chains=n.chains , n.adapt=0, quiet=TRUE)
  adapt(jags_model, max(1, n.adapt),  progress.bar="none", end.adaptation=TRUE)
  if(n.update > 0) { 
    update( jags_model, n.update, progress.bar="none")
  }
  mcmc_samples <- coda.samples( jags_model , variable.names= params,
                                n.iter=n.iter, thin=thin, progress.bar=progress.bar)
  mcmc_samples <- reorder_coda(mcmc_samples, params)
  mcmc_samples
}
jags_binom_test<-function(x, n, n.chains=3, n.iter=5000, progress.bar="none") {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  mcmc_samples <- run_jags(binom_model_string, data = list(x = x, n = n), inits = list(theta = (x + 1) / (n + 2)), 
                           params = c("theta", "x_pred"), n.chains = n.chains, n.adapt = 0,
                           n.update = 0, n.iter = n.iter, thin = 1, progress.bar=progress.bar)
  mcmc_samples
}
bayes.binom.test<-function (x, n, comp.theta = 0.5, alternative = NULL, cred.mass = 0.95, n.iter=15000, progress.bar="none", p, conf.level) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  if(! missing(alternative)) {
    warning("The argument 'alternative' is ignored by bayes.binom.test")
  }
  if(! missing(p)) {
    comp.theta <- p
  }
  if(! missing(conf.level)) {
    cred.mass <- conf.level
  }
  ### Begin code from binom.test 
  x_name <- deparse(substitute(x))
  DNAME <- x_name
  n_name <- deparse(substitute(n))
  xr <- round(x)
  if (any(is.na(x) | (x < 0)) || max(abs(x - xr)) > 1e-07) 
    stop("'x' must be nonnegative and integer")
  x <- xr
  if (length(x) == 2L) {
    n <- sum(x)
    x <- x[1L]
  }
  else if (length(x) == 1L) {
    nr <- round(n)
    if ((length(n) > 1L) || is.na(n) || (n < 1) || abs(n - 
                                                       nr) > 1e-07 || (x > nr)) 
      stop("'n' must be a positive integer >= 'x'")
    DNAME <- paste(x_name, "and", n_name)
    n <- nr
  }
  else stop("incorrect length of 'x'")
  if (!missing(comp.theta) && (length(comp.theta) > 1L || is.na(comp.theta) || comp.theta < 0 || 
                               comp.theta > 1)) 
    stop("'comp.theta' or 'p' must be a single number between 0 and 1")
  if (!((length(cred.mass) == 1L) && is.finite(cred.mass) && 
        (cred.mass > 0) && (cred.mass < 1))) 
    stop("'cred.mass' or 'conf.level' must be a single number between 0 and 1")
  ### END code from binom.test
  mcmc_samples <- jags_binom_test(x, n, n.chains = 3, n.iter = ceiling(n.iter / 3) , progress.bar=progress.bar)
  stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = comp.theta)
  bfa_object <- list(x = x, n = n, comp_theta = comp.theta, cred_mass = cred.mass,
                     x_name = x_name, n_name = n_name, data_name = DNAME,
                     mcmc_samples = mcmc_samples, stats = stats) 
  class(bfa_object) <- c("bayes_binom_test", "bayesian_first_aid")
  bfa_object
}
print.bayes_binom_test<-  function(x, ...) {
  s <- format_stats(x$stats)["theta",]
  cat("\n")
  cat("\tBayesian binomial test\n")
  cat("\n")
  cat("data: ", x$data_name, "\n", sep="")
  cat("number of successes = ", x$x,", number of trials = ", x$n, "\n", sep="")
  cat("Estimated relative frequency of success:\n")
  cat(" ", s["median"], "\n")
  cat(s["HDI%"],"% credible interval:\n", sep="")
  cat(" ", s[ c("HDIlo", "HDIup")], "\n")
  cat("The relative frequency of success is more than", s["comp"] , "by a probability of", s["%>comp"], "\n")
  cat("and less than", s["comp"] , "by a probability of", s["%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_one_sample_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - one sample\n")
  cat("\n")
  cat("data: ", x$data_name,  ", n = ", length(x$x),"\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean of ",  x$x_name, ": ", s["mu", "median"], " [", s["mu", "HDIlo"], ", ", s["mu", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$x_name, ": ", s["sigma", "median"], " [", s["sigma", "HDIlo"], ", ", s["sigma", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The mean is more than", s["mu","comp"] , "by a probability of", s["mu","%>comp"], "\n")
  cat("and less than", s["mu", "comp"] , "by a probability of", s["mu", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_paired_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - paired samples\n")
  cat("\n")
  cat("data: ", x$data_name,  ", n = ", length(x$pair_diff),"\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean paired difference: ", s["mu_diff", "median"], " [", s["mu_diff", "HDIlo"], ", ", s["mu_diff", "HDIup"] , "]\n",sep="")
  cat("sd of the paired differences: ", s["sigma_diff", "median"], " [", s["sigma_diff", "HDIlo"], ", ", s["sigma_diff", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The mean difference is more than", s["mu_diff","comp"] , "by a probability of", s["mu_diff","%>comp"], "\n")
  cat("and less than", s["mu_diff", "comp"] , "by a probability of", s["mu_diff", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_two_sample_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - two sample\n")
  cat("\n")
  cat("data: ", x$x_name, " (n = ", length(x$x) ,") and ", x$y_name," (n = ", length(x$y) ,")\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean of ",  x$x_name, ": ", s["mu_x", "median"], " [", s["mu_x", "HDIlo"], ", ", s["mu_x", "HDIup"] , "]\n",sep="")
  cat("mean of ",  x$y_name, ": ", s["mu_y", "median"], " [", s["mu_y", "HDIlo"], ", ", s["mu_y", "HDIup"] , "]\n",sep="")
  cat("difference of the means: ", s["mu_diff", "median"], " [", s["mu_diff", "HDIlo"], ", ", s["mu_diff", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$x_name, ": ", s["sigma_x", "median"], " [", s["sigma_x", "HDIlo"], ", ", s["sigma_x", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$y_name, ": ", s["sigma_y", "median"], " [", s["sigma_y", "HDIlo"], ", ", s["sigma_y", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The difference of the means is greater than", s["mu_diff","comp"] , "by a probability of", s["mu_diff","%>comp"], "\n")
  cat("and less than", s["mu_diff", "comp"] , "by a probability of", s["mu_diff", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
format_stats<-function(s) {
  s_char <- apply(s, c(1,2), function(x) { sign_digits(x, 2) })
  s_char[, "comp"] <- round(s[, "comp"], 3)
  s_char[, "%>comp"] <- num_to_char_with_lim(s[, "%>comp"], 0.001, 0.999,  3)
  s_char[, "%<comp"] <- num_to_char_with_lim(s[, "%<comp"], 0.001, 0.999,  3)
  s_char
}
sign_digits<-function(x,d){
  s <- format(x,digits=d)
  if(grepl("\\.", s) && ! grepl("e", s)) {
    n_sign_digits <- nchar(s) - 
      max( grepl("\\.", s), attr(regexpr("(^[-0.]*)", s), "match.length") )
    n_zeros <- max(0, d - n_sign_digits)
    s <- paste(s, paste(rep("0", n_zeros), collapse=""), sep="")
  } else if(nchar(s) < d && ! grepl("e", s)) {
    s <- paste(s, ".", paste(rep("0", d - nchar(s)), collapse=""), sep="")
  }
  s
}
num_to_char_with_lim<-function(x, low, high, digits) {
  ifelse(x > high, paste(">", round(high, digits) , sep=""),
         ifelse(x < low, paste("<", round(low, digits), sep=""),
                as.character(round(x, digits))))
}
bayes.t.test<-function(x, ...) {
  UseMethod("bayes.t.test")
}
bayes.t.test.formula<-function(formula, data, subset, na.action, ...) {
  ### Original code from t.test.formula ###
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), "term.labels")) != 1L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  data_name <- paste(names(mf), collapse = " by ")
  response_name <- names(mf)[1]
  group_name <- names(mf)[2]
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L) 
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  
  ### Own code starts here ###
  bfa_object <- do.call("bayes.t.test", c(DATA, list(...)))
  bfa_object$data_name <- data_name
  bfa_object$x_name <- paste("group", levels(g)[1])
  bfa_object$y_name <- paste("group", levels(g)[2])
  if(!missing(data)) {
    data_expr <- deparse(substitute(data))
    bfa_object$x_data_expr <- 
      paste("subset(", data_expr, ", as.factor(", group_name, ") == ",
            deparse(levels(g)[1]), ", ", response_name, ", drop = TRUE)", sep="")
    bfa_object$y_data_expr <- 
      paste("subset(", data_expr, ", as.factor(", group_name, ") == ",
            deparse(levels(g)[2]), ", ", response_name, ", drop = TRUE)", sep="")
  } else {
    bfa_object$x_data_expr <- 
      paste(response_name, "[", "as.factor(", group_name, ") == ",
            deparse(levels(g)[1]),"]",sep="")
    bfa_object$y_data_expr <- 
      paste(response_name, "[", "as.factor(", group_name, ") == ",
            deparse(levels(g)[2]),"]",sep="")
  }
  bfa_object  
}
bayes.t.test.default<-function(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
                               mu = 0, paired = FALSE, var.equal = FALSE, cred.mass = 0.95, n.iter = 30000, progress.bar="text", conf.level,...) {
  if(! missing(conf.level)) {
    cred.mass <- conf.level
  }
  if(var.equal) {
    var.equal <- FALSE
    warning("To assume equal variance of 'x' and 'y' is not supported. Continuing by estimating the variance of 'x' and 'y' separately.")
  }
  if(! missing(alternative)) {
    warning("The argument 'alternative' is ignored by bayes.binom.test")
  } 
  ### Original (but slighly modified) code from t.test.default ###
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  if (!missing(cred.mass) && (length(cred.mass) != 1 || !is.finite(cred.mass) || 
                              cred.mass < 0 || cred.mass > 1)) 
    stop("'cred.mass' or 'conf.level' must be a single number between 0 and 1")
  
  # removing incomplete cases and preparing the data vectors (x & y)
  if (!is.null(y)) {
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    data_name <- paste(x_name, "and", y_name)
    if (paired) 
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    x_name <- deparse(substitute(x))
    data_name <- x_name
    if (paired) 
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  
  # Checking that there is enough data. Even though BEST handles the case with
  # one data point it is still usefull to do these checks.
  nx <- length(x)
  mx <- mean(x)
  vx <- var(x)
  if (is.null(y)) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
  }
  else {
    ny <- length(y)
    if (nx < 2) 
      stop("not enough 'x' observations")
    if (ny < 2) 
      stop("not enough 'y' observations")
    my <- mean(y)
    vy <- var(y)
    stderrx <- sqrt(vx/nx)
    stderry <- sqrt(vy/ny)
    stderr <- sqrt(stderrx^2 + stderry^2)
    df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
                                                abs(my))) 
      stop("data are essentially constant")
  }
  ### Own code starts here ###
  if(paired) {
    mcmc_samples <- jags_paired_t_test(x, y, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, y = y, pair_diff = x - y, comp = mu, cred_mass = cred.mass,
                       x_name = x_name, y_name = y_name, data_name = data_name,
                       x_data_expr = x_name, y_data_expr = y_name,
                       mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_paired_t_test", "bayesian_first_aid")
  } else if(is.null(y)) {
    mcmc_samples <- jags_one_sample_t_test(x, comp_mu = mu, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, comp = mu, cred_mass = cred.mass, x_name = x_name, x_data_expr = x_name,
                       data_name = data_name, mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_one_sample_t_test", "bayesian_first_aid")
  } else { # is two sample t.test
    mcmc_samples <- jags_two_sample_t_test(x, y, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, y = y, comp = mu, cred_mass = cred.mass,
                       x_name = x_name, y_name = y_name, data_name = data_name,
                       x_data_expr = x_name, y_data_expr = y_name,
                       mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_two_sample_t_test", "bayesian_first_aid")
  }
  bfa_object
}
jags_two_sample_t_test<-function(x, y, n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  data_list <- list(
    x = x ,
    y = y ,
    mean_mu = mean(c(x, y), trim=0.2) ,
    precision_mu = 1 / (mad0(c(x, y))^2 * 1000000),
    sigma_low = mad0(c(x, y)) / 1000 ,
    sigma_high = mad0(c(x, y)) * 1000 
  )
  inits_list <- list(
    mu_x = mean(x, trim=0.2),
    mu_y = mean(y, trim=0.2),
    sigma_x = mad0(x),
    sigma_y = mad0(y),
    nuMinusOne = 4
  )
  params <- c("mu_x", "sigma_x", "mu_y", "sigma_y", "mu_diff", "sigma_diff","nu", "eff_size", "x_pred", "y_pred")  
  mcmc_samples <- run_jags(two_sample_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar)
  mcmc_samples
}
jags_one_sample_t_test<-function(x, comp_mu = 0,n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  data_list <- list(
    x = x,
    mean_mu = mean(x, trim=0.2) ,
    precision_mu = 1 / (mad0(x)^2 * 1000000),
    sigma_low = mad0(x) / 1000 ,
    sigma_high = mad0(x) * 1000 ,
    comp_mu = comp_mu
  )
  inits_list <- list(mu = mean(x, trim=0.2), sigma = mad0(x), nuMinusOne = 4)
  params <- c("mu", "sigma", "nu", "eff_size", "x_pred")
  mcmc_samples <- run_jags(one_sample_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar)   
  mcmc_samples
}
jags_paired_t_test<-function(x, y, comp_mu = 0, n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  pair_diff <- x - y
  data_list <- list(
    pair_diff = pair_diff,
    mean_mu = mean(pair_diff, trim=0.2) ,
    precision_mu = 1 / (mad0(pair_diff)^2 * 1000000),
    sigma_low = mad0(pair_diff) / 1000 ,
    sigma_high = mad0(pair_diff) * 1000 ,
    comp_mu = comp_mu
  )
  inits_list <- list(mu_diff = mean(pair_diff, trim=0.2), 
                     sigma_diff = mad0(pair_diff), 
                     nuMinusOne = 4)
  params <- c("mu_diff", "sigma_diff", "nu", "eff_size", "diff_pred")
  mcmc_samples <- run_jags(paired_samples_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar) 
  mcmc_samples
}
two_sample_t_model_string<-"model {\n  for(i in 1:length(x)) {\n    x[i] ~ dt( mu_x , tau_x , nu )\n  }\n  x_pred ~ dt( mu_x , tau_x , nu )\n  for(i in 1:length(y)) {\n    y[i] ~ dt( mu_y , tau_y , nu )\n  }\n  y_pred ~ dt( mu_y , tau_y , nu )\n  eff_size <- (mu_x - mu_y) / sqrt((pow(sigma_x, 2) + pow(sigma_y, 2)) / 2)\n  mu_diff <- mu_x - mu_y\n  sigma_diff <-sigma_x - sigma_y \n  \n  # The priors\n  mu_x ~ dnorm( mean_mu , precision_mu )\n  tau_x <- 1/pow( sigma_x , 2 )\n  sigma_x ~ dunif( sigma_low , sigma_high )\n\n  mu_y ~ dnorm( mean_mu , precision_mu )\n  tau_y <- 1/pow( sigma_y , 2 )\n  sigma_y ~ dunif( sigma_low , sigma_high )\n\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne+1\n  nuMinusOne ~ dexp(1/29)\n}"
one_sample_t_model_string<-"model {\n  for(i in 1:length(x)) {\n    x[i] ~ dt( mu , tau , nu )\n  }\n  x_pred ~ dt( mu , tau , nu )\n  eff_size <- (mu - comp_mu) / sigma\n\n  mu ~ dnorm( mean_mu , precision_mu )\n  tau <- 1/pow( sigma , 2 )\n  sigma ~ dunif( sigma_low , sigma_high )\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne + 1 \n  nuMinusOne ~ dexp(1/29)\n}"
paired_samples_t_model_string<-"model {\n  for(i in 1:length(pair_diff)) {\n    pair_diff[i] ~ dt( mu_diff , tau_diff , nu )\n  }\n  diff_pred ~ dt( mu_diff , tau_diff , nu )\n  eff_size <- (mu_diff - comp_mu) / sigma_diff\n  \n  mu_diff ~ dnorm( mean_mu , precision_mu )\n  tau_diff <- 1/pow( sigma_diff , 2 )\n  sigma_diff ~ dunif( sigma_low , sigma_high )\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne + 1 \n  nuMinusOne ~ dexp(1/29)\n}"
mad0<-function(..., na.rm=TRUE) {
  mad_est <- mad(..., na.rm=na.rm)
  if(mad_est != 0) {
    mad_est
  } else {
    sd(..., na.rm=na.rm)
  }
}
paired_samples_t_test_model_code<-function(pair_diff, comp_mu) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(pair_diff)) {
  pair_diff[i] ~ dt( mu_diff , tau_diff , nu )
  }
  diff_pred ~ dt( mu_diff , tau_diff , nu )
  eff_size <- (mu_diff - comp_mu) / sigma_diff
  mu_diff ~ dnorm( mean_mu , precision_mu )
  tau_diff <- 1/pow( sigma_diff , 2 )
  sigma_diff ~ dunif( sigma_low , sigma_high )
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne + 1 
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on mu and sigma.
  mean_mu = mean(pair_diff, trim=0.2)
  precision_mu = 1 / (mad(pair_diff)^2 * 1000000)
  sigma_low = mad(pair_diff) / 1000 
  sigma_high = mad(pair_diff) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(
    mu_diff = mean(pair_diff, trim=0.2),
    sigma_diff = mad(pair_diff),
    nuMinusOne = 4)
  data_list <- list(
    pair_diff = pair_diff,
    comp_mu = comp_mu,
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu_diff", "sigma_diff", "nu", "eff_size", "diff_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
two_sample_poisson_model_code<-function(x, t) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(group_i in 1:2) {
  x[group_i] ~ dpois(lambda[group_i] * t[group_i])
  lambda[group_i] ~ dgamma(0.5, 0.00001)
  x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"
  # Running the model
  model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
  samples <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
one_sample_poisson_model_code<-function(x, t) {
  # The model string written in the JAGS language
  model_string <- "model {
  x ~ dpois(lambda * t)
  lambda ~ dgamma(0.5, 0.00001)
  x_pred ~ dpois(lambda * t)
}"
  # Running the model
  model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
  samples <- coda.samples(model, c("lambda", "x_pred"), n.iter=5000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
summary.bayes_two_sample_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$x_name, ", n = ", length(object$x), "\n", sep="")
  cat(object$y_name, ", n = ", length(object$y), "\n", sep="")
  cat("\n")
  print_bayes_two_sample_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
summary.bayes_paired_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$x_name, ", n = ", length(object$x), "\n", sep="")
  cat(object$y_name, ", n = ", length(object$y), "\n", sep="")
  cat("\n")
  print_bayes_paired_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
summary.bayes_one_sample_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$data_name, ", n = ", length(object$x), "\n", sep="")
  cat("\n")
  print_bayes_one_sample_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
model.code.bayes_one_sample_t_test<-function(fit) {  
  cat("### Model code for Bayesian estimation supersedes the t test - one sample ###\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("comp_mu <- ", fit$comp, "\n")
  cat("\n")
  pretty_print_function_body(one_sample_t_test_model_code)
  invisible(NULL)
}
model.code.bayes_paired_t_test<-function(fit) {
  cat("## Model code for Bayesian estimation supersedes the t test - paired samples ##\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("y <-", fit$y_data_expr, "\n")
  cat("pair_diff <- x - y\n")
  cat("comp_mu <- ", fit$comp, "\n")
  cat("\n")
  pretty_print_function_body(paired_samples_t_test_model_code)
  invisible(NULL)
}
model.code.bayes_two_sample_t_test<-function(fit) {
  cat("## Model code for Bayesian estimation supersedes the t test - two sample ##\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("y <-", fit$y_data_expr, "\n")
  cat("\n")
  pretty_print_function_body(two_sample_t_test_model_code)
  invisible(NULL)
}
print_bayes_two_sample_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu_x: the mean of", x$x_name, "\n")
  cat("sigma_x: the scale of", x$x_name,", a consistent\n  estimate of SD when nu is large.\n")
  cat("mu_y: the mean of", x$y_name, "\n")
  cat("sigma_y: the scale of", x$y_name,"\n")
  cat("mu_diff: the difference in means (mu_x - mu_y)\n")
  cat("sigma_diff: the difference in scale (sigma_x - sigma_y)\n")
  cat("nu: the degrees-of-freedom for the t distribution\n")
  cat("  fitted to",x$data_name , "\n")
  cat("eff_size: the effect size calculated as \n", sep="")
  cat("  (mu_x - mu_y) / sqrt((sigma_x^2 + sigma_y^2) / 2)\n", sep="")
  cat("x_pred: predicted distribution for a new datapoint\n")
  cat("  generated as",x$x_name , "\n")
  cat("y_pred: predicted distribution for a new datapoint\n")
  cat("  generated as",x$y_name , "\n")
  invisible(NULL)
}
print_bayes_paired_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu_diff: the mean pairwise difference between", x$x_name, "and", x$y_name, "\n")
  cat("sigma_diff: the scale of the pairwise difference, a consistent\n  estimate of SD when nu is large.\n")
  cat("nu: the degrees-of-freedom for the t distribution fitted to the pairwise difference\n")
  cat("eff_size: the effect size calculated as (mu_diff - ", x$comp ,") / sigma_diff\n", sep="")
  cat("diff_pred: predicted distribution for a new datapoint generated\n  as the pairwise difference between", x$x_name, "and", x$y_name,"\n")
}
print_bayes_one_sample_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu: the mean of", x$data_name, "\n")
  cat("sigma: the scale of", x$data_name,", a consistent\n  estimate of SD when nu is large.\n")
  cat("nu: the degrees-of-freedom for the t distribution fitted to",x$data_name , "\n")
  cat("eff_size: the effect size calculated as (mu - ", x$comp ,") / sigma\n", sep="")
  cat("x_pred: predicted distribution for a new datapoint generated as",x$data_name , "\n")
}
print_mcmc_info<-function(mcmc_samples) {
  cat("\n", "Iterations = ", start(mcmc_samples), ":", end(mcmc_samples), "\n", sep = "")
  cat("Thinning interval =", thin(mcmc_samples), "\n")
  cat("Number of chains =", nchain(mcmc_samples), "\n")
  cat("Sample size per chain =", (end(mcmc_samples) - start(mcmc_samples))/thin(mcmc_samples) + 1, "\n")
}
print_diagnostics_measures<-function(s) {
  cat("  Diagnostic measures\n")
  print(s[, c("mean", "sd", "mcmc_se", "n_eff", "Rhat")])
  cat("\n")
  cat("mcmc_se: the estimated standard error of the MCMC approximation of the mean.\n")
  cat("n_eff: a crude measure of effective MCMC sample size.\n")
  cat("Rhat: the potential scale reduction factor (at convergence, Rhat=1).\n")
}
pretty_print_function_body<-function(fn) {
  fn_string <- deparse(fn, control="useSource")
  fn_string <- gsub("^  ", "", fn_string)
  cat(paste(fn_string[-c(1, length(fn_string))], collapse="\n"))
}
one_sample_t_test_model_code<-function(x, comp_mu) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(x)) {
  x[i] ~ dt( mu , tau , nu )
  }
  x_pred ~ dt( mu , tau , nu )
  eff_size <- (mu - comp_mu) / sigma
  mu ~ dnorm( mean_mu , precision_mu )
  tau <- 1/pow( sigma , 2 )
  sigma ~ dunif( sigma_low , sigma_high )
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne + 1 
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on mu and sigma.
  mean_mu = mean(x, trim=0.2)
  precision_mu = 1 / (mad(x)^2 * 1000000)
  sigma_low = mad(x) / 1000 
  sigma_high = mad(x) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(mu = mean(x, trim=0.2), sigma = mad(x), nuMinusOne = 4)
  data_list <- list(
    x = x,
    comp_mu = comp_mu,
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu", "sigma", "nu", "eff_size", "x_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
two_sample_t_test_model_code<-function(x, y) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(x)) {
  x[i] ~ dt( mu_x , tau_x , nu )
  }
  x_pred ~ dt( mu_x , tau_x , nu )
  for(i in 1:length(y)) {
  y[i] ~ dt( mu_y , tau_y , nu )
  }
  y_pred ~ dt( mu_y , tau_y , nu )
  eff_size <- (mu_x - mu_y) / sqrt((pow(sigma_x, 2) + pow(sigma_y, 2)) / 2)
  mu_diff <- mu_x - mu_y
  sigma_diff <-sigma_x - sigma_y 
  
  # The priors
  mu_x ~ dnorm( mean_mu , precision_mu )
  tau_x <- 1/pow( sigma_x , 2 )
  sigma_x ~ dunif( sigma_low , sigma_high )
  mu_y ~ dnorm( mean_mu , precision_mu )
  tau_y <- 1/pow( sigma_y , 2 )
  sigma_y ~ dunif( sigma_low , sigma_high )
  
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne+1
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on the mu's and sigma's.
  mean_mu = mean( c(x, y), trim=0.2)
  precision_mu = 1 / (mad( c(x, y) )^2 * 1000000)
  sigma_low = mad( c(x, y) ) / 1000 
  sigma_high = mad( c(x, y) ) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(
    mu_x = mean(x, trim=0.2), mu_y = mean(y, trim=0.2),
    sigma_x = mad(x), sigma_y = mad(y),
    nuMinusOne = 4)
  data_list <- list(
    x = x, y = y,    
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu_x", "mu_y", "mu_diff", "sigma_x", "sigma_y", "sigma_diff",
              "nu", "eff_size", "x_pred", "y_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the Posterior
  plot(samples)
  summary(samples)  
  }
