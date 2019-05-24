fitProbitModel = function(minimal_data, num_cat, prior_inst_var = 1.5, prior_cat_var = 0.5, iters = 100, burn_in = 10){
	#For num_cont = 0
	
	#Parse w/ fitBabyMonitor
	r = fitBabyMonitor(minimal_data, num_cat, num_cont = 0, iters = 10, dat_out = TRUE)
	
	n_inst = dim(r$inst_mat)[1]
	
	model_mat = cbind(modelMatrix(r$dat$inst_vec, intercept = TRUE), scale(r$dat$model_mat[ ,-1] ))
	
	#Construct prior var
	total_p = dim(model_mat)[2]
	
	prior_var_vec = rep(prior_cat_var, total_p)
	prior_var_vec[1:n_inst] = prior_inst_var
	
	#FIt
	mcmc_iters = probitFit(r$dat$y,  model_mat, prior_var_vec,
	  iters = iters + burn_in)[-(1:burn_in),1:n_inst]
	
	#Now create vector of z's
	covv = cov(as.matrix(mcmc_iters))
	
	mcmc_iters[ ,1] = 0
	
	z_model_untransformed = mcmc_iters %*% diag(sqrt(1/diag(covv)))
	z_est = apply(z_model_untransformed, 2, median)

	return(z_est)
}