#' Fit Baby-MONITOR for CPQCC/VON
	#'
	#' \code{fitBabyMonitor} applies the Baby-MONITOR score  to a single performance indicator. Designed for CPQCC/VON usage. 
	#'
	#' @param minimal_data Data_frame with a particular format:
	#'
	#' 1st column: Outcome vector (0-1 encoding)
	#'
	#' 2nd column: Institution ID
	#'
	#' 3rd column: Subset (if subset == TRUE):
	#'
	#' Next: num_cat columns of categorical variables (num_cat can equal 0)
	#'
	#' Next: num_cont columns of continuous variables (num_cont can equal 0)
	#'
	#' @param subset logical; if TRUE perform analysis with a subset variable ( and \code{minimal_data} must have subset data in the 3rd column)
	#'
	#' @param num_cat integer. Number of categorical risk adjusters.
	#' @param num_cont integer. Number of continious risk adjusters.
	#' @param var_intercept scalar. Prior variance for the intercept.
	#' @param var_cat scalar. Prior variance for categorical risk adjuster parameters. 
	#' @param var_cat_interaction scalar. Prior variance for interactions between categorical risk adjusters. 
	#' @param var_cont scalar. Prior variance for continuous risk adjusters.
	#' @param iters integer. Desired number of posterior MCMC iterations.
	#' @param burn_in integer. Number of 'burn-in' MCMC iterations to discard. 
	#' @param alpha scalar in (0,1). Statistical signifiance threshold. Posterior (1-alpha)\% intervals are generated.
	#' @param bonferroni logical; if TRUE, posterior intervals are widened with the Bonferroni correction.
	#' @param dat_out logical; if TRUE, export MCMC iterations and other parameters in the \code{dat} component.
	#' @param outcome_na Method for handling any NA values in the outcome vec. 'remove' removes rows with NA outcomes while 'set0' keeps the row and sets the outcome to 0.
	#' @param subset_na Method for handling any NA subset values. 'remove' removes rows with NA subset values while 'category' makes a new subset category (coded as 99) for NA values.
	#' @param cat_na Method for handling any NA values in the categorical risk adjusters. 'remove' removes rows with NA values while 'category' makes a new category (coded as 99) for NA catorical risk adjusters.
	#' @param cont_na Method for handling any NA values in the continous risk adjusters. 'remove' removes rows with NA values while 'median' replaces NA with the median value of the risk adjuster. 
	#' @return
	#'	Returns a large list with the following components:
	#'
	#'		inst_mat: Matrix (one row per institution) computing summary statistics, DGH institution ranking, and intervals with both effect-size and standardized scores.
	#'
	#' 	     full_subset_mat_baseline, full_subset_mat_nobaseline: Matrix with rankings and intervals for the various subset categories.
	#'
	#'
	#'	    subset_baseline_mat, subset_nobaseline_mat: Matrix with rankings and intervals for subset categories within institution.
	#'
	#'
	#'		group_labels: A vector of the names of each institution
	#'
	#'	    mcmc_fit: Matrix of MCMC iterations for each coefficient
	#'		dg_z: Matrix of computed z score for each institution at each MCMC iterations.
	#'
	#'		coefs: Names of each coefficient (1st is intercept, then institution, then everything else)
	#'
	#'		prior_var_vec: Vector of prior variances for each coefficient
	#'
	#'		model_matrix: Design matrix
	#'
	#'
	#'

fitBabyMonitor = function(minimal_data,
                          num_cat,
                          num_cont,
                          subset = FALSE,
						  subset_base_catgory = 1,
                          var_intercept = 40,
                          var_cat = 10,
                          var_cat_interaction = 10,
                          var_cont = 10,
                          iters = 100,
                          burn_in = 100,
                          alpha = 0.05,
                          n_cutoff = 5,
                          bonferroni = TRUE,
                          outcome_na = 'set0',
                          subset_na = 'category',
                          cat_na = 'category',
                          cont_na = 'median',
                          dat_out = FALSE){
	

  dat = parseMinimalData(minimal_data, num_cat, num_cont,
                                      subset = subset, outcome_na = outcome_na, subset_na = subset_na, cat_na = cat_na, cont_na = cont_na, n_cutoff = n_cutoff)

  #Partition data by institution, subset, and institution-subset
  p_inst = partitionSummary(dat$y, dat$inst_vec)
  p_subset = partitionSummary(dat$y, dat$subset)
  p_inst_subset = NULL
  if (subset){p_inst_subset = partitionSummary(dat$y, dat$inst, dat$subset_vec)} #To save time, don't always compute

  #Build model additively
  model_mat_cat = modelMatrix(dat$cat_var_mat, interactions = TRUE)
  model_mat_cont = modelMatrix(dat$cont_var_mat)
  model_mat = cbind( rep(1, dat$N), model_mat_cat, model_mat_cont)
  if (num_cat == 0){  model_mat = cbind( rep(1, dat$N), model_mat_cont)} #Not sure why cbind doesn't handle the NULL well..

  #Build prior variance vector
  prior_var_cat = NULL
  if (num_cat > 0){
    prior_var_cat = rep(var_cat, dim(model_mat_cat)[2])
	prior_var_cat[grep(':',
			colnames(model_mat_cat))] = var_cat_interaction
  }
   prior_var_cont = NULL
   if (num_cont > 0){
	prior_var_cont = rep(var_cont, dim(model_mat_cont)[2])
  }
  prior_var_vec = c(var_intercept, prior_var_cat, prior_var_cont)

  
  #Fit the design matrix to a probit model
  mcmc_iters = probitFit(dat$y, model_mat, prior_var_vec,
                         iters = iters + burn_in)[-(1:burn_in),  ]

  #MCMC matrix of individual level pobabilities
  p_i_mat = pnorm(as.matrix(tcrossprod(model_mat, mcmc_iters))) 

  #Extract rowwise mean, implied observational variance, and rowwise variance
  p_i_vec = apply(p_i_mat, 1, mean)
  p_i_var_vec = apply(p_i_mat, 1, var)
  pq_i_vec = p_i_vec * (1-p_i_vec)
  p_i_overall_var_vec =  pq_i_vec + p_i_var_vec #Law of total variance
  rm(p_i_mat) #Remove to save space
  
  #Compute z_star based on Bonferonni correction
  dat$z_star = computeZstar(alpha, dat$p, bonferroni)

  #Compute DGH effect scores
  inst_effects = dghRank(dat$y, p_i_vec,p_i_overall_var_vec, p_inst$ind_mat, dat$z_star)
  subset_effects  = dghRank(dat$y, p_i_vec, p_i_overall_var_vec, p_subset$ind_mat, dat$z_star)
  subset_inst_effects = dghRank(dat$y, p_i_vec, p_i_overall_var_vec, p_inst_subset$ind_mat, dat$z_star)

  #Build human readable data.frames
  summaryMat = function(part_dat, dgh_dat){
    #Build summary mat
    out = cbind(part_dat$part_mat,
          data.frame(n = part_dat$n, o_mean = part_dat$o_mean, effect_est = dgh_dat$D,
                      effect_lower = dgh_dat$lower, effect_upper = dgh_dat$upper,
                     effect_se = dgh_dat$S, stat_z = dgh_dat$Z))
    return(out)
  }
  inst_mat = summaryMat(p_inst, inst_effects)
  subset_mat = summaryMat(p_subset, subset_effects)
  inst_subset_mat = summaryMat(p_inst_subset, subset_inst_effects)
	inst_mat = cbind(inst_mat, toScore(inst_mat, dat$z_star))
	names(inst_mat)[1] = 'inst'
	subset_mat_baseline = inst_subset_mat_baseline = NULL
  if(subset){
	names(subset_mat)[1] = 'subset'
	  z_star_subset = computeZstar(alpha, length(unique(dat$subset_vec)), bonferroni)
	  z_star_inst_subset = computeZstar(alpha, dim(inst_subset_mat)[1], bonferroni)
	  
	   subset_mat = cbind(subset_mat, toScore(subset_mat, z_star_subset))
	  
	  inst_subset_mat = cbind(inst_subset_mat, toScore(inst_subset_mat,z_star_inst_subset))
	  names(inst_subset_mat)[1:2] = c('inst','subset')

	  ##Finally, add in the baseline 
	  #Subset baseline coding
	  subset_mat_baseline = toBaseline(subset_mat)
		
	#Add in inst_baseline 
	inst_subset_list_baseline = lapply(unique(inst_subset_mat$inst),
		function(inst) toBaseline(inst_subset_mat[inst_subset_mat$inst == inst,  ], inst=TRUE))
	inst_subset_mat_baseline = do.call('rbind', inst_subset_list_baseline)
}

	if (!dat_out){#Saves storage space.
	  dat = NULL
	} else{#Export variables
		dat$model_mat = model_mat; dat$mcmc_iters = mcmc_iters; dat$prior_var_vec = prior_var_vec
	}

  return(list(
    dat = dat, #The parsed input, output of parseMinimalData()
    inst_mat = inst_mat,
	subset_mat_nobaseline = subset_mat,
	subset_mat_baseline = subset_mat_baseline,
	inst_subset_mat_nobaseline = inst_subset_mat,
	inst_subset_mat_baseline = inst_subset_mat_baseline
	 ))
}
