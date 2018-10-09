fitBabyMonitor = function(minimal_data, num_cat, num_cont, subset = FALSE,
                          var_intercept = 40, var_cat = 10, var_cat_interaction = 10, var_cont = 10,
                          iters = 100, burn_in = 100, alpha = 0.05,
                          verbose = TRUE,
                          outcome.na = 'set0', subset.na = 'category', #Behavior for handling NA values
                          cat.na = 'category', cont.na = 'median'){
	#' Fit Baby-MONITOR for CPQCC/VON
	#'
	#' \code{fitBabyMonitor} comprehensively applies the Baby-MONITOR score
	# to a \code{minimal_data} file. Designed for CPQCC/VON.
	#' Returns a large list. Use inst_mat for institution rankings,
	#' full_subset_mat_baseline and full_subset_mat_nobaseline for subset rankings,
	#' and subset_baseline_mat and subset_nobaseline_mat for subset rankings within institution.
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
	#' @param num_cat Scalar number of categorical variables.
	#' @param num_cont Number of continuous variables.
	#' @param var_intercept Prior variance for intercept parameter.
	#' @param var_cat Prior variance for categorical parameters.
	#' @param var_cat_interaction Prior variance for categorical interaction parameters.
	#' @param var_cont Prior variance for continuous parameters.
	#' @param iters Number of MCMC iterations to use.
	#' @param burn_in Number of initial iterations to discard for burn in.
	#' @param sparse Should design_matrix be stored as Sparse matrix? Requires Matrix package.
	#' @param alpha We look at posterior (1-alpha)\% posterior intervals.
	#' @param verbose If TRUE, display status messages while fitting
	#' @param subset If TRUE, perform subset fitting tasks
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
  
  dat = processMinimalData(minimal_data, num_cat, num_cont,
                                      subset = subset, outcome.na = outcome.na, subset.na = subset.na, cat.na = cat.na, cont.na = cont.na)  								 
	pcf_vec = NULL
	if (!is.null(dat$cat_var_mat)){
	  pcf_vec = apply(dat$cat_var_mat, 1, paste, collapse = '-') #For DG ranking
	}	

  #Partition data by institution, subset, and institution-subset
  p_inst = partitionSummary(dat$y, dat$inst_vec)
  p_subset = partitionSummary(dat$y, dat$subset)
  p_inst_subset = NULL
  if (subset){p_inst_subset = partitionSummary(dat$y, dat$inst, dat$subset)} #To save time, don't always compute

  #Build model additively
  model_mat_cat = modelMatrix(dat$cat_var_mat, interactions = TRUE)
  model_mat_cont = modelMatrix(dat$cont_var_mat)
  model_mat = cbind(rep(1, dat$N), model_mat_cat, model_mat_cont)

  #TODO intercept, cat, interaction, cont variances. No longer fit w/ subset or inst :)
  #With this sample size, the posterior is overwhelmed by the likelihood and so the prior doesn't really matter
  prior_var_vec = rep(var_cat, dim(model_mat)[2])#TODO fix this prior specification

  #Fit the design matrix to a probit model
  #TODO, modify code to use Bayesian Regression for continuous predictors
  mcmc_iters = probitFit(dat$y, model_mat, prior_var_vec,
                         iters = iters + burn_in)[-(1:burn_in),  ]

  #Large matrix to compute and store, 
  #TODO avoid computing this
  #TODO get the namespace working so we can do this multiplication as a spase matrix
  p_i_mat = pnorm(tcrossprod(as.matrix(model_mat), mcmc_iters))

  #Extract rowwise mean, its implied observational variance, and rowwise variance
  p_i_vec = apply(p_i_mat, 1, mean)
  p_i_var_vec = apply(p_i_mat, 1, var)
  pq_i_vec = p_i_vec * (1-p_i_vec)
  p_i_overall_var_vec =  pq_i_vec + p_i_var_vec #Law of total variance

  #Compute z_star based on Bonferonni correction
  dat$z_star = qnorm( 1 - alpha/dat$p)

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
                     effect_s = dgh_dat$S, effect_z = dgh_dat$Z))
    return(out)
  }
  inst_mat = summaryMat(p_inst, inst_effects)
  subset_mat = summaryMat(p_subset, subset_effects)
  subset_inst_mat = summaryMat(p_inst_subset, subset_inst_effects)


  
	inst_mat = cbind(inst_mat, toScore(inst_mat, dat$z_star))
	subset_mat_baseline = subset_inst_mat_baseline = NULL
  if(subset){
	  subset_mat = cbind(subset_mat, toScore(subset_mat, dat$z_star))
	  subset_inst_mat = cbind(subset_inst_mat, toScore(subset_inst_mat,dat$z_star))
	  
	  ##Finally, add in the baseline #TODO fix this hard-coding
	  subset_mat_baseline = cbind( subset_mat[ ,1:3],
	  #Subset baseline coding
		toBaseline(subset_mat$effect_est, subset_mat$effect_s, dat$z_star))
	  
		#Subset-inst baseline coding
	   temp_baseline_mat = data.frame()
	   for(i in unique(subset_inst_mat[ ,1])){
		indices = subset_inst_mat[ ,1] == i
		inst_specific = toBaseline(subset_inst_mat$effect_est[indices], subset_inst_mat$effect_s[indices], dat$z_star)
		temp_baseline_mat = rbind(temp_baseline_mat,
			inst_specific)
	   }
	   subset_inst_mat_baseline = cbind( subset_inst_mat[ ,1:4], temp_baseline_mat)	  
}

  return(list(
    dat = dat, #The parsed input, output of processMinimalData()
  	pcf_vec = pcf_vec, #Useful for computing D-G rankings
    inst_mat = inst_mat,
	full_subset_mat_nobaseline = subset_mat,
	full_subset_mat_baseline = subset_mat_baseline,
	subset_nobaseline_mat = subset_inst_mat,
	subset_baseline_mat = subset_inst_mat_baseline
	 ))
}