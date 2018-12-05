
correctInf = function(mat){
	#Convert Inf values to 0
	mat[is.infinite(mat)] = 0
	return(mat)
}
	
linCholSolver = function(R, y){
		#' Solve System of Equations w/ Cholesky
		#'
		#' \code{linCholSolver} solves Ax = y for x.
		#' It is first required to take the Cholesky decomposition,
		#' obtaining A = t(R) %*% R. This must be performed outside of this function for speed.
		#' Results should be checked against a known solver, as this function optimizes for speed.
		#'
		#' @param R - Upper triangular matrix of dimension <n x p>; Cholesky decomposition of A.
		#' @param y - Vector of length n.
		q = forwardsolve(t(R), y)
		return(backsolve(R, q))
}


  toScore = function(summary_dat, z_star){
  #This is just a linear transform, mean and variance transform as well
  #Requires the effect be computed with an estimate and an SE
  if(is.null(summary_dat)){return(NULL)}
	y_bar = mean(summary_dat$effect_est)
	sd_obs = sd(summary_dat$effect_est)
	#est = (summary_dat$effect_est - y_bar) / sd_obs
	est = summary_dat$effect_est / sd_obs
	s = summary_dat$effect_se / sd_obs
	return(
		data.frame( score_est = est, 
	score_lower = est - z_star * s, score_upper = est + z_star * s,
	score_se = s)
	)		
  }
  
   computeZstar = function(alpha, p, bonferroni = TRUE){
		if (!bonferroni){p = 1}
		return( qnorm(1 - alpha / p))
	  }
	  
  
