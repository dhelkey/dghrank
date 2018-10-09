
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
  if(is.null(summary_dat)){return(NULL)}
	y_bar = mean(summary_dat$effect_est)
	sd_obs = sd(summary_dat$effect_est)
	est = (summary_dat$effect_est - y_bar) / sd_obs
	s = summary_dat$effect_s / sd_obs
	return(
		data.frame( score_est = est, 
	score_lower = est - z_star * s, score_upper = est + z_star * s,
	effect_s = s, score_z = est/s )
	)		
  }
  
  toBaseline = function(est_vec, s_vec, z_star){
	base = est_vec[1]
	est_vec = est_vec - base
	est_vec[1] = 0
	
	z_vec = est_vec / s_vec	
	s_vec[1] = 0
	
	lower = est_vec - z_star * s_vec
	upper = est_vec + z_star * s_vec 
	
	return(data.frame(effect_est = est_vec, 
		effect_lower = lower, effect_upper = upper,
		effect_s = s_vec, effect_z = z_vec))
  }