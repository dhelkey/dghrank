  idStr = function(x){ #Make sure id (potentially two columns) is a vector
		as.character(paste(x, collapse = '-'))
	  }
	  
	  
toQuantiles = function(x, levels = 4){
	#Recode a numeric variable as a catgorical w/ 4 levels
	if (length(x) == 0){return(x)}
  q_vec = quantile(x, probs = seq(0, 1, length = levels + 1), na.rm = TRUE)
  q_vec[1] = -Inf
  return(as.factor(cut(x, q_vec, labels = 1:levels)))
}
	  
	  
	  
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

	criticalValues = function(n_vec, q = NULL, alpha = 0.01, bonferroni = TRUE, t_scores = TRUE){
		#' Compute critical values for a (100 - alpha)% CI
		#'
		#'@param n_vec Vector of length (q x 1) of sample sizes of the q institutions or categories.
		#' @inheritParams fitBabyMonitor	
		if(is.null(q)){q = length(n_vec)}
		adj = 1
		if (bonferroni){adj = q}
		p_interval = 1 - alpha / (2 * adj)
		QT = function(p,n_vec){
			out = rep(0, length(n_vec))
			out[n_vec > 0] = qt(p, n_vec[n_vec > 0])
			return(out)
		}
		if (t_scores){c_values = QT(p_interval, n_vec)
		} else{c_values = rep( qnorm(p_interval), length(n_vec))}
		return(c_values)
	}
  
  Head = function(x){head(x, 15)}
  
  addIntervals = function(data_frame, alpha = 0.01, bonferroni = TRUE, t_scores = TRUE, composite = FALSE){
	#' Add upper and lower bounds to score and est bounds...
	#'
	#'@inheritParams fitBabyMonitor
	if (is.null(data_frame)){return(NULL)}
	
	df_vec = data_frame$n - 1
	df_vec[df_vec == 0] = 1
	if (composite){df_vec = data_frame$ws_approximate_df}
	
	
	
	#If score_se = 0, the value is fixed, and is not counted in multiple comparisons
	q = sum(data_frame$score_se != 0)
	c_values = criticalValues(df_vec, q, alpha = alpha, bonferroni = bonferroni,
	t_scores = t_scores)

	#Critical values.
	data_frame$score_lower = data_frame$score_est -  c_values * data_frame$score_se
	data_frame$score_upper = data_frame$score_est +  c_values * data_frame$score_se
	
		
	if ('effect_est' %in% names(data_frame)){
		data_frame$effect_lower = data_frame$effect_est -  c_values * data_frame$effect_se
		data_frame$effect_upper = data_frame$effect_est +  c_values * data_frame$effect_se
	}
	 data_frame$critical_value = c_values
	
	 return(data_frame)

  }
