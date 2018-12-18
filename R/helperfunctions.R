  idStr = function(x){ #Make sure id (potentially two columns) is a vector
		as.character(paste(x, collapse = '-'))
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


  
   computeZstar = function(alpha, p, bonferroni = TRUE){
		if (!bonferroni){p = 1}
		return( qnorm(1 - alpha / p))
	  }
	  
  
