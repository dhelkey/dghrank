# dghRank = function(y_vec, mean_vec, var_vec, ind_mat){

	# if (is.null(ind_mat)){return(NULL)}
    
	# #Diagonal N matrix from ind_mat
    # n_mat = diag(apply(ind_mat, 2, sum))
    # p = dim(ind_mat)[2]
	
    # #Implement DGH
    # n_inv = solve(n_mat)
    # ind_mat = as.matrix(ind_mat)
    # e_vec =  n_inv %*% (t(ind_mat) %*% mean_vec)
    # o_vec = n_inv %*% (t(ind_mat) %*% y_vec)
    # d_vec = o_vec - e_vec
    # s_vec = n_inv %*% sqrt( t(ind_mat) %*% var_vec)

    # #Compute posterior bounds w/ Bonferroni correction
    # return(list(
      # E = e_vec, D = d_vec, S = s_vec, Z = d_vec/s_vec
    # ))
  # }

  
    #' Computes Draper-Gittoes-Helkey scores
    #'
	#'y_vec, mean_vec, var_vec <Nx1> vectors
    #'ind_mat <Nxq> indicator matrix
	#' Optimized by Lucy Greenberg
  dghRank<-
function (y_vec, mean_vec, var_vec, ind_mat) 
{
    if (is.null(ind_mat)) return(NULL)
      n_inv = diag(1/Matrix::colSums(ind_mat))
      n_inv = Matrix(n_inv,sparse = TRUE)
          p = dim(ind_mat)[2]
          t_ind_mat<-Matrix::t(ind_mat)
          e_vec = matrix(n_inv %*% (t_ind_mat %*% mean_vec))
          o_vec = matrix(n_inv %*% (t_ind_mat %*% y_vec))
          d_vec = o_vec - e_vec
          s_vec = matrix(n_inv %*% sqrt(t_ind_mat %*% var_vec))
          return(list(E = e_vec, 
                      D = d_vec, 
                      S = s_vec, 
                      Z = d_vec/s_vec
                      ))  
  
}