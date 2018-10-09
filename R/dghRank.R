dghRank = function(y_vec, mean_vec, var_vec, ind_mat, z_star = 1.96){
    #Obtains Draper-Gittoes-Helkey scores
    #y_vec, mean_vec, var_vec <Nx1> vectors
    #var_vec should be computed w/ law of total variance (i.e. propogate posterior variance)
    #ind_mat <Nxq> indicator matrix

	if (is.null(ind_mat)){return(NULL)}
	
    #Diagonal N matrix from ind_mat
    n_mat = diag( apply(ind_mat, 2, sum))
    p = dim(ind_mat)[2]

    #Implement DGH
    n_inv = solve(n_mat)
    ind_mat = as.matrix(ind_mat) #TODO speedup later w/ sparse matrices
    e_vec =  n_inv %*% (t(ind_mat) %*% mean_vec)
    o_vec = n_inv %*% (t(ind_mat) %*% y_vec)
    d_vec = o_vec - e_vec
    s_vec = n_inv %*% sqrt( t(ind_mat) %*% var_vec)

    #Compute posterior bounds w/ Bonferroni correction
    return(list(
      E = e_vec, D = d_vec, S = s_vec, Z = d_vec/s_vec,
      lower = d_vec - z_star * s_vec,
      upper = d_vec + z_star * s_vec
    ))
  }
