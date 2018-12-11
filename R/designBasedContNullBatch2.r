designBasedContNullBatch2 = function(gamma_vec, outcome_mat, pcf_cat_vec, instid_vec){
	print('hi')
	#Input an N x iterations matrix of null data
	
	##Computes D-G z scores less cost prohabitivly....
	#There is a lot of redundancy in the computation

	##TODO add tests to make sure weird ordering isnt hurting anything
	
	##TODO make sure to test that it will work with an input vector...
	
	###Draper Gittoes
	#M student types - pcf categories
	#N universities
	#n_ga,,a gamma types
	#iters, number of iterations
	#N_total - overall number of students
	
	#Extract variables
	pcf_cat_vec = as.factor(pcf_cat_vec)
	pcf_cat_vec_i = as.integer(pcf_cat_vec)
	instid_vec = as.factor(instid_vec)
	instid_vec_i = as.integer(instid_vec)
	
	M = length(levels(pcf_cat_vec))
	N = length(levels(instid_vec))
	n_gamma = length(gamma_vec)
	iters = dim(outcome_mat)[2]
	
	#Build N x M count matirx and fill in 
	#Also count mean and SD
	n_mat = matrix(NA, nrow = N, ncol = M)
	mean_array = var_array = array(0, c(N,M, iters))
	#Arrays are N by M by iters	
	for (i in 1:N){#Loop over institutions
		for (j in 1:M){#Loop over pcf categories
			indices = (pcf_cat_vec_i == j &
					instid_vec_i == i)						
			n = sum(indices)
			n_mat[i, j] = n	
				
			if (n > 0){
				outcome_mat_small = matrix(outcome_mat[indices, ], ncol = iters)
				mean_array[i, j, ] = apply(outcome_mat_small,
									2, mean)
				var_array[i, j, ] = apply(outcome_mat_small,
									2, var)		
			}
		}
	}

	##Compute marginals (N or M by iters)
	n_u_vec = apply(n_mat, 1, sum)
	n_cat_vec = apply(n_mat, 2, sum)
	pcf_ybar_mat = matrix(NA, nrow = M, ncol = iters)
	for (i in 1:iters){
		pcf_ybar_mat[ ,i] = sapply(levels(pcf_cat_vec), function(x) 
			mean(outcome_mat[pcf_cat_vec == x,i]))
	}
	
	#Compute 3d array of lambdas
	lambdaFun = function(i,k,j){
		if (i == k){
			(n_mat[i,j] / n_u_vec[i]) *
				(1 - (n_mat[i,j] / (n_cat_vec[j])))
		} else {
			-(n_mat[i,j] * n_mat[k,j])/(n_u_vec[i] * n_cat_vec[j])
		}
	}

	lambda_array =  array(NA, c(N, N, M) )
	for (i in 1:N){
		for (k in 1:N){
			for (j in 1:M){
				lambda_array[i, k, j] = lambdaFun(i,k,j)
			}
		}
	}
	lambda_array_squared = lambda_array^2

	z_array = se_array =  array(NA, c(N, n_gamma, iters))
	d_mat = o_mat = e_mat = matrix(NA, nrow = N, ncol = iters)
	n_array = outer(n_mat, rep(1, n_gamma)) #Extend n_mat into a 3rd dimention
	for (i in 1:iters){
		#Compute V(D_i) mat (N x )
		var_global = var(outcome_mat[ ,i])
		var_mat =  var_array[ , ,i]
		var_mat[n_mat == 1] = var_global 
		var_array[ , ,i] = var_mat
		
		var_di_mat =  matrix(NA, nrow = N, ncol = n_gamma)
		di_vec = sapply(1:N, function(x)
			sum(lambda_array[x,,] * mean_array[ , ,i], na.rm = TRUE))
		o_vec = sapply(levels(instid_vec), function(x)
			mean(outcome_mat[instid_vec == x,i]))
		e_vec = sapply(1:N, function(x)
			sum(n_mat[x, ] * pcf_ybar_mat[ ,i])) /
			n_u_vec
		d_mat[ ,i] = di_vec; o_mat[ ,i] = o_vec; e_mat[ ,i] = e_vec

		#Compute SE and Z w/o using a loop over gamma
		var_gamma_array = ((outer(var_mat, rep(1, n_gamma)) *
				  outer(matrix(1,nrow = N, ncol = M), 1-gamma_vec )) + outer( matrix(var_global, nrow = N, ncol = M), gamma_vec) ) 
		 var_gamma_array[n_array > 0] = var_gamma_array[n_array > 0] / n_array[n_array > 0]
		# var_di_mat - N x n_gamma
		 var_di_mat = t(sapply(1:N, function(x)
		 apply( outer(lambda_array_squared[x, , ], rep(1, n_gamma)) * var_gamma_array, 3, sum, na.rm = TRUE)
		 ))
		 se_array[ , ,i] = sqrt(var_di_mat)
		 z_array[ , ,i] = matrix(di_vec, nrow = N, ncol = n_gamma, byrow = FALSE) / se_array[ , ,i]
		}
		
	return(
		list( D = d_mat, O = o_mat, E = e_mat, Z = z_array, 
				SE = se_array)	)
}
	
