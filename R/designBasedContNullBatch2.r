designBasedContNullBatch2 = function(gamma_vec, outcome_mat, pcf_cat_vec, instid_vec){
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



	###First, turn everything into a factor
	# create lambda matrix
	#sparse indicators for membership``
	#loop over iters
	#	use aggregate to compute mean and sd
	# will have to insert into a sparse matrix after computing w/ aggregate...shouldnt be too hard
	#  	sparse linear algebra for to transformations, try to avoid using too  much storage space


	#Helper Function
	insertAggregate = function(Mat, agg){
  #We do this a few times
  #Aggregated on 2 dimensions, then insert into a sparse matrix
  Mat[ cbind( as.numeric(agg[ ,1]),
              as.numeric(agg[ ,2]))] = as.numeric(agg[ ,3])
	return(Mat)
	}

	#Extract variables
pcf_cat_vec = as.factor(pcf_cat_vec)
pcf_cat_vec_i = as.integer(pcf_cat_vec)
instid_vec = as.factor(instid_vec)
instid_vec_i = as.integer(instid_vec)

#Summary statistics
n_gamma = length(gamma_vec)
n_replicates = dim(outcome_mat)[2]
n_total = length(pcf_cat_vec)
iters = dim(outcome_mat)[2]
N = length(levels(instid_vec))
M = length(levels(pcf_cat_vec))
insts = levels(instid_vec)
cats = levels(pcf_cat_vec)


#Count number in each category (lower half of HEFCE grid (table 2))
n_counts = aggregate(rep(1, n_total) ~
              instid_vec * pcf_cat_vec, FUN = sum)
n_mat = matrix(0, nrow = N, ncol = M)
n_mat = insertAggregate(n_mat, n_counts)

	#Marginal counts
	n_u_vec = apply(n_mat, 1, sum)
	n_cat_vec = apply(n_mat, 2, sum)

	#This takes a while, but only needs to be done once for a batch of null simulations
    #Compute 3d array of lambdas
	#THIS PART TAKES FOREVER
	lambdaFun = function(i,k,j){
		if (i == k){
			(n_mat[i,j] / n_u_vec[i]) *
				(1 - (n_mat[i,j] / (n_cat_vec[j])))
		} else {
			-(n_mat[i,j] * n_mat[k,j])/(n_u_vec[i] * n_cat_vec[j])
		}
	}


	#Generating the array like this is a little faster
	#Tried using foreach, wasn't faster. I think this is memory access, rather than compute time limited
	lambdaFun = Vectorize(lambdaFun, vectorize.args = c('i','k'))
	print('computing lambda')
	lambda_array = array(NA, c(N, N, M) )
	#lambda_array = array(rnorm(N*N*M), c(N, N, M) )
	 for (j in 1:M){
		 lambda_array[ , ,j] = outer(1:N, 1:N, lambdaFun, j = j)
	 }
	lambda_array_squared = lambda_array^2
	 print('done computing lambda')


	d_mat = matrix(NA, nrow = N, ncol = n_replicates)
	z_array = se_array = array(NA, c(N, n_gamma, iters))
	#cl = makeCluster(2)
	#registerDoParallel(cl)
	n_array = outer(n_mat, rep(1, n_gamma))
	#DO par doesnt speed it up that much due to memory constraints :(
	for (i in 1:n_replicates){#Loop over minimal_data iterations #Turn this parallel?
		outcome_vec = outcome_mat[ ,i]
		var_global = var(outcome_vec)
		#Compute mean and variance matrix
		#N x M
		mean_mat = var_mat = matrix(NA, nrow = N, ncol = M)
		aggregated_mean = aggregate(outcome_vec ~
				instid_vec * pcf_cat_vec, FUN = mean)
		mean_mat = insertAggregate(mean_mat, aggregated_mean)

		aggregated_var = aggregate(outcome_vec ~
				instid_vec * pcf_cat_vec, FUN = var)
		var_mat = insertAggregate(var_mat, aggregated_var)
		var_mat[n_mat==1] = var_global

		#Compute D vec using linear algebra
		d_mat[ ,i] = apply( lambda_array * outer(rep(1,N), mean_mat), 1, sum, na.rm = TRUE)

		varArray = function(gamma){
			#Creates array of dimention N x N x M to compute SE vec using same linear algebra as above
			v_temp = matrix(var_global, nrow = N, ncol = M) * gamma + (1-gamma) * var_mat
			v_temp[n_mat > 0] = v_temp[n_mat > 0] / n_mat[n_mat > 0]
			return( outer(rep(1,N), v_temp))
		}

		##Do it w/ linear algebra instead
		#Matrix algebra to compute the gamma matrix
		#And then matrix algebrea inside foreach (this is instead of calling a function w/ indexing)
		#Foreach doesnt save much time...
		# var_gamma_array = ((outer(var_mat, rep(1, n_gamma)) *
				  # outer(matrix(1,nrow = N, ncol = M), 1-gamma_vec )) + outer( matrix(var_global, nrow = N, ncol = M), gamma_vec) )
		# var_gamma_array[n_array > 0] = var_gamma_array[n_array > 0] / n_array[n_array > 0]
		# a = foreach(j = 1:n_gamma,.combine ='cbind') %dopar% apply(lambda_array_squared * outer( rep(1, N), var_gamma_array[, ,j]), 1, sum, na.rm = TRUE)
		#n_gamma X N x N x M
		a = foreach(j=1:n_gamma, .combine = 'cbind') %do%
				apply( lambda_array_squared * varArray(gamma_vec[j]), 1, sum, na.rm = TRUE)

		se_array[ , ,i] = sqrt(a)
		z_array[ , ,i] = matrix( d_mat[ ,i], nrow = N, ncol = n_gamma, byrow = FALSE) / se_array[ , ,i]
		if ((i %% 10) == 0){print(i)}
	}
		#stopCluster(cl)
	return( #We don't compute O and E for time reasons, use designBasedCont
	list(D = d_mat, Z = z_array, SE = se_array))
}


