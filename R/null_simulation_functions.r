   #Simulate Observations in this PCF category
    simulateObs = function(desired_mean, desired_sd, n, I, emperical_dist = FALSE){
        #Simulate outcomes from normal distribution (if emperical_dist == FALSE) 
        # or from a transformation of the vector given by emperical_dist (if given)
        #desired_mean - desired output mean
        # desired_sd - desired output_sd
        #n - number of individuals
        #I - number of MC iterations
        #emeperical_dist - FALSE, or a vector of observations
        if (!is.numeric(emperical_dist)){
            matrix( rnorm(n * I, desired_mean, desired_sd),
                      nrow = n, ncol = I)
        } else{ #if emperical_dist == TRUE, standardize the observations
			stopifnot(is.numeric(emperical_dist))
			emperical_standardized = scale(emperical_dist)
			sampled_standardized = sample(emperical_standardized, n * I, replace = TRUE)
			matrix((sampled_standardized * desired_sd) + desired_mean, nrow = n, ncol = I)
		}
    }

	
generateNullData = function(outcome_vec, pcf_vec, iters = 10,
	emperical_dist = FALSE){
  #Generate Null data for a continious outcome vec
  #COULD generate in parallel, but takes so little time it doesn't seem worth it
  #Only worth if we want to avoid storing the minimal data!
  #TODO - this is where we will change the function to simulate from the
  # emperical distribution

  stopifnot(length(outcome_vec) == length(pcf_vec))

  #Extract variables
  pcf_vec = as.factor(pcf_vec)
  sd_grand = sd(outcome_vec)
  N = length(outcome_vec)

  #Set up storage
  n_vec = mean_vec = sd_vec = NULL
  simulated_mat = matrix(NA, nrow = N, ncol = iters)

  for (cat in levels(pcf_vec)){#Loop over PCF categories
    indices = pcf_vec == cat

    #COmpute summary statistics
    m = mean(outcome_vec[indices])
    s = sd(outcome_vec[indices])
    n = sum(indices)
    if (is.na(s)){s = sd_grand}

    #Simulate observations
    data_temp = simulateObs(m, s, n, iters, emperical_dist)

    #Append summary statistics and data
    n_vec = c(n_vec, n)
    mean_vec = c(mean_vec, m)
    sd_vec = c(sd_vec, s)

    simulated_mat[indices, ] = data_temp
  }
  return( list(
    data = simulated_mat,
    n = n_vec,
    m = mean_vec,
    s = sd_vec
  ))
}

##Test computeNullBlock (TODO test against designBasedCont)
computeNullBlock = function(simulated_data, pcf_vec, inst_vec, gamma_vec){
  #simulated_data <N x iters>
  #inst_vec <N x 1>: p institutions
  #pcf_vec <N x 1>

  iters = dim(simulated_data)[2]

  out_array = NULL
  for (i in 1:iters){
    out_array = abind(out_array,
                      designBasedContNull(gamma_vec, simulated_data[ ,i], pcf_vec, inst_vec),
                      along = 3)
  }
  return(out_array)
  #Returns array <P x n_gamma x iters)
}