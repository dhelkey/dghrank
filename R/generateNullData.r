generateNullData = function(outcome_vec, pcf_vec, iters = 10){
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
    data_temp = matrix(rnorm(n * iters, mean = m, sd = s),
                       nrow = n)

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