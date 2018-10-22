    #Simulate Observations in this PCF category
    simulateObs = function(desired_mean, desired_sd, n, I, emperical_dist = FALSE){
        #Simulate outcomes from normal distribution (if emperical_dist == FALSE) 
        # or from a transformation of the vector given by emperical_dist (if given)
        #desired_mean - desired output mean
        # desired_sd - desired output_sd
        #n - number of individuals
        #I - number of MC iterations
        #emeperical_dist - FALSE, or a vector of observations
        if (!emperical_dist){
            matrix( rnorm(n * I, desired_mean, desired_sd),
                      nrow = n, ncol = I)
        } 
    }
