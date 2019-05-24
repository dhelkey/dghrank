simulateNullInstDG = function(minimal_data, num_cat, num_cont, iters = 100){


	#Parse Data
	dat = parseMinimalData(minimal_data, num_cat, num_cont,
		n_cutoff = 1)
		
		
	r = fitBabyMonitor(minimal_data,
		num_cat, num_cont,
		dat_out = TRUE,
		n_cutoff = 1, iters = 2, burn_in = 2)
		

	#Extract variables
	y = r$dat$y #binary outcome vector
	N = r$dat$N #total # of individuals
	p = r$dat$p #total # of institutions
	inst_mat = r$inst_mat[ ,c('inst', 'n', 'o_mean')]
		
	pcf_vec = as.factor(r$dat$pcf_vec) #factor of pcf categories

	#Calculate success probablity for each pcf category
	pcf_success_rates = as.numeric(sapply(levels(pcf_vec),
		function(level) mean(y[pcf_vec == level])))

	#N x 1 vector of success probablities
	individual_prob_vec = pcf_vec
	levels(individual_prob_vec) = pcf_success_rates
	individual_prob_vec = as.numeric(levels(individual_prob_vec))[individual_prob_vec]
		
	#Initialize storage
	stat_z_mat = effect_mat = effect_se_mat = matrix(NA, nrow = p, ncol = iters)
		
	data_mat = matrix(NA, nrow = N, ncol = iters)
		
	for (i in 1:iters){#Loop over monte carlo iterations
	
		#Simulate data
		data_mat[ ,i] = rbinom(N, 1, individual_prob_vec)
		
		m_temp = minimal_data
		m_temp[ ,1] = data_mat[ ,i]
		
		#Parse data
		dat_temp = parseMinimalData(m_temp, num_cat, num_cont, n_cutoff = 1)
		
		dg = designBased(0.5, dat_temp$y,dat_temp$pcf_vec,
		dat_temp$inst_vec)
		
		
		stat_z_mat[ ,i] =dg$Z
		effect_mat[ ,i] = dg$D
		effect_se_mat[ ,i] = dg$SE
	
	}
	return( list( 
		inst_mat = inst_mat,
		stat_z = stat_z_mat,
		effect = effect_mat,
		effect_se = effect_se_mat,
		data_mat = data_mat
		))
}