scoreComposite = function(returner_list, alpha = 0.05, type = 'inst', method = 'scaled'){ 
  #mode: 'inst'/'subset_nobaseline'
  # 'inst_subset_nobaseline', #inst_subset_baseline  #These ones have 2 columns
  ##Obtain the matrices and columns of interest
  #TODO alpha doesnt do anything yet
  #TODO TEMP alpha just equals avg/scaled
  
  
  ###Output columns - score_est, score_lower, score_upper, score_s, n
  if (type == 'inst'){
	id_cols = 'inst'
	mat = 'inst_mat'
  }	else if (type == 'z_standardized'){
	id_cols = 'inst'
	mat = inst_mat
  } else if (type == 'subset_baseline'){
	id_cols = 'subset'
	mat = 'full_subset_mat_baseline'
  } else if (type == 'subset_nobaseline'){
	id_cols = 'subset'
	mat = 'full_subset_mat_nobaseline'
  } else if (type == 'inst_subset_baseline'){
	id_cols = c('inst', 'subset')
	mat = 'subset_baseline_mat'
  } else if (type == 'inst_subset_nobaseline'){
	id_cols = c('inst', 'subset')
	mat = 'subset_nobaseline_mat'
  }

	#First, scan through the list and build the large matrix.
	full_id_cols = NULL
	for (r in returner_list){
		col_data = as.matrix(r[[mat]][ ,id_cols], ncol = length(id_cols))
		full_id_cols = unique(rbind(full_id_cols, col_data))
	}
	id_vec = apply(full_id_cols, 1, paste, collapse = '-')
	p_total = length(full_id_cols)
	#Set up storage
	score_est_mat = score_se_mat = matrix(NA, nrow = p_total,
											ncol = length(returner_list))
	z_mat=matrix(NA, nrow = p_total, ncol = length(returner_list))
	#Iterate through fits and associate scores and ests w/ institutions
	for (i in 1:length(returner_list)){
		r = returner_list[[i]]
		col_data = as.matrix(r[[mat]][ ,id_cols], ncol = length(id_cols))
		id_temp = apply(col_data, 1, paste, collapse = '-')
		indices = id_vec %in% id_temp
		
		score_est_mat[indices,i] = r[[mat]]$score_est
		score_se_mat[indices,i] = r[[mat]]$score_se
		z_mat[indices, i] = r[[mat]]$stat_z
	}
	
	#Compute resulting
	num_components_vec = apply(score_est_mat, 1, function(x) sum(!is.na(x)))
	
	if (method == 'scaled'){
		score_est = apply(score_est_mat, 1, sum, na.rm = TRUE) / sqrt(num_components_vec)
		se_vec = apply(score_se_mat, 1, function(x) sqrt(sum(x^2, na.rm = TRUE)))  /
												sqrt(num_components_vec)
	} else if (method == 'avg'){
				score_est = apply(score_est_mat, 1, sum, na.rm = TRUE) / num_components_vec
		se_vec = apply(score_se_mat, 1, function(x) sqrt(sum(x^2, na.rm = TRUE)))  /
												num_components_vec
	}
	
	

	z_star = qnorm( 1 - alpha/p_total)

	z_est = apply(z_mat, 1, function(x) sum(x, na.rm = TRUE)) / num_components_vec
											
	#Output
	out_mat = data.frame(n = num_components_vec,
		score_est = score_est, 
		score_lower = score_est - z_star * se_vec,
		score_upper = score_est + z_star * se_vec,
		score_se = se_vec,
		z_score = z_est)
		return(out_mat)
	#TODO MAKE SURE IT WORKS WITH EDGE CASES
	

	return(out_mat)
	
	print(head(col_data))
	id_vec = apply(full_id_cols, 1, paste, collapse = '-')
	out_mat = data.frame(full_id_cols, score_est = NA, score_s = 0, n = 0)
	
	#Now scan through the list again and update means and SDs.
	 for (r in returner_list){
		col_data = as.matrix(r[[mat]][ ,id_cols], ncol = length(id_cols))
		id_temp = apply(col_data, 1, paste, collapse = '-')
		indices = id_vec %in% id_temp

		out_mat$score_est[indices] = out_mat$score_est[indices] + r[[mat]]$score_est
		#apply(
		#	cbind(out_mat$score_est[indices], r[[mat]]$score_est), 
		#		1, sum, na.rm = TRUE)
		out_mat$score_s[indices] = sqrt(out_mat$score_s[indices]^2 
			+ r[[mat]]$score_s^2) #for numerical reasons, ideally this would be done outside the loop just once
		out_mat$n[indices] = out_mat$n[indices] + 1
	 }
	#Compute composite mean and SD
	out_mat$score_est = out_mat$score_est / out_mat$n # / sqrt(out_mat$n)
	out_mat$score_s = out_mat$score_s / sqrt(out_mat$n)#/ sqrt(out_mat$n)
	
	names(out_mat)[1:length(id_cols)] = id_cols
	z_star = qnorm(1 - alpha/length(id_vec))
	#COmpute upper and lower
	out_mat$score_lower = out_mat$score_est - z_star * out_mat$score_s
	out_mat$score_upper = out_mat$score_est + z_star * out_mat$score_s
  return(out_mat[ ,c(id_cols, 'score_est', 'score_lower',
				'score_upper', 'n')])
  }