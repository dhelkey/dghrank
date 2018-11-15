scoreComposite = function(returner_list, alpha = 0.05, type = 'inst'){
  #mode: 'inst'/'subset_nobaseline'
  # 'inst_subset_nobaseline', #inst_subset_baseline  #These ones have 2 columns
  ##Obtain the matrices and columns of interest
  
  
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
		full_id_cols = unique(rbind(full_id_cols, col_data  ))
	}
	
	id_vec = apply(full_id_cols, 1, paste, collapse = '-')
	out_mat = data.frame(full_id_cols, score_est = NA, score_s = 0, n = 0)
	
	#Now scan through the list again and update means and SDs.
	 for (r in returner_list){
		col_data = as.matrix(r[[mat]][ ,id_cols], ncol = length(id_cols))
		id_temp = apply(col_data, 1, paste, collapse = '-')
		indices = id_vec %in% id_temp
		out_mat$score_est[indices] = apply(
			cbind(out_mat$score_est[indices], r[[mat]]$score_est), 
				1, sum, na.rm = TRUE)
		out_mat$score_s[indices] = sqrt(out_mat$score_s[indices]^2 
			+ r[[mat]]$score_s^2) #for numerical reasons, ideally this would be done outside the loop just once
		out_mat$n[indices] = out_mat$n[indices] + 1
	 }
	#Compute composite mean and SD
	out_mat$score_est = out_mat$score_est / sqrt(out_mat$n)
	out_mat$score_s = out_mat$score_s / sqrt(out_mat$n)#/ sqrt(out_mat$n)
	
	names(out_mat)[1:length(id_cols)] = id_cols
	z_star = qnorm(1 - alpha/length(id_vec))
	#COmpute upper and lower
	out_mat$score_lower = out_mat$score_est - z_star * out_mat$score_s
	out_mat$score_upper = out_mat$score_est + z_star * out_mat$score_s
  return(out_mat[ ,c(id_cols, 'score_est', 'score_lower',
				'score_upper', 'n')])
  }