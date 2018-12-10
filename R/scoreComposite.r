#' Compute composite Baby-MONITOR scores
#'
#' \code{scoreComposite} generates composite Baby-MONITOR scores  after analysis using fitBabyMonitor() on multiple performance indicators. 
#'
#' @param returner_list: List of outputs from fitBabyMonitor(), one element for each performance indicator.
#'
#' @inheritParams fitBabyMonitor
scoreComposite = function(returner_list, alpha = 0.05, bonferroni = TRUE){

	#Different types of composite scores available
  type_vec = c('inst', 'subset_baseline', 'subset_nobaseline', 'inst_subset_baseline' , 'inst_subset_nobaseline')

  #Check if all are subset
  subset = all( sapply(returner_list, function(x) x$dat$subset))
  
  
  returner_out = list()
	for (type in type_vec){
	 if (type == 'inst'){
		id_cols = 'inst'
		mat = 'inst_mat'
	  }	 else if (type == 'subset_baseline'){
		id_cols = 'subset'
		mat = 'subset_mat_baseline'
	  } else if (type == 'subset_nobaseline'){
		id_cols = 'subset'
		mat = 'subset_mat_nobaseline'
	  } else if (type == 'inst_subset_baseline'){
		id_cols = c('inst', 'subset')
		mat = 'inst_subset_mat_baseline'
	  } else if (type == 'inst_subset_nobaseline'){
		id_cols = c('inst', 'subset')
		mat = 'inst_subset_mat_nobaseline'
	  }

	  if (!subset & type != 'inst'){
		#Break loop if we don't have subset data
		returner_out[[mat]] = data.frame()
		next
	  }
	  
	  n_scores = length(returner_list)
	  n_id_cols = length(id_cols)

	  parsed_list = vector('list',  length = n_scores)
	  full_id_mat = matrix(NA, nrow = 0, ncol = n_id_cols)
	 
	  for (i in 1:n_scores){ #Iterate through the returners and extract variables
		r = returner_list[[i]]
		parsed_temp = list()
		parsed_temp$id_mat = as.matrix(r[[mat]][ ,id_cols])
		full_id_mat = rbind(full_id_mat, parsed_temp$id_mat)
		parsed_temp$score_est = r[[mat]]$score_est
		parsed_temp$score_se = r[[mat]]$score_se
		parsed_list[[i]] = parsed_temp
			  }

	  #Remove duplicate ids and sort
		full_id_mat = apply(unique(
			data.frame(full_id_mat, row.names = 1:dim(full_id_mat)[1])),2,as.numeric)
	  if (n_id_cols==1){ #Sort by one category
		full_id_mat = as.matrix(full_id_mat[order(full_id_mat[ ,1]) , ])
	  } else{ #Sort by two categories
		full_id_mat = full_id_mat[order(full_id_mat[ ,1],full_id_mat[ ,2]), ]
	  }

	  idStr = function(x){ #Make sure id (potentially two columns) is a vector
		as.character(paste(x, collapse = '-'))
	  }
	  full_id_vec = apply(full_id_mat, 1, idStr)
	  P = length(full_id_vec) 
	  
	  #Put data in a matrix for easy score computation
	  score_est_mat = score_se_mat = score_present_mat = matrix(NA, nrow = P, ncol = n_scores)
	  for (i in 1:n_scores) {
		r = parsed_list[[i]]
		id_str = apply(r$id_mat, 1, idStr)
		indices = match(id_str, full_id_vec)
		score_est_mat[indices, i] = r$score_est
		score_se_mat[indices, i] = r$score_se
		
		score_present_mat[indices, i] = 1
	  }

	  #Create variance inflated score by multiplying the average by sqrt(n_scores)
	  n_components = apply(score_present_mat, 1, sum, na.rm = TRUE)
	  score_est = apply(score_est_mat, 1, sum, na.rm = TRUE) / sqrt(n_components)
	  score_se = apply(score_se_mat, 1,function(x) sqrt(sum(x^2, na.rm =TRUE))) / sqrt(n_components)

		#Compute upper and lower from est and SE
		z_star =  computeZstar(alpha, P, bonferroni)
		score_lower = score_est - z_star * score_se
		score_upper = score_est + z_star * score_se

	  
	  #Spit out data
	  out_mat = data.frame(cbind(full_id_mat, score_est, score_se, score_lower, score_upper, n_components))
	  names(out_mat)[1:n_id_cols] = id_cols
	  returner_out[[mat]] = out_mat
	}
	return(returner_out)
}
