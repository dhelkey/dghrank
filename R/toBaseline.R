toBaseline = function(data_frame, inst = FALSE){
	return_cols = c('subset','effect_est', 'effect_lower', 'effect_upper', 'effect_se', 'score_est', 'score_lower', 'score_upper', 'score_se')
	if (inst){return_cols = c('inst', return_cols)}

	#Identify if the first subset is present
	base_index = which(data_frame$subset == 1)
	if (length(base_index) == 0){ #First subset not present
		df = data.frame(matrix(0, nrow = 0, ncol = length(return_cols)))
		names(df) = return_cols
		return(df)
	}
	#If it is present, then change the relevent columns
	sub_effect = data_frame$effect_est[base_index]
	sub_score = data_frame$score_est[base_index]
	
	data_frame$effect_est = data_frame$effect_est - sub_effect
	data_frame$effect_lower = data_frame$effect_lower - sub_effect
	data_frame$effect_upper = data_frame$effect_upper - sub_effect
	
	data_frame$score_est = data_frame$score_est - sub_score
	data_frame$score_lower = data_frame$score_lower - sub_score
	data_frame$score_lower = data_frame$score_lower - sub_score
	
	data_frame$effect_lower[base_index] = data_frame$effect_upper[base_index] = 
	data_frame$score_lower[base_index] = data_frame$score_upper[base_index] = 
	data_frame$score_se[base_index] = data_frame$effect_se[base_index] = 0

	return(data_frame[ ,return_cols])
}
  