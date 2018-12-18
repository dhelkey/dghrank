  toScore = function(summary_dat, z_star, score_type = 'effect_scaled'){ #effect_scaled, stat_z, stat_z_scaled
  #This is just a linear transform, mean and variance transform as well
  #Requires the effect be computed with an estimate and an SE
  if(is.null(summary_dat)){return(NULL)}

	#Return the 3 different scores
	sd_effect = sd(summary_dat$effect_est)
	est_effect_scaled = summary_dat$effect_est / sd_effect
	s_effect_scaled = summary_dat$effect_se / sd_effect
	
	est_stat_z = summary_dat$effect_est / summary_dat$effect_se
	s_stat_z = summary_dat$effect_se/ summary_dat$effect_se
	
	sd_z = sd(summary_dat$stat_z)
	est_stat_z_scaled = summary_dat$effect_est / (sd_z * summary_dat$effect_se)
	s_stat_z_scaled = summary_dat$effect_se/ (sd_z * summary_dat$effect_se)
	
	est = switch(score_type,
	'effect_scaled'= est_effect_scaled,
	'stat_z'= est_stat_z,
	'stat_z_scaled'= est_stat_z_scaled
	)
	
	s = switch(score_type,
	'effect_scaled'= s_effect_scaled,
	'stat_z'= s_stat_z,
	'stat_z_scaled'= s_stat_z_scaled
	)
	
	return(
		data.frame( score_est = est, 
	score_lower = est - z_star * s, score_upper = est + z_star * s,
	score_se = s,
	est_effect_scaled = est_effect_scaled, s_effect_scaled = s_effect_scaled,
	est_stat_z = est_stat_z, s_stat_z = s_stat_z, 
	est_stat_z_scaled = est_stat_z_scaled, s_stat_z_scaled = s_stat_z_scaled)
	)		
  }