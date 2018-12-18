visualizeReturner = function(returner, mat = 'inst_mat',
                  type = 'score', only_significant = FALSE,
                  plot_order = 'effect', xlab = '', ylab = '',lwd = 1, ...){ #given/ranked, some other other order, 'sorted'

 #Extract id colums for each option
  if (mat == 'inst_mat'){
    id_cols = 'inst'
  }	 else if (mat == 'subset_mat_baseline'){
    id_cols = 'subset'
  } else if (mat == 'subset_mat_nobaseline'){
    id_cols = 'subset'
  } else if ( mat == 'inst_subset_mat_baseline'){
    id_cols = c('inst', 'subset')
  } else if (mat == 'inst_subset_mat_nobaseline'){
    id_cols = c('inst', 'subset')
  }
  if (type == 'score'){
    cols = c('score_est', 'score_lower', 'score_upper')
  } else if (type == 'effect'){
    cols = c('effect_est', 'effect_lower', 'effect_upper')
  }

  mat_use = returner[[mat]]
  id_vec = mat_use[ ,id_cols]
  n = dim(mat_use)[1]
  if (length(id_cols) == 2){
    id_vec = apply(id_vec, 1, idStr)
  }

  #ID vars
  plot_vars = mat_use[ ,cols] #Subse 

  #First order, then subset 
  if (plot_order == 'ranked' ){
    order_vec = order(plot_vars[ ,1])
  } else if (plot_order == 'source'){
	order_vec = 1:n
  } else if (plot_order == 'effect'){
	order_vec = order(mat_use$effect_est)
  }
  
  indices = is.finite(plot_vars[ ,1])
  if (only_significant){
	indices = plot_vars[ ,2] > 0 | plot_vars[ ,3] < 0
  }
    #Extract the variables we want at the end
	plot_vars = plot_vars[order_vec, ]
	plot_names = id_vec[order_vec]
	indices = indices[order_vec]
	plot_vars = plot_vars[indices, ]
	plot_names = plot_names[indices]
  center = plot_vars[ ,1]
  lower = plot_vars[ ,2]
  upper = plot_vars[ ,3]
  p = length(center)
  x = 1:p

  graphics::plot(center, ylim = c(min(lower), max(upper)),
  xlab = xlab, ylab = '', axes = FALSE)
  box()
  axis(2)
  axis(1, at = x, labels=plot_names, las=2, cex.axis = 0.65)
  print(plot_names)
  mtext(side = 2, line = 2, ylab)
  graphics::segments(x0=x, x1=x,y0 = lower, y1 = upper, lwd = lwd, ...)
  lower_indices = upper < 0
  upper_indices = lower > 0
  graphics::segments(x0=x[lower_indices], x1=x[lower_indices],
                     y0 = lower[lower_indices], y1 = upper[lower_indices],  col = 'red',lwd = lwd* 1.5 , ...)
  graphics::segments(x0=x[upper_indices], x1=x[upper_indices],
                     y0 = lower[upper_indices], y1 = upper[upper_indices],  col = 'blue', lwd = lwd * 1.5, ...)
  graphics::abline(h = 0, lty = 3)
  
  #Order and subset? if necesasary
  #Here is where we would extrreact only_significatnt
  return(0)
}



##Construct the posterior line plots
# plotRankingLines = function(est, lower, upper, name_vec = NULL,  order = NULL, add = FALSE,...){
  # #Plots (or just calls segments() to add to plot) effects or scores
  # #This function should return the order
  # if (is.null(order)){order = order(est)}
  # #Sort them now so we don't have do do it again
  # est = est[order]; name_vec = name_vec[order]
  # lower = lower[order]; upper = upper[order]
  # if (!add){#If add=FALSE, make a new plot
	# graphics::plot(est, ylim = c(min(lower), max(upper)))
	# }
  # p = length(est)
  # x = 1:p

  # graphics::segments(x0=x, x1=x,y0 = lower, y1 = upper, ...)
  # lower_indices = upper < 0
  # upper_indices = lower > 0
  # graphics::segments(x0=x[lower_indices], x1=x[lower_indices],
	# y0 = lower[lower_indices], y1 = upper[lower_indices], lwd = 3, col = 'red')
	# graphics::segments(x0=x[upper_indices], x1=x[upper_indices],
	# y0 = lower[upper_indices], y1 = upper[upper_indices], lwd = 3, col = 'blue')
  # graphics::abline(h = 0, lty = 3)
  # return(list(order = order))
# }


histSum = function(x, ...){
	hist(x, lwd = 2,  ...)
	text(mean(x), 0, paste0('Mean: ', round(mean(x),3)), pos = 3, offset = 2)
	text(mean(x), 0, paste0('SD: ', round(sd(x),3)), pos = 3, offset = 1)
}