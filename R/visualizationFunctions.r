##Construct the posterior line plots
plotRankingLines = function(est, lower, upper, name_vec = NULL,  order = NULL, add = FALSE,...){
  #Plots (or just calls segments() to add to plot) effects or scores
  #This function should return the order
  if (is.null(order)){order = order(est)}
  #Sort them now so we don't have do do it again
  est = est[order]; name_vec = name_vec[order]
  lower = lower[order]; upper = upper[order]
  if (!add){#If add=FALSE, make a new plot
	graphics::plot(est, ylim = c(min(lower), max(upper)))
	}
  p = length(est)
  x = 1:p

  graphics::segments(x0=x, x1=x,y0 = lower, y1 = upper, ...)
  lower_indices = upper < 0
  upper_indices = lower > 0
  graphics::segments(x0=x[lower_indices], x1=x[lower_indices],
	y0 = lower[lower_indices], y1 = upper[lower_indices], lwd = 3, col = 'red')
	graphics::segments(x0=x[upper_indices], x1=x[upper_indices],
	y0 = lower[upper_indices], y1 = upper[upper_indices], lwd = 3, col = 'blue')
  graphics::abline(h = 0, lty = 3)
  return(list(order = order))
}


histSum = function(x, ...){
	hist(x,   ...)
	text(mean(x), 0, paste0('Mean: ', round(mean(x),3)), pos = 3, offset = 1)
	text(mean(x), 0, paste0('SD: ', round(sd(x),3)), pos = 3, offset = 2)
}