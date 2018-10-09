##Construct the posterior line plots
plotRankingLines = function(est, lower, upper, name_vec = NULL,  order = NULL, add = FALSE,...){
  #Plots (or just calls segments() to add to plot) effects or scores
  #This function should return the order
  if (is.null(order)){order = order(est)}
  #Sort them now so we don't have do do it again
  est = est[order]; name_vec = name_vec[order]
  lower = lower[order]; upper = upper[order]
  if (!add){#If add=FALSE, make a new plot
	plot(est, ylim = c(min(lower), max(upper)))
	}
  p = length(est)
  x = 1:p

  segments(x0=x, x1=x,y0 = lower, y1 = upper, ...)
  abline(h = 0, lty = 3)
  return(list(order = order))
}