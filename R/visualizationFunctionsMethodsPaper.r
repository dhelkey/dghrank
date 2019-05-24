##Save functions for creating methods plots



##Compare plot

cPlotM = function(x, y, xlab = 'x', ylab = 'y', lwd = 2, ...){
  #Plot x and y against each other, compare w/ line y=x

  plot(x, y, ...)
  abline(0,1, lwd = lwd, col = 'red', lty = 2)

  mtext(side = 1, line = 2, xlab)
  mtext(side = 2, line = 2, ylab)

  c_val = round(cor(x,y), 2)
  legend('bottomright', c(paste('Correlation: ',c_val)), bty = 'n')
}


#Ranking plot (be able to fix the order of the columns)


qualPlot = function(d_vec, se_vec, order_vec = FALSE, ylab = 'Estimated Quality', main = '', lwd = 3,...){

  stopifnot( length(d_vec) == length(se_vec)) #estimated quality and standard error should be the same length

  #If order_vec isn't specified, sort by effect
  if (is.logical(order_vec) & !order_vec){
    order_vec = order(d_vec)
  }

  #Sort everything
  d_vec = d_vec[order_vec]
  se_vec = se_vec[order_vec]
  x = 1:length(d_vec)

  #Create upper and lower limits
  lower = d_vec - 1.96 * se_vec
  upper = d_vec + 1.96 * se_vec

  #Generate the caterpiller plots
  plot(x, d_vec, ylab = '', xlab = '', axes = FALSE, ylim = c(min(lower), max(upper)))
  box()
  axis(2)
  axis(1)
  mtext(side = 2, line = 2, ylab)
  mtext(side = 1, line = 2, 'Institution')
  title(main)


graphics::segments(x0=x, x1=x,y0 = lower, y1 = upper, lwd = lwd, ...)
lower_indices = upper < 0
upper_indices = lower > 0
graphics::segments(x0=x[lower_indices], x1=x[lower_indices],
                   y0 = lower[lower_indices], y1 = upper[lower_indices],  col = 'red',lwd = lwd* 1.5 , ...)
graphics::segments(x0=x[upper_indices], x1=x[upper_indices],
                   y0 = lower[upper_indices], y1 = upper[upper_indices],  col = 'blue', lwd = lwd * 1.5, ...)
graphics::abline(h = 0, lty = 3)


  #Now plot upper and lower segments...
  return(order_vec)
}
