
  Q <- quantile(xaxis, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(xaxis, na.rm = TRUE)
  ind_valid = ind_valid & xaxis > (Q[1] - 1.5*iqr) & xaxis < (Q[2]+1.5*iqr)