# The following function describes the variance of the random variable resulting from the 
# multiplication of dependent random variables (not neccessarily normally distributed)
# x1: mean m1 and variance v1
# x2: mean m2 and variance v2
# covariance(x1,x2) = c1
# covariance(x1^2, x2^2) = c2
depVar <- function(m1,v1,m2,v2,c1=0,c2=0) {
   ret <- ((c2) + (v1 + m1^2) * (v2 + m2^2) - (c1 + m1*m2)^2) / m1 / m1 / m2 / m2;
   print(ret);
}
