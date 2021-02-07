
is.wholenumber = function(x, tol = .Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol
}

handShake = function(n=1, plotMe =FALSE)
{
  if(n < 1) {stop("n cannot be less than 1");}
  if(!is.wholenumber(n)) {stop("not an integer");}
  h = n*(n-1)/2;
  
}


