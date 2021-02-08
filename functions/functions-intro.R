library(plotrix)
library(pracma)
is.wholenumber = function(x, tol = .Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol
}

handShake = function(n=1, plotMe =FALSE)
{
  if(n < 1) {stop("n cannot be less than 1");}
  if(!is.wholenumber(n)) {stop("not an integer");}
  h = n*(n-1)/2;
  if(plotMe) 
  {
    # plots the circle
    r = n
    plot(0,0,xlim=c(-n-1,n+1), ylim=c(-n-1,n+1), main=paste0(h," handshakes for ",n," people"), xlab="", ylab="", xaxt="n", yaxt="n",bty="n", asp=1);
    draw.circle(0,0,n,n+100);
    
    # plots the points
    for(i in 1:n){
      deg = 360/n;
      theta = 2*pi*deg/360;
      x = r*sin(i*theta);
      y = r*cos(i*theta);
      points(x, y, pch=16, type="o")
      # plot the line handshakes
      for(s in 1:n){
        for(t in 1:n){
          xnext = r*sin((s+t)*theta);
          ynext = r*cos((s+t)*theta);
          segments(x,y,xnext[t],ynext[t])
        }
      }
    }
  }
  h;
}


# declaration function
countLetterInString = function(str, letter)
{
  nchar(as.character(str)) -nchar( gsub(letter, "", str, fixed=TRUE))
}
AlphabetCounter = function(str)
{
  str = gsub("[[:space:]]", "", str)
  str = tolower(str)
  df = data.frame(matrix(0, nrow=1, ncol=27, byrow=TRUE))
  colnames(df) = c(letters,"OTHER")
  for(letter in letters)
  {
    idx = which(letters == letter)
    df[1,idx] = countLetterInString(str,letter)
    str = gsub(letter,"",str,fixed=TRUE)
  }
  
  df[1,27] = nchar(str)
  df;
}

ComputeDeterminate3 = function(myMatrix)
{
  numrow=nrow(myMatrix);
  numcol=ncol(myMatrix);
  if (numrow != numcol)
  {stop("The matrix must be square to compute the determinant");}
  if (numrow != 3)
  {stop("The matrix must be 3 by 3");}
  a=myMatrix[1,1];
  b=myMatrix[1,2];
  c=myMatrix[1,3];
  d=myMatrix[2,1];
  e=myMatrix[2,2];
  f=myMatrix[2,3];
  g=myMatrix[3,1];
  h=myMatrix[3,2];
  i=myMatrix[3,3];
  
  (a*(e*i-f*h))-(b*(d*i-f*g))+(c*(d*h-e*g))
}

