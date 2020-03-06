#' xbarthetadist function
#'
#' This is a constuctor function that is used to create thetaeigen and thetarot from sample variance covariance matrix
#'
#' @param n
#' @param iter
#' @param mu
#' @param sigma
#'
#' @return a matrix composed of iterated results on four categories including xbar1, xbar2, thetae, and thetar
#' @export
#'
#' @examples  mu1=c(0,0); sigma1 = matrix(c(100,40,40,400) nr=2,nc=2,byrow=FALSE); xbarthetadist(n=30,iter=1000,mu=mu1,sigma=sigma1)
xbarthetadist = function(n,iter,mu,sigma){
  library(mvtnorm)
  library(ggplot2)
  mat = matrix(NA, nr= iter, nc=4)
  colnames(mat)= c("xbar1","xbar2","thetae", "thetar")
  for(i in 1:iter){
    x = rmvnorm(n,mu,sigma)
    mat[i,c(1,2)] <- colMeans(x)
    s=(n-1)/n*cov(x)
    eig=eigen(s)
    thetaeigen = acos(eig$vectors[,1][1])
    thetarot = 1/2*atan(2*s[1,2]/(s[1,1]-s[2,2])) +pi/2

    mat[i,3]<-thetaeigen
    mat[i,4]<-thetarot
  }

  df=as.data.frame(mat)
  g=ggplot(df, aes(x=xbar1,y=xbar2))  + coord_equal()
  ae = ggplot(df, aes(x=thetae))
  ar = ggplot(df, aes(x=thetar))

  gp = g + geom_point()
  print(gp)
  gd = g + stat_density_2d()
  print(gd)

  ahe = ae + geom_histogram()
  ade = ae + geom_density()
  ahr = ar + geom_histogram()
  adr = ar + geom_density()


  print(ahe)
  print(ade)
  print(ahr)
  print(adr)
  head(mat)
  list(mat = mat)
}
