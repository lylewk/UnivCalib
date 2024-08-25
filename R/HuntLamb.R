#' @export
HuntLamb<-function (bivariate_stats='biv_stats',y.vec='y',n.dec=3,area=0.95)
{
  dat=get(bivariate_stats)
  N=dat$N[1]
  mu.x=dat$mu[1]
  mu.y=dat$mu[2]
  V=dat$V
  v.x=V[1,1]
  v.y=V[2,2]
  cov_xy=V[1,2]
  y=get(y.vec)
  B_y.x=cov_xy/v.x
  r=cov_xy/sqrt(v.x*v.y)
  see_sq=(1-r^2)*v.y*(N-1)/(N-2)
  s2B=B_y.x^2
  t.scale=sqrt((N+1)*see_sq/(N*s2B))
  Bc=v.x/cov_xy
  ac=mu.x-Bc*mu.y
  plus.minus=qt_scaled(sd=t.scale,df=N-2,p=area)

  n.cases=length(y)
  if(n.cases==1){
  est=Bc*y+ac
  L=est-plus.minus
  U=est+plus.minus
  results=round(c(L,est,U),n.dec)
  names(results)=c('lo','est','hi')
  return(results)
  }
  else{
  est=vector()
  L=vector()
  U=vector()
  for(i in 1:n.cases){
    est[i]=Bc*y[i]+ac
    L[i]=round(est[i]-plus.minus,n.dec)
    U[i]=round(est[i]+plus.minus,n.dec)
    est[i]=round(est[i],n.dec)
  }
  results=data.frame(L,est,U)
  colnames(results)=c('lo','est','hi')
  }
  return(results)
}
