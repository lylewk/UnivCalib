#' @export
exact<-function (bivariate_stats='biv_stats',y.vec='y',n.dec=3,area=0.95)
{
  dat=get(bivariate_stats)
  N=dat$N[1]
  mu.x=dat$mu[1]
  mu.y=dat$mu[2]
  V=dat$V
  y=get(y.vec)
  B_b.s=V[1,2]/V[1,1]
  r=V[1,2]/sqrt(V[1,1]*V[2,2])
  s2_y.x=(1-r^2)*V[2,2]*(N-1)/(N-2)
  s2B=s2_y.x/(V[1,1]*(N-1))
  t.crit=qt((1+area)/2,N-2)
  K=B_b.s^2-t.crit^2*s2B
  Bc=V[1,1]/V[1,2]
  ac=mu.x-Bc*mu.y
  SSQ=V[2,2]*(N-1)

  n.cases=length(y)
  if(n.cases==1){
  est=Bc*y+ac
  plus.minus=t.crit/K*sqrt(s2_y.x*((est-mu.y)^2/SSQ + K*(1+1/N)))
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
    plus.minus=t.crit/K*sqrt(s2_y.x*
      ((est[i]-mu.y)^2/SSQ + K*(1+1/N)))
    L[i]=round(est[i]-plus.minus,n.dec)
    U[i]=round(est[i]+plus.minus,n.dec)
    est[i]=round(est[i],n.dec)
  }
  results=data.frame(L,est,U)
  colnames(results)=c('lo','est','hi')
  }
  return(results)
}
