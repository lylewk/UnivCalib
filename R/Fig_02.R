#' @export
Fig_02<-function ()
{
  lab=expression(Left ~~ 3^rd ~~ MC ~~ mm.)
  opar = par(no.readonly = T)
  on.exit(par(opar))
  # Get bivariate summary statistics
  lab=expression(Left ~~ 3^rd ~~ MC ~~ mm.)
  opar = par(no.readonly = T)
  on.exit(par(opar))
  # Get bivariate summary statistics
  sto=convert_to_biv()
  N=sto$N
  mu=sto$mu
  V=sto$V
  B.sb=(V[1,2]/V[2,2])
  a.sb=mu[1]-mu[2]*B.sb
  r=V[1,2]/sqrt(V[1,1]*V[2,2])
  see=sqrt(V[1,1]*(1-r^2)*(N-1)/(N-2))
  mu.s=mu[1]
  sd.s=sqrt(V[1,1])
  b=90.7
  #  SSb = (r*sd.s/B.sb)^2*(N-1)
  SSs = sd.s^2*(N-1)
  mu.b = mu[2]
  covb.s = V[1,2]
  Beta = V[1,2]/V[1,1]
  intercept = mu.b-Beta*mu.s

  par(mar=c(5,4,4,5)+.1)
  plot(c(1000,3000),c(0,100),type='n',xlab='Stature (mm)',
       ylab=lab,axes=F)
  axis(1,pos=0)
  axis(2,pos=1000)
  lines(c(0,3000),rep(100,2))
  lines(rep(3000,2),c(0,100))

  x=1000:3000
  y.hat=x*Beta+intercept

  t.crit=qt(1-0.05/2,df=N-2)
  see.new=sqrt(V[2,2]*(1-r^2)*(N-1)/(N-2))

  post.scale=sqrt(((N+1)*see.new^2)/(N*Beta^2))

  lines(x[y.hat<=100],y.hat[y.hat<=100])
  plus.minus = t.crit*see.new*sqrt(1+1/N+(x-mu.s)^2/SSs)
  y.plus=y.hat+plus.minus
  lines(x[y.plus<=100],y.plus[y.plus<=100],lty=2)
  draw.me=y.hat-plus.minus>=0
  lines(x[draw.me],y.hat[draw.me]-plus.minus[draw.me],lty=2)

  s2b = see.new^2/SSs
  K = Beta^2-t.crit^2*s2b
  Center=mu.s+Beta*(b-mu.b)/K
  p.m = t.crit/K*sqrt(see.new^2*((b-mu.b)^2/SSs+K*(1+1/N)))
  L = Center-p.m
  U = Center+p.m


  B.c = B.sb/(r^2)
  A.c = (mu.s*(r^2-1)+a.sb)/(r^2)
  est=b*B.c+A.c

  shape::Arrows(U,b,U,0,arr.adj = 1)
  shape::Arrows(L,b,L,0,arr.adj = 1,)
  shape::Arrows(est,b,est,0,arr.adj = 1,arr.type='triangle')
  lines(c(0,U),rep(b,2),lty=3,lwd=2)

  lines(2170:2760,10000*dt_scaled(2170:2760,N-2,est,post.scale))
  axis(4,pos=3000,at=c(0,20,40,60),labels=c(0,20,40,60)/10000)
  mtext('Density',side=4,line=2,at=30)
  shape::Arrows(2388,10,2388,0,arr.adj=1,arr.type='simple',lwd=2)
}
