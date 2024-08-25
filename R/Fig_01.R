#' @export
Fig_01<-function ()
{
  lab=expression(Left ~~ 3^rd ~~ MC ~~ mm.)

  sto=convert_to_biv()
  mu=sto$mu
  V=sto$V

  mixtools::ellipse(mu,sigma=V,npoints=300,newplot=T,type='l',alpha=0.05,
                    xlab='Stature (mm)',ylab='',xlim=c(1530,2500),ylim=c(55,100),
                    xaxs='i',yaxs='i')
  title(ylab=lab,line=2)

  lines(c(1531.8741,1894.13306),rep(75.32676,2),lty=3)
  lines(rep(1894.13306,2),c(55.27348,75.32676),lty=3)

  B_rma=sqrt(V[2,2]/V[1,1])
  r=V[1,2]/sqrt(V[1,1]*V[2,2])
  # RMA
  a_rma=mu[2]-B_rma*mu[1]
  abline(a_rma,B_rma)

  # Allometry (OLS bone on stature)
  B_b.s=V[1,2]/V[1,1]
  a_b.s=mu[2]-B_b.s*mu[1]
  abline(a_b.s,B_b.s)


  B_s.b=1/(V[1,2]/V[2,2])
  a_s.b=mu[2]-B_s.b*mu[1]
  abline(a_s.b,B_s.b)
  cat('\nInverted stature on bone slope (Inverse calib) = ',round(B_s.b,4))
  cat('\nRMA slope = ',round(B_rma,4))
  cat('\nOLS slope bone on stature (Classical calib) = ',round(B_b.s,4))
  cat('\n\nConvert OLS Bone on stature slope to (inverse of)')
  cat('\n      stature on bone slope (Inverse Calib)\n')
  cat('OLS bone on stature / r^2 = ',round(B_b.s/r^2,4),'\n\n')

  Cotter=90.7
  A=c(a_s.b,a_rma,a_b.s)
  B=c(B_s.b,B_rma,B_b.s)

  Est=(Cotter-A)/B
  lines(rep(Est[1],2),c(0,Cotter),lty=2,lwd=2)
  lines(rep(Est[2],2),c(0,Cotter),lty=3,lwd=2)
  lines(rep(Est[3],2),c(0,Cotter),lty=1,lwd=2)
  names(Est)=c('Inverse','RMA','Classical')
  cat('Estimated statures when MC3 length = 90.7 mm\n')
  print(Est)
  lines(c(Est[1],Est[3]),rep(Cotter,2))

  shape::Arrows(2388,55.27348+5,2388,55.27348,arr.adj=1,arr.type='simple',lwd=2)

  legend(x='topleft',bty='n',legend=c('x on y','RMA',
                                      'y on x','Actual stature'),lty=c(c(2,3,1),NA),lwd=c(rep(2,3),NA))
  par(font = 5) #change font to get arrows
  legend("topleft", legend = c(NA, NA, NA, NA), pch = c(NA,NA,NA, 175),
         bty = "n")
  text(2263.819,91.61802,expression('90.7 mm.'),adj=c(0,0))
  par(font = 1) #back to default
}
