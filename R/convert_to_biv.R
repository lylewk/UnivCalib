#' @title Convert regression parameters to summary bivariate statistics
#' @description Uses the regression of x on y, the average x, and the
#' correlation coefficent to write the summary bivariate statistics.
#' @param s.mu Real value giving the average of the x vector.  Referred
#' to here as "s.mu" given a common problem where x is s(tature).
#' Alternatively, x could be age regressed onto an age "indicator. Default:
#' s.mu = 1713.
#' @param s.sd Standard deviation of x, where x could be stature, age,
#' etc.  Default: s.sd = 74
#' @param b_s.b Regression coefficient for x on y.  Referred to here as
#' "b_s.b" as the default problems is for regression of stature on bone.
#' Default: b_s.b = 11.2
#' @param a_s.b Intercept ("a coefficient") for the regression of x on
#' y. Default: a_s.b = 982.1
#' @param r Regression coefficient between x and y.  Default: r = 0.62
#' @param N Integer sample size.  Default: N = 53
#' @param From Character string specifying the source of the data.
#' Default: From = #Left male MCIII"
#' @details The default values are taken from Musgrave and Harjena's (1978)
#' regression of stature on the (adjusted) male 3rd metacarpal (see their
#' Table 3).  Parameters have been converted so that they give the regression
#' of stature in millimeters on the 3rd metacarpal in millimeters.
#' @return An object with components $mu for the bivariate mean and $V
#' for the bivariate variance/covariance mtraix.
#' @references
#' Musgrave, Jonathan H, and Narendra K Harneja
#' 1978	The estimation of adult stature from metacarpal bone length. \emph{American
#' Journal of Physical Anthropology} 48(1):113-119.
#'
#' @export
convert_to_biv<-function (s.mu=1713,s.sd=74,b_s.b=11.2,a_s.b=982.1,
    r=0.62,N=53,From='Left male MCIII')
{
  s.mu=s.mu
  b.mu=(s.mu-a_s.b)/b_s.b
  var.s=s.sd^2
  cov.bs=r^2*var.s/b_s.b
  var.b=cov.bs^2/(r^2*var.s)
  mu=c(s.mu,b.mu)
  V=matrix(c(var.s,cov.bs,cov.bs,var.b),nc=2)
  object=list(From=From,N=N,mu=mu,V=V)
  class(object)='Summary bivariate statistics'
  object
}
