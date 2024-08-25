#' @title Return summary bivariate statistics from raw data
#' @description Uses the regression of x on y, the average x, and the
#' correlation coefficent to write the summary bivariate statistics.
#' @param dat Character variable giving name of object with the
#' bivariate raw data Default: dat = "femur_train"
#' @details Calculates the summary bivariate statistics from raw data,
#' where there are two columns.  The first column is the x variable and
#' the second column is the y variable. __Note__: the "x" variable is
#' the independent (explanatory) variable and the "y" variable is
#' the dependent (explained) variable.  For age, age is the x variable
#' and the age indicator is the y variable.  For stature, stature is
#' the x variable and bone size is the y variable (as you would see in
#' allometry studies)
#' @return An object with components $mu for the bivariate mean and $V
#' for the bivariate variance/covariance mtraix.
#' @references
#' Musgrave, Jonathan H, and Narendra K Harneja
#' 1978	The estimation of adult stature from metacarpal bone length. \emph{American
#' Journal of Physical Anthropology} 48(1):113-119.
#'
#' @export
make_biv<-function(dat_source='femur_train'){
  dat=get(dat_source)
  N=NROW(dat)
  mu=apply(dat,2,mean)
  V=var(dat)
  return(list(From=dat_source,N=N,mu=mu,V=V))
}
