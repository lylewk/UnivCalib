#' @title Enter summary bivariate statistics
#' @description You will be prompted for all information
#' @usage enter_biv()
#' @details You will be prompted for a character string that describes the source
#' of the data, an integer giving the sample size, a real value for the mean of x,
#' a real value for the mean of y, the variance of x, the variance of y, and the
#' covariance of x and y.  Note that the x variable is the "explanatory" variable
#' and y is the "explained value."  In calibration terminology, x is the known value
#' from a "gold standard" and y is the calibration measurement.  In biological
#' anthropology x could be known stature and y could be a bone length (as in allometry)
#' or x could be known age and y could be osteon population density.
#' @return Returns an object
#' @examples
#' \dontrun{
#' enter_biv() # There are no parameters. You will be prompted for everything.
#' }
#' @export
enter_biv <-
  function ()
  {
    cat('Enter a name for the sample (without quotes):\n')
    sample.name=scan('',n=1,quiet=T,what=character())

    cat('Enter the sample size, n=\n')
    n=as.integer(scan('',n=1,quiet=T))

    # Get means
    mu=vector()
    cat('Enter the mean x\n')
    mu[1]=as.numeric(scan('',n=1,quiet=T))

    cat('Enter the mean y\n')
    mu[2]=as.numeric(scan('',n=1,quiet=T))

    # Get V
    V=matrix(NA,nc=2,nr=2)
    cat('Enter the variance of  x\n')
    V[1,1]=as.numeric(scan('',n=1,quiet=T))
    cat('Enter the variance of  y\n')
    V[2,2]=as.numeric(scan('',n=1,quiet=T))
    cat('Enter the variance of covariance of x & y\n')
    V[1,2]=as.numeric(scan('',n=1,quiet=T))
    V[2,1]=V[1,2]
    list(From=sample.name,n=n,mu=mu,V=V)
  }
