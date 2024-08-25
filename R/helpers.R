#' @export
dt_scaled<-function (x, df, mean = 0, sd = 1)
{
  dt((x - mean)/sd, df)/sd
}

qt_scaled<-function (p=.95, df, mean = 0, sd = 1)
{
  mean + sd * qt((1+p)/2, df, lower.tail = TRUE)
}
