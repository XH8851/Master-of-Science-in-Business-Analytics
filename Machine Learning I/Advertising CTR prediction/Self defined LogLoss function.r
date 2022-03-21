LogLoss <- function(PHat,Click) {
  Y <- as.integer(Click)
  eps <- 1e-15
  out <- -mean(Y*log(pmax(PHat,eps))+(1-Y)*log(pmax(1-PHat,eps)))
  return(out)
}