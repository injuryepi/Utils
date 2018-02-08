smr.conf2 <-function (data, count, population, conf.level = 0.95){
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  
  count = enquo(count)
  population = enquo(population)
  
  Obs <- data
  exp <- (sum(dat[, 1])/sum(dat[, 2])) * dat[, 2]
  smr <- Obs/Exp
  #Boice and Monson method for CI for
  # standardized morbidity ratio (SMR)
  #Rothman KJ, Greenland S. Modern epidemiology.
  #Philadelphia, PA: Lippincott-Raven; 1998.P. 236
  
  low <- exp( (log(Obs/Exp)) - (z*(1/sqrt(Obs))) )
  up <- exp( (log(Obs/Exp)) + (z*(1/sqrt(Obs))) )
  rval <- data.frame(est = smr,lower = low,
                     upper = up)
  return(rval)
}
