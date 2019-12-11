sampleCount <- function(meancount, sdcount){
  
  # single draw from the truncated normal dist. Lower bounded 0, no upper bound
  rtnorm(n = 1, mean = meancount, sd = sdcount, lower = 0)
  
}



