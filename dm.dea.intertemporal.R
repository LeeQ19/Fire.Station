dm.dea.intertemporal <- function(xdata, ydata, zdata, finalz, rts = "crs", orientation = "i"){
  # Load library
  library(lpSolveAPI)
  
  # Initial checks
  if(dim(xdata)[1] != dim(ydata)[1] | dim(xdata)[length(dim(xdata))] != dim(ydata)[length(dim(ydata))]) stop('Data must be balanced.')
  if(!(rts %in% c("crs", "vrs", "irs", "drs")))                                                         stop('rts must be "crs", "vrs", "irs", or "drs".')
  if(!(orientation %in% c("i", "o")))                                                                   stop('orientation must be "i" or "o".')
  
  # Parameters
  xdata    <- if(length(dim(xdata)) != 3)  array(xdata, c(dim(xdata)[1], 1, dim(xdata)[2])) else as.array(xdata)
  ydata    <- if(length(dim(ydata)) != 3)  array(ydata, c(dim(ydata)[1], 1, dim(ydata)[2])) else as.array(ydata)
  zdata    <- if(length(dim(zdata)) != 3)  array(zdata, c(dim(zdata)[1], 1, dim(zdata)[2])) else as.array(zdata)
  finalz   <- if(length(dim(finalz)) != 2) array(finalz, c(length(finalz), 1))              else as.array(finalz)
  initialz <- finalz + apply(zdata, 1, sum)
  
  n        <- dim(xdata)[1]
  m        <- dim(xdata)[2]
  s        <- dim(ydata)[2]
  b        <- dim(zdata)[2]
  t        <- dim(xdata)[3]
  
  # Data frames
  results.efficiency  <- array(NA, dim = c(n, 1))
  results.lambda      <- array(NA, dim = c(n, n))
  results.efficiencyt <- array(NA, dim = c(n, t))
  results.xslack      <- array(NA, dim = c(n, m, t))
  results.yslack      <- array(NA, dim = c(n, s, t))
  results.zslack      <- array(NA, dim = c(n, b, t))
  
  # LP
  for(j in 1:n){
    # Declare LP
    lp.dea <- make.lp(0, n + t + m * t + b * t + s * t + b + b) # lambda+efficiency+xslack+zslack+yslack+initialZslack+finalZslack
    
    # Set objective
    if(orientation == "i") set.objfn(lp.dea, c(rep(1 / t, t)),  indices = c((n + 1):(n + t)))
    if(orientation == "o") set.objfn(lp.dea, c(rep(-1 / t, t)), indices = c((n + 1):(n + t)))
    
    # RTS
    if(rts == "vrs") add.constraint(lp.dea, c(rep(1, n)), indices = c(1:n), "=", 1)
    if(rts == "crs") set.constr.type(lp.dea, 0, 1)
    if(rts == "irs") add.constraint(lp.dea, c(rep(1, n)), indices = c(1:n), ">=", 1)
    if(rts == "drs") add.constraint(lp.dea, c(rep(1, n)), indices = c(1:n), "<=", 1)
    
    # constraints
    for(k in 1:t){
      # Input constraints
      for(i in 1:m){
        if(orientation == "i")
          add.constraint(lp.dea, c(xdata[, i, k], -xdata[j, i, k], 1), indices = c(1:n, n + k, n + t + m * (k - 1) + i), "=", 0)
      }
      # Stock constraints
      for(i in 1:b){
        if(orientation == "i")
          add.constraint(lp.dea, c(zdata[, i, k], -zdata[j, i, k], 1), indices = c(1:n, n + k, n + t + m * t + b * (k - 1) + i), "=", 0)
      }
      # Output constraints
      for(r in 1:s){
        if(orientation == "i")
          add.constraint(lp.dea, c(ydata[, r, k], -1), indices = c(1:n, n + t + m * t + b * t + s * (k - 1) + r), "=", ydata[j, r, k])
      }
    }
    # Initial Stock constraints
    for(i in 1:b){
      if(orientation == "i")
        add.constraint(lp.dea, c(initialz[, i], 1), indices = c(1:n, n + t + m * t + b * t + s * t + i), "=", initialz[j, i])
    }
    # Final Stock constraints
    for(i in 1:b){
      if(orientation == "i")
        add.constraint(lp.dea, c(finalz[, i], -1), indices = c(1:n, n + t + m * t + b * t + s * t + b + i), "=", finalz[j, i])
    }
    
    # Bounds
    set.bounds(lp.dea, lower = c(rep(0, n), rep(-Inf, t), rep(0, m * t + b * t + s * t + b + b)))
    
    # Solve
    solve.lpExtPtr(lp.dea)
    
    # Get results
    results.efficiency[j]    <- abs(get.objective(lp.dea))
    
    temp.p                   <- get.variables(lp.dea)
    results.lambda[j, ]      <- temp.p[1:n]
    results.efficiencyt[j, ] <- temp.p[(n + 1):(n + t)]
    results.xslack[j, , ]    <- array(temp.p[(n + t + 1):(n + t + m * t)], dim = c(m, t))
    results.yslack[j, , ]    <- array(temp.p[(n + t + m * t + 1):(n + t + m * t + s * t)], dim = c(s, t))
    results.zslack[j, , ]    <- array(temp.p[(n + t + m * t + s * t + 1):(n + t + m * t + s * t + b * t)], dim = c(b, t))
  }
  results <- list(eff    = results.efficiency, 
                  efft   = results.efficiencyt, 
                  lambda = results.lambda, 
                  xslack = results.xslack, 
                  yslack = results.yslack, 
                  zslack = results.zslack)
  return(results)
}
