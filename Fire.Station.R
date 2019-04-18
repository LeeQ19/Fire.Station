#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load function
source("dm.dynamic.ba.R")
source("dm.dea.intertemporal.R")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)
df.eff <- simplify2array(by(df.raw[,-c(1, 11)], df.raw$Year, as.matrix))

# Parameter
id.t        <- 1
id.x        <- c(2:4)
id.y        <- c(5:6)
id.z        <- c(8)
id.f        <- c(9)
rts         <- "crs"
orientation <- "i"

# Preprocess data
df.final   <- apply(df.eff[, id.f, ], 1, sum)
df.initial <- apply(df.eff[, id.z, ], 1, sum) + df.final


#########################################################################################################################
### Analysis
#########################################################################################################################

# Run function
res.it <- dm.dea.intertemporal(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.final, rts, orientation)
res.ba <- dm.dynamic.ba(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.initial, rts, orientation)

# Compare results
matrix(c(res.it$efft, res.ba$eff.t), nrow(df.eff[,,1]), 
       dimnames = list(levels(df.raw$DMU), 
                       c(paste0("it.", 2012:2016), paste0("ba.", 2012:2016))))


