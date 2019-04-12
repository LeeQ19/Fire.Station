#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("abind", "DJL")
sapply(pkgs, require, character.only = T)

source("dm.dea.intertemporal.R")
source("dm.dynamic.ba.R")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)

# Preprocess data
df.eff <- abind(split(df.raw[, c(-1, -11), ], df.raw[, c(-1, -11), ]$Year), along = 3)

# Parameter
id.t        <- 1
id.x        <- c(2:4)
id.y        <- c(5:6)
id.z        <- c(8)
id.f        <- c(9)
rts         <- "crs"
orientation <- "i"

# Preprocess data
df.final <- apply(df.eff[, id.f, ], 1, sum)
df.init  <- apply(df.eff[, id.z, ], 1, sum) + apply(df.eff[, id.f, ], 1, sum)

#########################################################################################################################
### Analysis
#########################################################################################################################

# Run intertemporal
result.1 <- dm.dea.intertemporal(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.final, rts, orientation)

result.1$efft

# Run budget allocation
result.2 <- dm.dynamic.ba(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.init, rts, orientation)

result.2$eff.t

