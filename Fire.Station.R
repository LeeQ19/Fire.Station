# Load library
library(abind)

# Load function
source("dm.dea.intertemporal.R")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)

# Preprocess data
df.eff   <- abind(split(df.raw[, c(-1, -11), ], df.raw[, c(-1, -11), ]$year), along = 3)

# Parameter
id.t     <- 1
id.x     <- c(2:4)
id.y     <- c(5:7)
id.z     <- c(8)
id.f     <- c(9)
rts  <- "crs"
orientation <- "i"

# Preprocess data
df.final <- apply(df.eff[, id.f, ], 1, sum)

# Run function
result <- dm.dea.intertemporal(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.final, rts, orientation)
