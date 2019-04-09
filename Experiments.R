# Load library
library(DJL)
source("dm.dea.intertemporal.R")

# Experimental data
df.io <- array(c(2, 4, 8, 4, 1, 2, 2, 2, 3, 6, 12, 6,
                 5, 4, 3, 8, 1, 1, 1, 1, 5, 4,  3, 8),
               c(4, 3, 2), 
               dimnames = list(LETTERS[1:4], c("X", "Y", "z"), c("t1", "t2")))
df.zt <- array(c(2, 3, 5, 10), c(4, 1), dimnames = list(LETTERS[1:4], c("Z^t")))
df.bg <- array(apply(df.io[,id.z,], 1, sum) + c(df.zt), c(4, 1), dimnames = list(LETTERS[1:4], c("Z^t")))

# Parameter
id.x <- c(1)
id.y <- c(2)
id.z <- c(3)
rts  <- "crs"
ori  <- "i"

# Run
res.it <- dm.dea.intertemporal(df.io[,id.x,], df.io[,id.y,], df.io[,id.z,], df.zt, rts, ori)
res.ba <- dm.dynamic.ba(df.io[,id.x,], df.io[,id.y,], df.io[,id.z,], df.bg, rts, ori)
res.t1 <- dm.dea(df.io[,c(id.x, id.z), 1], df.io[,id.y, 1], rts, ori)
res.t2 <- dm.dea(df.io[,c(id.x, id.z), 2], df.io[,id.y, 2], rts, ori)

# Compare effs
cbind(res.it$efft, res.ba$eff.t, res.t1$eff, res.t2$eff)

df.io

data.frame(Z.0 = apply(df.io[,id.z,], 1, sum) + c(df.zt),
           z   = df.io[,id.z,],
           Z.T = c(df.zt))

res.ba$lambda
