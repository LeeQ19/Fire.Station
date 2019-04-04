# Load library
library(DJL)

# Experimental data
df <- array(c(2, 4, 8, 1, 1, 1, 3, 6, 6, 2, 5, 5,
              5, 4, 3, 1, 1, 1, 5, 4, 3, 3, 2, 1),
            c(3, 4, 2), 
            dimnames = list(LETTERS[1:3], c("X", "Y", "z", "Z^t"), c("t1", "t2")))

# Parameter
id.x <- c(1)
id.y <- c(2)
id.z <- c(3)
id.f <- c(4)
rts  <- "crs"
ori  <- "i"

# Run
finalz <- apply(df[,id.f, ], 1, sum)
res.it <- dm.dea.intertemporal(df[,id.x,], df[,id.y,], df[,id.z,], finalz, rts, ori)
res.t1 <- dm.dea(df[,c(id.x, id.z), 1], df[,id.y, 1], rts, ori)
res.t2 <- dm.dea(df[,c(id.x, id.z), 2], df[,id.y, 2], rts, ori)

# Compare effs
cbind(res.it$efft, res.t1$eff, res.t2$eff)

