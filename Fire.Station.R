#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library and functions
pkgs <- c("DJL")
sapply(pkgs, require, character.only = T)
source("dm.dynamic.ba.R")
source("dm.dea.intertemporal.R")

# Load data
df.2d <- read.csv(url("http://bit.ly/Fire4Data"), header = T)
df.3d <- simplify2array(by(df.2d[, -c(1, 11)], df.2d$Year, as.matrix))

# Parameter
id.t <- c(1)
id.x <- c(2:4)
id.y <- c(5:6)
id.z <- c(7)
id.f <- c(8)
rts  <- "vrs"
ori  <- "i"

# Budget
df.Z.T <- apply(df.3d[, id.f, ], 1, sum)
df.Z.0 <- apply(df.3d[, id.z, ], 1, sum) + df.Z.T


#########################################################################################################################
### Analysis
#########################################################################################################################

# Table 1. Descriptive statistics
df.aggr <- cbind(df.2d[, c(id.x) + 1],
                 df.2d[, c(id.z, id.y[1]) + 1] * 10^-6,
                 df.2d[, c(id.y[2]) + 1, drop = F],
                 Total.Budget = rep(df.Z.0 * 10^-6, 5))

table.1 <- sapply(df.aggr, function(x) c(Min  = min(x), 
                                         Med  = median(x), 
                                         Mean = mean(x), 
                                         Max  = max(x), 
                                         Std  = sd(x)))

noquote(format(round(t(table.1), 2), big.mark = ","))


# Table 2. Comparative results of efficiency
res.it <- dm.dea.intertemporal(df.3d[, id.x, ], df.3d[, id.y, ], df.3d[, id.z, ], df.Z.T, rts, ori)
res.ba <- dm.dynamic.ba(df.3d[, id.x, ], df.3d[, id.y, ], df.3d[, id.z, ], df.Z.0, rts, ori)

table.2 <- matrix(c(res.it$eff, res.ba$eff, res.it$eff.t, res.ba$eff.t), nrow(df.3d[,,1]), 
                  dimnames = list(unique(df.2d$DMU), 
                                  c("it.aggr", "ba.aggr", paste0("it.", 2012:2016), paste0("ba.", 2012:2016))))

print(table.2[, c(1, 2, 3, 8, 4, 9, 5, 10, 6, 11, 7, 12)])


# How many system efficient DMUs?
apply(table.2[, 1:2], 2, function(x) sum(round(x, 8) == 1))


# Footnote 5
summary(lm((table.2[,1] - table.2[,2]) ~ df.Z.T))


# Table 3. Namyangju
id.nyj  <- which(rownames(table.2) == "Namyangju")
table.3 <- rbind(df.aggr[id.nyj + 33 * 0:4, -7],
                 aggregate(df.aggr[, -7], list(df.2d$Year), "mean")[, -1])
rownames(table.3) <- c(2012:2016, paste0("Avg.", 2012:2016))
round(t(table.3[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10),]), 2)


# Budget available at each T
df.A.t <- array(df.Z.0, dim(df.3d)[c(1, 3)], dimnames = list(unique(df.2d$DMU), 2012:2016))
for(i in 2:dim(df.3d)[3]){df.A.t[, i] <- df.A.t[, i - 1] - df.3d[, id.z, (i - 1), drop = F]}

