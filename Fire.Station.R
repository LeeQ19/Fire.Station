#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library and functions
pkgs <- c("DJL", "ggplot2")
sapply(pkgs, require, character.only = T)
source("dm.dynamic.ba.R")
source("dm.dea.intertemporal.R")

# Load data
df.2d  <- read.csv(url("http://bit.ly/Fire4Data"), header = T)
df.3d  <- simplify2array(by(df.2d[, -c(1, 11)], df.2d$Year, as.matrix))
id.out <- c(12)
df.eff <- df.3d[-id.out,,]

# Parameter
id.t <- c(1)
id.x <- c(2:4)
id.y <- c(5:6)
id.z <- c(8)
id.f <- c(9)
rts  <- "vrs"
ori  <- "i"

# Budget
df.Z.T <- apply(df.eff[, id.f, ], 1, sum)
df.Z.0 <- apply(df.eff[, id.z, ], 1, sum) + df.Z.T


#########################################################################################################################
### Analysis
#########################################################################################################################

# Table 1. Descriptive statistics
df.aggr <- cbind(df.2d[-id.out * 1:5, c(id.x) + 1],
                 df.2d[-id.out * 1:5, c(id.z, id.y[1]) + 1] * 10^-6,
                 df.2d[-id.out * 1:5, c(id.y[2]) + 1, drop = F],
                 Total.Budget = rep(df.Z.0 * 10^-6, 5))

table.1 <- data.frame(Min  = apply(df.aggr, 2, "min"),
                      Med  = apply(df.aggr, 2, "median"),
                      Mean = apply(df.aggr, 2, "mean"),
                      Max  = apply(df.aggr, 2, "max"),
                      Std  = apply(df.aggr, 2, "sd"))

print(format(round(table.1, 2), big.mark = ",", scientific = F))


# Table 2. Comparative results of efficiency
res.it <- dm.dea.intertemporal(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.Z.T, rts, ori)
res.ba <- dm.dynamic.ba(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.Z.0, rts, ori)
res.pw <- cbind(dm.dea(df.eff[, c(id.x, id.z), 1], df.eff[, id.y, 1], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 2], df.eff[, id.y, 2], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 3], df.eff[, id.y, 3], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 4], df.eff[, id.y, 4], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 5], df.eff[, id.y, 5], rts, ori)$eff)

table.2 <- matrix(c(res.it$eff, res.ba$eff, res.it$eff.t, res.ba$eff.t), nrow(df.eff[,,1]), 
                  dimnames = list(unique(df.2d$DMU)[-id.out], 
                                  c("it.aggr", "ba.aggr", paste0("it.", 2012:2016), paste0("ba.", 2012:2016))))

print(table.2[order(df.2d[1:nrow(df.3d), 1][-id.out]), c(1, 2, 3, 8, 4, 9, 5, 10, 6, 11, 7, 12)])


# Compare results
matrix(c(res.it$eff.t, res.ba$eff.t, res.pw), nrow(df.eff[,,1]), 
       dimnames = list(unique(df.2d$DMU)[-id.out], 
                       c(paste0("it.", 2012:2016), paste0("ba.", 2012:2016), paste0("in.", 2012:2016))))


#########################################################################################################################
### Descriptive Statistics
#########################################################################################################################

# Summary
boxplot(scale(df.2d[, 3:10]))
summary(df.2d[, 3:10])

# Employee - Reduction.of.damage
ggplot(df.2d, aes(x = Employee, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Employee - Rescue
ggplot(df.2d, aes(x = Employee, y = Rescue, color = Location)) + 
  geom_point()

# Ambulance - Reduction.of.damage
ggplot(df.2d, aes(x = Ambulance, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Ambulance - Rescue
ggplot(df.2d, aes(x = Ambulance, y = Rescue, color = Location)) + 
  geom_point()

# Firewagon - Reduction.of.damage
ggplot(df.2d, aes(x = Firewagon, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Firewagon - Rescue
ggplot(df.2d, aes(x = Firewagon, y = Rescue, color = Location)) + 
  geom_point()


