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
df.eff  <- simplify2array(by(df.2d[, -c(1, 11)], df.2d$Year, as.matrix))


# Parameter
id.t <- c(1)
id.x <- c(2:4)
id.y <- c(5:6)
id.z <- c(7)
id.f <- c(8)
rts  <- "vrs"
ori  <- "i"

# Budget
df.Z.T <- apply(df.eff[, id.f, ], 1, sum)
df.Z.0 <- apply(df.eff[, id.z, ], 1, sum) + df.Z.T


#########################################################################################################################
### Descriptive Statistics
#########################################################################################################################

# Summary
boxplot(scale(df.2d[, 3:9]))
summary(df.2d[, 3:9])

# Table.1
table.1 <- sapply(df.2d[, c(id.x, id.z, id.y) + 1], function(x) c(Min  = min(x), 
                                                                  Med  = median(x), 
                                                                  Mean = mean(x), 
                                                                  Max  = max(x), 
                                                                  Std  = sd(x)))
print(noquote(format(round(t(table.1), 2), big.mark = ",")))

#########################################################################################################################
### Analysis
#########################################################################################################################

# Run function
res.it <- dm.dea.intertemporal(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.Z.T, rts, ori)
res.ba <- dm.dynamic.ba(df.eff[, id.x, ], df.eff[, id.y, ], df.eff[, id.z, ], df.Z.0, rts, ori)
res.pw <- cbind(dm.dea(df.eff[, c(id.x, id.z), 1], df.eff[, id.y, 1], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 2], df.eff[, id.y, 2], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 3], df.eff[, id.y, 3], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 4], df.eff[, id.y, 4], rts, ori)$eff,
                dm.dea(df.eff[, c(id.x, id.z), 5], df.eff[, id.y, 5], rts, ori)$eff)

# Compare results
matrix(c(res.it$eff.t, res.ba$eff.t, res.pw), nrow(df.eff[,,1]), 
       dimnames = list(unique(df.2d$DMU), 
                       c(paste0("it.", 2012:2016), paste0("ba.", 2012:2016), paste0("in.", 2012:2016))))

# Table.2
table.2 <- data.frame(Name    = unique(df.2d$DMU), 
                      it.avg  = round(res.it$eff,        4),
                      ba.avg  = round(res.ba$eff,        4),
                      it.2012 = round(res.it$eff.t[, 1], 4), 
                      ba.2012 = round(res.ba$eff.t[, 1], 4), 
                      it.2013 = round(res.it$eff.t[, 2], 4), 
                      ba.2013 = round(res.ba$eff.t[, 2], 4), 
                      it.2014 = round(res.it$eff.t[, 3], 4), 
                      ba.2014 = round(res.ba$eff.t[, 3], 4), 
                      it.2015 = round(res.it$eff.t[, 4], 4), 
                      ba.2015 = round(res.ba$eff.t[, 4], 4), 
                      it.2016 = round(res.it$eff.t[, 5], 4), 
                      ba.2016 = round(res.ba$eff.t[, 5], 4))
print(table.2)
