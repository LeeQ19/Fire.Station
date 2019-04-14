#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
library("ggplot2")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)

# Set outlier
id.out <- c(12, 46, 80, 114, 148)
df.clr <- df.raw[-id.out, ]

#########################################################################################################################
### Data Statistic
#########################################################################################################################

# Summary
boxplot(scale(df.raw[, 3:10]))
summary(df.raw[, 3:10])

boxplot(scale(df.clr[, 3:10]))
summary(df.clr[, 3:10])

# Employee - Reduction.of.damage
ggplot(df.clr, aes(x = Employee, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Employee - Rescue
ggplot(df.clr, aes(x = Employee, y = Rescue, color = Location)) + 
  geom_point()

# Ambulance - Reduction.of.damage
ggplot(df.clr, aes(x = Ambulance, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Ambulance - Rescue
ggplot(df.clr, aes(x = Ambulance, y = Rescue, color = Location)) + 
  geom_point()

# Firewagon - Reduction.of.damage
ggplot(df.clr, aes(x = Firewagon, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Firewagon - Rescue
ggplot(df.clr, aes(x = Firewagon, y = Rescue, color = Location)) + 
  geom_point()
