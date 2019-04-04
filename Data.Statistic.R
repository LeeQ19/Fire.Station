#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
library("ggplot2")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)

# Set outliar
id.out <- c(NA)
df.clr <- df.raw[, ]

#########################################################################################################################
### Data Statistic
#########################################################################################################################

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
