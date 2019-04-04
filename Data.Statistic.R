#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
library("ggplot2")

# Load data
df.raw <- read.csv(url("http://bit.ly/Fire4Data"), header = T)

#########################################################################################################################
### Data Statistic
#########################################################################################################################

# Employee - Reduction.of.damage
ggplot(df.raw, aes(x = Employee, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Employee - Rescue
ggplot(df.raw, aes(x = Employee, y = Rescue, color = Location)) + 
  geom_point()

# Ambulance - Reduction.of.damage
ggplot(df.raw, aes(x = Ambulance, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Ambulance - Rescue
ggplot(df.raw, aes(x = Ambulance, y = Rescue, color = Location)) + 
  geom_point()

# Firewagon - Reduction.of.damage
ggplot(df.raw, aes(x = Firewagon, y = Reduction.of.damage, color = Location)) + 
  geom_point()

# Firewagon - Rescue
ggplot(df.raw, aes(x = Firewagon, y = Rescue, color = Location)) + 
  geom_point()

