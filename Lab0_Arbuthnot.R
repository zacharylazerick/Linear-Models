## Commands for Lab0_Arbuthnot

# Read in data
source("http://www.openintro.org/stat/data/arbuthnot.R")

# Determine basic properties
dim(arbuthnot)
names(arbuthnot)

# Exploration
attach(arbuthnot)
boys
girls

# Plots
plot(year,girls,type = "p")
plot(year, boys, type = "p")
plot(year,girls+boys,type = "p")
plot(year,boys/girls,type = "p")
plot(year,boys/(girls+boys),type = "p")

# Logical
(boys > girls)

