# Check for, installs, and loads the pacman package.

#if the pacman package is not present, install it from the given repo and then load it.
if (!require("pacman"))
install.packages("pacman", repos = "http://cran.us.r-project.org"); library("pacman")
