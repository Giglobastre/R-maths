#ex 4.4.4
rm(list=ls())
fct <- function(x) z = cos(x * x)
fct1 <- function(x,y) z = 3 - 8 * x + x^2
curve(fct,from = -5, to = 5)

range <- c(-100,110)
#polyroot(c(3-8+1))
uniroot(fct,range)

#ex 4.1.2
rm(list=ls())
fct <- function(x,y) z = cos(x^2)
range <- -100:100
curve(fct,from = -100, to = 100)
optimize(fct, range)