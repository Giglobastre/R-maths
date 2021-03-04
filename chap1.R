# ex 5.1.1
  #fonction et courbe
rm(list=ls())
fct = function(x){
  y = 60*sqrt(x)-5*x
  return(y)
}
fct(1)
fct(16)
fct(100)
fct(150)
curve(expr = fct)
curve(expr = fct, from = 0, to = 150)
abline(h = 0)

#ex 5.1.2
  #derivee
rm(list=ls())
a=D(expression(3 * exp(7 - 2 * x)),"x")
b=D(expression(log(x^2 + 6 * x + 2)),"x")
a
b

#ex 5.1.3
rm(list=ls())
fct <- function(x) (x - 2)^2
curve(expr = fct, from = -2, to = 6)
inv <- function(x) 1/fct(x)
curve(inv, from = 1, to = 3)
#restrindre ?

#ex 5.2.1
  #fonction a deux variables
rm(list=ls())
fct <- function(x,y) z = x^3 * y^2 - 2 * x^2 * y + 3 * x
a = fct(2,1)
b = fct(2,-1)
fctx <- function(x,y) z = 3 * x^2 * y^2 - 4 * x * y +3
fcty <- function(x,y) z = 2 * x^3 * y - 2 * x^2
c = fctx(2,1)
d = fcty(2,-1)

#ex 5.2.2
  #derivée partielle
rm(list=ls())
a=D(expression(x * y^2 * exp(x * z)),"x")
b=D(expression(x * y^2 * exp(x * z)),"y")

#ex 5.3.1
rm(list=ls())
fct <- function(x,y) z = 3 * x^2 - x * y
fctx <- function(x,y) z = 6 * x - y
fcty <- function(x,y) z = -x

dzx <- 0.1
dzy <- -0.2

a <- fct(1,2)
b <- fct(1 + dzx, 2 + dzy)
var_exacte = a - b

diff_fx0y0 = fctx(1,2) * dzx + fcty(1,2) * dzy

erreur = var_exacte - diff_fx0y0

#ex 5.4.1
rm(list=ls())
y <- function(t) y = 3 * t^2 + 2 * t + 1
x <- function(t) x = t^2 + 3 * t - 2
fctz <- function(t) z = x(t)^2 - 3 * x(t)^3 * y(t)^2 + 4 * x(t) - 2 * y(t)^2
fctz2 <- function(t) z2 = 
  (t^2 + 3 * t - 2)^2 -3 *
  (t^2 + 3 * t - 2)^3 * (3 * t^2 + 2 * t + 1)^2 + 4 *
  (t^2 + 3 * t - 2) - 2 * (3 * t^2 + 2 * t + 1)^2
return(z2)
curve(fctz2, from =-10, to = 10)
a=D(expression(fctz2),"t")
#ex 5.4.3















