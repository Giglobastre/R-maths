#ex 4.4.4
rm(list=ls())
fct <- function(x) z = cos(x^2)
fct1 <- function(x,y) z = 3 - 8 * x + x^2
curve(fct,from = -5, to = 5)

range <- c(-100,110)
polyroot(c(3, -8, +1))
#uniroot(fct,range)
#-----------------------------------------------------------------
#ex 4.1.2
rm(list=ls())
fct <- function(x,y) z = cos(x^2)
range <- -100:100
curve(fct,from = -100, to = 100)
optimize(fct, range)
#-----------------------------------------------------------------
#3D plot

#genere une liste de 30 valeurs reparties en 20 valeurs
x=seq(
  from = -10,
  to = 10,
  length = 30
)
y = x
f = function(x,y){
  r = sqrt(x^2+y^2);
  z = 10 * sin(r) / r
  return(z)
}
#calcule f a tout les couples possible de x,y
z = outer(
  x,
  y,
  f
)
#plotting
# persp(
#   x,
#   y,
#   z
# )
#plotting with tilt pan roll
persp(
  x,
  y,
  z,
  theta = 30,
  phi = 30,
  expand = 0.5,
  col = "lightblue"
)
#autre type de plot 3D
#image(x,y,z)
#contour(x,y,z)
filled.contour(x,y,z)
#-----------------------------------------------------------------
#minimisation
f = function(X){
x=X[1]
y=X[2]
a=exp(-(x-1.2)^2-(y-2)^2)
b=cos(2*pi*(x-1.2))
z=a*b
return(z)
}
my_obj_nlm=
nlm(
f=f,
c(4,5)
)
attributes(my_obj_nlm)
my_obj_nlm$minimum
my_obj_nlm$estimate
my_obj_nlm$gradient
my_obj_nlm$code
my_obj_nlm$iterations

my_obj_nlminb=
  nlminb(
    c(0.8,0),
    f,
    lower=c(0.5,0),
    upper=c(1.5,4)
  )
attributes(my_obj_nlminb)
