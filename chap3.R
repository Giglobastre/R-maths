#ex 4.1.1
rm(list=ls())
#creer des vecteurs
v1 = c(1,1,-1,2,3)
v2 = c(1,-1,2,3,1)
v3 = c(-1,2,3,1,1)
v4 = c(2,3,1,1,-1)
v5 = c(3,1,1,-1,2)
#rassemble les vecteurs en matrice
P = cbind(v1,v2,v3,v4,v5)
#calcule le determinant
det(P)
#inverse
P_inv = solve(P)
P_inv
x_old = c(1,0,3,5,2)
#multiplication matricielle
x_new = P_inv%*%x_old

#ex 4.1.5
rm(list=ls())
#rng seed
set.seed(452)
#creer une distrib normale de deviation sd et de moyenne mean
A1 = rnorm(8,mean=0,sd=1)
A2 = rnorm(8, mean=5,sd=0.5)
A3 = rnorm(8, mean=15,sd=1)
A4 = rnorm(8, mean=2,sd=1)

A = cbind(A1,A2,A3,A4)
#arrondi a 3 termes apres la ,
A = round(A,3)
#applique la fonction
vec_mu = apply(X = A, MARGIN = 2, FUN = mean)
#crée une matrice
mat_mu = matrix(data = rep(vec_mu,8), nrow = 8, ncol = 4, byrow = 1)

A_centered= A-mat_mu
#t fais la transposée
cov_A=t(A_centered)%*%A_centered

vec_sd=apply(X=A,MARGIN=2,FUN=sd )
mat_sd= matrix(data=rep(vec_sd,8),nrow=8,ncol=4,byrow=1)
mat_A_scaled=A_centered/mat_sd
mean(mat_A_scaled[,1])
sd(mat_A_scaled[,1])
mean(mat_A_scaled[,3])
sd(mat_A_scaled[,3])

#ex 4.1.6
Gamma=
  rbind(
    c(1,1,-1),
    c(0,2,3),
    c(-2,1,1)
  )
A=t(Gamma)%*%Gamma

#cherche les eigen values
eigen_value_A=eigen(A)$values
eigen_vector_A=eigen(A)$vectors
sum(eigen_value_A)
sum(diag(A))

A_rebuild=
  eigen_vector_A%*%
    diag(eigen_value_A)%*%
      t(eigen_vector_A)

diag_A2=diag(eigen_value_A^2)
A2=
  eigen_vector_A%*%
  diag_A2%*%
  t(eigen_vector_A)
diag_A3=diag(eigen_value_A^3)
A3=
  eigen_vector_A%*%
  diag_A3%*%
  t(eigen_vector_A)
diag_A_sqrt=diag(sqrt(eigen_value_A))
A_sqrt=
  eigen_vector_A%*%
  diag_A_sqrt%*%
  t(eigen_vector_A)
A_sqrt%*%A_sqrt

#ex 4.1.7
rm(list=ls())
set.seed(285)
mu=
  runif(
    n=5,
    min=-1,
    max=4
  )
mu=
  round(mu,3)
Gamma=
  matrix(
    rnorm(25),
    nrow=5,
    ncol=5
  )
Sigma=
  t(Gamma)%*%Gamma
Sigma=
  round(Sigma,3)

eigen_value_Sigma=
  eigen(Sigma)$values
eigen_vector_Sigma=
  eigen(Sigma)$vectors
L=
  eigen_vector_Sigma%*%
  diag(
    sqrt(eigen_value_Sigma)
  )
nb_realization=
  30
set.seed(380)
mat_shock=
  matrix(
    data=
      rnorm(5*nb_realization),
    nrow=5,
    ncol=nb_realization
  )
17
mat_mu=
  matrix(
    data=rep(mu,nb_realization),
    nrow=5,
    ncol=nb_realization,
    byrow=FALSE
  )
mat_X=
  mat_mu+L%*%mat_shock
mat_real_X=
  t(mat_X)
mat_real_X=
  round(mat_real_X,3)

#ex 4.1.8
#decompostion spectrale

rm(list=ls())
v1=c(1,1,-1,2,1)
v2=c(1,0,2,-1,1)
v3=c(-1,2,-1,0,1)
v4=c(2,1,0,1,-1)
v5=c(1,1,1,0,2)
P=
  rbind(v1,v2,v3,v4,v5)
det(P)
D=
  diag(c(4,3,2,1,0.5))
A=
  solve(P)%*%D%*%P

my_PC_object=
  princomp(
    x=A,
    cor=TRUE
  )
attributes(my_PC_object)
summary(my_PC_object)
my_PC_object$sdev
my_PC_object$loadings
my_PC_object$center
my_PC_object$scale
my_PC_object$n.obs
my_PC_object$scores
my_PC_object$call
class(
  my_PC_object$scores
)
OO=cbind(
  my_PC_object$scores[,1],
  my_PC_object$scores[,2],
  my_PC_object$scores[,3],
  my_PC_object$scores[,4],
  my_PC_object$scores[,5]
)
screeplot(
  my_PC_object,
  col="red",
  pch=16,
  type="lines",
  cex=2,
  lwd=2,
  main="My PC analysis"
)
biplot(
  my_PC_object,
)
OO=
  round(
    OO,
    digits=4
  )


























