#ex 4.1.1
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

P_inv = solve(P)