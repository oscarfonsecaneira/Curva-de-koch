graficar = function(lado,n){
  altura =sign((lado^2)-((lado/2)^2))*abs((lado^2)-((lado/2)^2))^(1/2)
  altura
  x=c(0, lado, lado/2)
  y=c(0, 0, altura)
  if (n==0){
    plot(x,y, pch=19, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Diagrama de Nodos ")
  }
  else {
    for (i in 1:n){
      al = sign(((lado/3)^2)-((lado/6)^2))*abs(((lado/3)^2)-((lado/6)^2))^(1/2)
      x = c(x, lado/3, 2*lado/3, lado/2, lado/6, lado/3, 2*lado/3, 5*lado/6, 0, 1)
      y = c(y, 0, 0, -al, altura/3, 2*altura/3, 2*altura/3, altura/3, 2*altura/3, 2*altura/3)
    }
    plot(x,y, pch=19, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Curva de Koch ")
  }
}

perimetro = function(n){
  perimetro = (4^n)/(3^(n-1))
  return (perimetro)
}

lado = 1
n = 1
graficar(lado,n)
trozos = 3*(4^n)
trozos
perimetro(n)
