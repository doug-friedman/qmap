solveLS = function(x, y){
  x2 = cbind(1, x)
  A = crossprod(x2)
  b = crossprod(x2, y)
  as.vector(solve(A,b))
}