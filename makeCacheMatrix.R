##Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##(there are also alternatives to matrix inversion that we will not discuss here).
##this assignment is to write a pair of functions that cache the inverse of a matrix.
##code1
##this function creates a special "matrix" object that can cache its inverse.
##p = inverse matrix
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##code2
##this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...){
  ##return a inverse of "x"
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##example for get solution
matrix<-makeCacheMatrix(matrix(rnorm(4),2,2))
matrix$get()
matrix$getInverse()
cacheSolve(matrix)
