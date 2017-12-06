## Put comments here that give an overall description of what your
## functions do

## x = a square invertible matrix
## return a list containing functions to 
## 1. Set the matrix 2. get the matrix 3.set the inverse 4. get the inverse
## the list is used as the input to cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv

  list(set=set,get=get, setinv=setinv, getinv=getinv)
}


## x = output of makecachematrix 
## return inverse of the original matrix 

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
      ## if the inverse has already been calculated
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        
        new.mat = x$get()
        inv = solve(new.mat, ...)
        x$setinv(inv)
        
        return(inv)
}
