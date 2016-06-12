##First of all the matrix supplied is always invertible. The function
## makeCacheMatrix returns a list with the steps to obtain the inverse.
##The function cacheSolve calculates the inverse with solve.

## Read, assign an return the data

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate the inverse

cacheSolve <- function(x, ...) {
  
  i <- solve(x$get())
  x$setinv(i)
  i
  
}