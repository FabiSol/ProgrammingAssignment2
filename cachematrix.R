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


###If the inverse has been calculated returns the inverse from the cache.
## Calculate the inverse

cacheSolve <- function(x, ...) {
  
  i<-x$getinv()
  
  if(!is.null(i)){
    
    message("GETTIN CACHE DATA")
    return(i)
  }
  
  i <- solve(x$get())
  x$setinv(i)
  i
  
}
