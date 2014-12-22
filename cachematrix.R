## Computing and caching inverse of a matrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Calcualtes inverse of a matrix from matrix and cache it in matrix mi

makeCacheMatrix <- function(x = matrix()) {
 mi <- matrix()
 set <- function(y) {
     x <<- y
     mi <<- matrix()
 }
 get <- function() x
 setInverse <- function(solve) mi <<- solve
 getInverse <- function() mi
 list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
 mi <- x$getInverse()
 if( !is.na(mi[1,1])) {
  message("Getting cached data")
  return(mi)
 }
 data <- x$get()
 mi <- solve(data, ...)
 x$setInverse(mi)
 mi
}
