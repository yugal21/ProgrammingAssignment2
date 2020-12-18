## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The following function calculates the mean of the special "vector" created 
##with the above function. However, it first checks to see if the mean has 
##already been calculated. If so, it gets the mean from the cache and skips the 
##computation. Otherwise, it calculates the mean of the data and sets the value 
##of the mean in the cache via the setmean function.



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


## Write a short comment describing this function
##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
