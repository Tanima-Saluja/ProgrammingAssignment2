## Here in this Assignment ,we are caching the inverse of a matrix. Matrix
## inversion is costly computation hence we can get it cached to make calculation
## quick and easy.it may make sense to cache the value of the mean so that when 
## we need it again, it can be looked up in the cache rather than recomputed.
## 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.Also  <<- operator can be used to assign
## a value to an object in an environment that is different from 
## the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
    }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set , get = get , setInverse = setInverse , getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
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
