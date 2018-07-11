## The functions provide an interface using which a cacheable matrix can de defined. When its inverse is required, then
## it will be calculated for the first time and caches such that on susequent calls, the cached inverse is returned.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <-NULL # initialize inverse as null
  set <- function(m)
  {
    x <<- m
    inv <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  getInverse <- function() {
     inv
  }
  
  setInverse <- function(inverse){
     inv <<- inverse
    
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(! is.null(inv))
  {
    message("Returning cached value")
    return (inv)
  }
  else
  {
    message("Cache does not exist")
    x_matrix <- x$get() 
    inv <- solve(x_matrix)
    x$setInverse(inv)
    inv
  }
}
