## Function makeCacheMatrix creates a matrix that can cache its inverse.
## Function cacheSolve reads cached inverse (if possible), or computes the inverse.

## Function makeCacheMatrix creates a matrix that can cache its inverse. Takes a matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function cacheSolve reads cached inverse (if possible), or computes the inverse. 
## Takes as an argument a CacheMatrix and optionaly arguments that are passed to function
## solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
