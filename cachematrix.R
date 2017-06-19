## Two functions: makeCachMatrix and cachesolve
##
## 1. makeCacheMatrix, which returns a list of functions: 
## set - takes in the given matrix
## get - retrieves that matrix
## setinverse - the R fnction "inverse"
## getinverse - the result of a previous call of the inverse function

## Write a short comment describing this function

makeCacheMatrix <- function(A = matrix()) {
    i <- NULL
  set <- function(B) {
          A <<- B
          i <<- NULL
  }
  get <- function() A
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2. cacheSolve given a matrix, tests if its inverse has already 
## been calculated, in which case it is in the cache, and is returned
## without further calculation, otherwise it calculates the inverse.
cacheSolve <- function(A, ...) {
  i <- A$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- A$get()
  i <- solve(data, ...)
  A$setinverse(i)
  i
}
