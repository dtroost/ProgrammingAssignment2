## The two functions makeCachematrix and cacheSolve below store, retrieve, and compute the inverse of a matrix.

## the function makeCachematrix creates a list of functions for matrix/value setting and retrieval

makeCachematrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
      x <<- y
      i <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve checks to see if the matrix inverse has already been stored.  if so, it will retrieve from cache.  Otherwise, compute new inverse.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
