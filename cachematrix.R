## This function is desgined to cache the matrix inverse for future use
## functions makeCacheMatrix does the cache and the function cacheSolve returns the inversed matrix

## In the vaiable x is set to orginal matrix and the m is intially set to Null then to inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Checks if the inverse exists - it will return the inverse if it exists else calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
           m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinv(m)
     m
}
