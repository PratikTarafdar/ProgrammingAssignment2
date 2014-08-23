## The pair of functions creates a special object that stores a matrix and cache's
## its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mInv <<- inverse
  getinv <- function() mInv
  list(set = set, get = get,
       setinv = setinv ,
       getinv = getinv )
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <-solve(data)
  x$setinv(i)
  i
}
