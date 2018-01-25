## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
      x <<- y
      invs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse and sets the value in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}

