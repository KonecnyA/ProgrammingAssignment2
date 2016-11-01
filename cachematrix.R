## Matrix inversion is a costly computation.
## May be beneficial to cache vs. recompute.
## Here are two functions to cache the inverse of a matrix.

## 1st function, makCacheMatrix, creates a special "matrix" object, which is really a list conataining a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversed
## 4. get the value of the matrix inversed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2nd function, cacheSolve, computes the inverse of the special "matrix" returned by function above.
## 1st checks - has inverse already been calculated (and the matrix has not changed).
## If yes     - gets the inverse from the cache and skips the computation.
## If no,     - calculates the inverse and sets the inverse in the cache via the setinverse function.
## Assume     - the matrix is invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}