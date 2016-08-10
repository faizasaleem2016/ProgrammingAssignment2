## I am attempting to Cache the inverse of a matrix rather than computing it
## repeatedly

## The first function, "makeCacheMatrix" makes a special "matrix" object, which
## is a list containing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has  already 
## been calculated (and the matrix has not changed), then`cacheSolve`
## should retrieve the inverse from the cache. Otherwise, it calculates 
## the matrix inverse. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
