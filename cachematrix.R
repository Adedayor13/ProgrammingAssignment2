## Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Creating a special matrix object that can cahce its inverse, assuming the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## code that returns a matrix that is inverse of m
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


###Testing Example
c <- matrix(c(4,3,2,1),2,2)
c1 <- makeCacheMatrix(c) ##code to make Cache matrix of c
c2 <- cacheSolve(c1) #code to return inverse from Cache
c2 #inverse retruned from Cache
