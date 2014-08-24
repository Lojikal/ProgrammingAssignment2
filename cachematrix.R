## Function takes in a Matrix X and computes the inverse of the matrix 
## if the Inverse is not cached.The Inverse of the Matrix is cached on run
## if it does not already exist.

## makeCacheMatrix will create the function for caching ther inverse matrix
## example:
## g <- matrix(c(1,2,3,4),2,2)
## cG <- makeCacheMatrix(g)
## cacheSolve(cG) ## will caculate the inverse
## cacheSolve(cG) ## will use the cached inverse instead of calculating it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve will cahche the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
