## The functions that you can see below are used for improve the performance of getting the inverse of matrices
## by cache the Inverse-Matrix in the memory and get it from the cache when it required, for doing it we need a list of functions that handle
## cache for each Matrix

## Build the list of function that handle the Cache for Matrix x
makeCacheMatrix <- function(x = matrix()) {
  inMat <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inMat <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(y) inMat <<- y
  getInverseMatrix <- function() inMat
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Retrieve an Inverse-Matrix from cache if exists or calculate it and Store the Matrix (from list x) and it's inverse-matrix in the cache
cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("Getting cached Inverse-Matrix")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}


## Test function (unrequired)
test <- function() {
  mat <- 1:4
  dim(mat) <- c(2, 2)
  message("Source Matrix")
  print(mat)
  message("Inversed Matrix")
  x <- makeCacheMatrix(mat)
  print(cacheSolve(x))
  mat_r <- cacheSolve(x)
  mat_r
  message("Checking that we get Unit Matrix when multiply the matrices:")
  print(mat %*% mat_r)
}

test()

