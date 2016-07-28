## makeCacheMatrix creates a matrix object that can cache its own inverse.
## It returns a list containing methods to set and get the matrix and the matrix's inverse.

## casheSolve takes in a matrix created from makeCacheMatrix and solves and returns the matrix's
## inverse.

## this function takes in a conventional matrix object and returns a list containing
## 4 functions 2 functions set and get the value of the matrix object, while another 2
## functions set and get the value of the inverse of the matrix.
makeCacheMatrix <- function(A = matrix()) {
  I <- NULL
  set <- function(B)
  {
    A <<- B
    I <<- NULL
  }
  get <- function() A
  setInverse <- function(Inv)
  {
    I <<- Inv
  }
  getInverse <- function() I
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## this function takes a matrix created from makeCacheMatrix and checks if the
## matrix has a cached inverse. If not, the function solves for the inverse and 
## caches it into the original matrix object. The function also returns in inverse.
cacheSolve <- function(A, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  I <- A$getInverse()
  if (!is.null(I))
  {
    message("getting cached data")
    return(I)
  }
  data <- A$get()
  I <- solve(data)
  A$setInverse(I)
  I
}
