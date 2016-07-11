## Cachematrix.R
##
## A couple of functions to allow storage of a matrix within a special
## list object, making possible the caching of the inverse matrix
## (so that the calculation need only be made once for a given matrix)
##
## makeCacheMatrix
##
## DLRA
## 9/7/16
##
## Function to construct a special "vector" (actually a list)
## containing functions to
##  1. Set the values of the matrix elements
##  2. Get the matrix
##  3. Set the values of the inverse matrix
##  4. Get the inverse matrix


makeCacheMatrix <- function(X = matrix()) {
  matInv <- NULL
  set <- function(Y) {
    X <<- Y
    matInv <<- NULL
  }
  get <- function() X
  setInv <- function(inv) matInv <<- inv
  getInv <- function() matInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve
##
## DLRA
## 10/7/16
##
## Function to calculate inverse of matrix stored using
## makeCacheMatrix.R


cacheSolve <- function(X,...) {
  ## take inverse from cache and if not NULL return object
  matInv <- X$getInv()
  print("a")
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  ## if NULL, compute inverse matrix using solve(X)
  data <- X$get()
  matInv <- solve(data,...)
  X$setInv(matInv)
  matInv
}