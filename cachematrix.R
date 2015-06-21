## makeCacheMatrix creates a matrix object that can store its inverse,
## and is a list containing functions that: 
##  1. create a new matrix: makeCacheMatrix$set()
##  2. get the created matrix: makeCacheMatrix$get()
##  3. set matrix inverse: makeCacheMatrix$setinverse()
##  4. get matrix inverse: makeCacheMatrix$getinverse()
## cacheSolve is used to calculate the inverse of the matrix created
## by makeCacheMatrix, or return the matrix inverse if it has already
## been calculated.


## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  
  # Until calculated, set matrix inverse variable to NULL
  mat_inv <- NULL
  
  set <- function(new_data, ...) {
    # "..." used to pass required arguments to matrix(), without 
    # writing them all out. When calling $set() you can specify 
    # arguments that are needed by matrix() by name, or in the
    # order that they appear as arguments to matrix(). 
    mat <<- matrix(new_data, ...)
    
    # New matrix just created, so set matrix inverse variable
    # to NULL until the matrix inverse is properly calculated
    mat_inv <<- NULL
  }
  
  get <- function() mat
  
  setinverse <- function(mat_inverse) mat_inv <<- mat_inverse
  
  getinverse <- function() mat_inv
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix"returned by makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  
  ## Return a matrix that is the inverse of 'mat'
  
  # Get inverse of "mat" if already known
  # If inverse not already calculated, $getinverse() will return NULL
  mat_inv <- mat$getinverse()
  
  # Matrix inverse already cached, so return it
  if(!is.null(mat_inv)) {
    message("getting cached inverse matrix...")
    return(mat_inv)
  }
  
  # Matrix inverse not yet calculated, so calculate and display it
  mat_inv <- solve(mat$get(),...)
  mat$setinverse(mat_inv)
  mat_inv
}
