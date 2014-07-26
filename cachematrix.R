#-------------------------------------------------------------
# Filename: cachematrix.R
# Author: James Wolfe
# Johns Hopkins Data Science - Coursera - R Programming
# Programming Assignment 2
#
# Purpose: Computes the inverse of a matrix and caches the result
#   so that it may be obtained later without re-computing the inverse.
#--------------------------------------------------------------

#--------------------------------------------------------------
# Function: makeCacheMatrix
# Description: Constructs a "makeCacheMatrix" object which holds
#   the data of a matrix and a set of functions to store and retrieve
#   the inverse of the matrix
# Parameters: x - The matrix data
#--------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  # set: sets the matrix data "x" and initializes the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # get: returns the matrix data
  get <- function() {
     x
  }

  # setInverse: stores the matrix inverse data
  setInverse <- function(inverse) {
     inv <<- inverse
  }

  # getInverse: retrieves the matrix inverse data
  getInverse <- function() {
     inv
  }

  # return a list containing the functions for this object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#--------------------------------------------------------------
# Function: cacheSolve
# Description: Computes the inverse of a "makeCacheMatrix" object,
#   if the inverse has already been computed and cached it is
#   retrieved instead of re-computing the inverse.
# Parameters: x - "makeCacheMatrix" object to retrieve or compute
#                  the inverse of.
#--------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Retrieve the cached inverse of the matrix
  inv <- x$getInverse()

  # Determine if the inverse has already been computed
  if (!is.null(inv)) {
    # Return the cached inverse back to the calling program instead
    # of re-computing.
    message("getting cached data")
    return(inv)
  }

  # Retrieve the matrix data from the object
  data <- x$get()

  # Compute and store the inverse of the matrix
  inv <- solve(data, ...)
  x$setInverse(inv)

  # Return the computed inverse back to the calling program
  inv
}
