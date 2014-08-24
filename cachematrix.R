## Put comments here that give an overall description of what your
## functions do

# This R script makes use of some scoping concepts along with use of '<<-'
# assignment operator to cache frequently used data.

# In this case, that data is the inverse of a matrix

## Functions used in this script are:
# setMatrix: To set the value of the matrix whose inverse needs to be calculated
# getMatrix: To return the matrix under consideration
# setInverse: To set the matrix inverse value by using solve()
# getInverse: 

## Write a short comment describing this function

# MakeCacheMatrix creates a matrix that provides for its inverse to be cached.
# The matrix and inverse variables are available to cacheSolve through '<<-'

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	setMatrix<-function(y){
		x<<-y
		inverse<<-NULL
	}
	getMatrix<-function() x
	setInverse<-function(inv) inverse<<-inv
	getInverse<-function() inverse
	list(setMatrix=setMatrix,
		getMatrix=getMatrix,
		setInverse=setInverse,
		getInverse=getInverse)

}

## Write a short comment describing this function

# cacheSolve uses the matrix x and inverse variables made globally available
# by the makeCacheMatrix function to cache inverse and improve performance.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
  	if(!is.null(inverse)) {
    	message("Getting the cached data")
    	return(inverse)
  	}
  	data <- x$get()
  	inverse <- solve(data)  ## Calculating the inverse
  	x$setInverse(inverse)
  	inverse
}
