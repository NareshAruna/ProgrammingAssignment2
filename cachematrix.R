## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an Object which creates a Matrix and Inverse of 
## that matrix
## It has four function set, get, setInverse, getInverse, which is used to 
## set the inputmatrix to variable x, fetch the input matrix from variable 
## x, set the inverse of inputmatrix to variable inverse, get the the inverse
## of matrix (inverse) and return to calling method respectively.
## values of x, inverse are cached as <<- assignment is used. The values 
## returned by these methods are stored to a list

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
	    x <<- y
	    inverse <<- NULL
	}
	
	get <- function() {
	    x
	}
	
	setInverse <- function(inv) {
	    inverse <<- inv
	}
	
	getInverse <- function() {
	    inverse
	}
	
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## When this function is called and a matrix is given as input the inverse
## of that matrix is returned. This tries to retrive the inverse of matrix 
## from cache. If it present in cache it prints "Fetching Cached Data"
## and returns the inverse value. If value is null, it computes the inverse
## and sets it to the inverse variable in that Object and returns the inverse
## value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        if(!is.null(inverse)) {
          print("Fetching Cached Data")
          return (inverse)
        }
        
        inData <- x$get()
        inverse <- solve(inData, ...)
        x$setInverse(inverse)
        inverse
}
