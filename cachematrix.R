## 
## 

## Function makeCacheMatrix() takes as its argument a square invertible matrix 
## and returns an list object containing the matrix in a form that allows 
## the inverse of the matrix to be calculated and then cached 

makeCacheMatrix <- function(x = matrix()) {

	# inv equals NULL means inverse not calculated - initialise to NULL
	inv <- NULL				
	 
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(y) inv <<- y
	getinv <- function() inv
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

## Function cacheSolve() takes as its argument an object created by the 
## makeCacheMatrix() function and returns the inverse of the original matrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
