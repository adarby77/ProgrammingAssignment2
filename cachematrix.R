## These functions are used calculate the inverse of a given matrix. 
## The inverse is computed with result being stored such that 
## on subsequent calls to calculate the inverse the cached result is 
## returned instead of repeating the computation.  
##
## Example usage
##
## > A <- matrix(c(1, 4, 6, -1), ncol=2, nrow=2, byrow=TRUE)
## > cA <- makeCacheMatrix(A)
## > AI <- cacheSolve(cA)


## Function makeCacheMatrix() takes as its argument a square invertible matrix.
## It returns an list object containing functions to store/retrieve 
## the matrix and to allow the inverse of that matrix to be 
## cached for later use.

makeCacheMatrix <- function(x = matrix()) {

	## inv == NULL means inverse has not been calculated. 
    ## Initialise to NULL.
	inv <- NULL				
	 
	## The set() function allows the list object matrix to be 
	## replaced/updated with different matrix passed as 
	## the argument to the function.  Resets the inverse to NULL.
	set <- function(y) {
		x <<- y
		inv <<- NULL	
	}

	## The get() function returns the stored matrix
	get <- function() x

	## The setinv() function stores the matrix inverse passed as 
    ## the argument to the function
	setinv <- function(y) inv <<- y

	## The getinv() function returns the stored matrix inverse 
	getinv <- function() inv

	## Build and return list object containing functions
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

## Function cacheSolve() takes as its argument an object created by the 
## makeCacheMatrix() function and returns the inverse of the original matrix

cacheSolve <- function(x, ...) {

	# Retrieve matrix inverse in the list object
	inv <- x$getinv()

	## Check if the inverse matrix is NULL
	if(!is.null(inv)) {
		## Not NULL - therefore return the cached/stored matrix inverse
		return(inv)
	}

	## Inverse is NULL therefore we must calculate it.
	## Retrieve stored matrix
	data <- x$get()

	## The R Solve() function with a single matrix argument returns 
	## the inverse of that matrix
	inv <- solve(data, ...)

	## Store the Inverse in the list object
	x$setinv(inv)

	## Return the calculated matrix inverse
	inv
}
