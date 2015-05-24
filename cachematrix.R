## Below are two functions are created for assignment 2, that are used to: 
##  create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special kind of "matrix", 
## which is really a list containing a function to:
## 1)set the value of the matrix (set_matrix)
## 2)get the value of the matrix (get_matrix)
## 3)set the value of the inverse (set_inverse)
## 4)get the value of the inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
	inverse_Matrix <- NULL 					## Initially assigning 'NULL' to inverse
	set_matrix <- function(y) {			
		x <<- y 					## Setting the matrix 'x'
		inverse_Matrix <<- NULL
	}
	get_matrix <- function() x 				## Returning matrix 'x'
	set_inverse <- function(solve) inverse_Matrix <<- solve 	## Cache the value of the inverse_Matrix 
	get_inverse <- function() inverse_Matrix 			## Returning inverse_Matrix
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the 'set_inverse' function.

cacheSolve <- function(x, ...) {				## Return a matrix that is the inverse of 'x'
	inverse_Matrix <- x$get_inverse()				## Getting inverse
	if(!is.null(inverse_Matrix)) {					## Checking for the presence of inverse
		message("getting cached data")			## Displaying message
		return(inverse_Matrix)
	}
	data <- x$get_matrix()					## Getting Matrix
	inverse_Matrix <- solve(data, ...)				## Using solve() to compute inverse
	x$set_inverse(inverse_Matrix)					## To cache the inverse
	inverse_Matrix 						## Returning the inverse
}