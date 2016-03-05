##Caching the inverse of a matrix.

## This function creates the matrix through the same protocol as makeVector. It sets and gets the value of the vector and then sets and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inv <- function(inverse) m <<- inverse
	get_inv <- function() m
	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## The second function calculates the inverse of the matrix created in the makeCacheMatrix function using the solve() function. However, in order to save time it first checks that there is not already a calculated value available. If the answer is no, the function will calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$set_inv(m)
    m
}
