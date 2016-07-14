## Matrix iversion is unually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. The following two 
## functions are used to caching the inverse of a matrix.

## Creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Computes the inverse of the special "matrix" created with the above function
## This function assumes that the matrix is always invertible 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
