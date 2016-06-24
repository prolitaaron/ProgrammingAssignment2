## Creates matrix-like object with basic get/set functionality for a matrix, and get/set matrix inverse functionality
## to store this data without needing to recalculate at every use

## "Vector" with getinverse/setinverse functionality to cache inverse value

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solved) m <<- solved
	getinverse <- function() m
	list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## Retrieves cached data if available, otherwise generates inverse then caches and returns it

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
}
