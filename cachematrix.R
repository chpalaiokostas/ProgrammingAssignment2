# prepare cache matrix object
# a matrix should be provided
makeCacheMatrix <- function(x=numeric()) {
	if(class(x)!='matrix') {
		return("Need a matrix")
	}
	mat_inv <- NULL
	set <- function(y) {
		x<<- y
		mat_inv <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) mat_inv <<- inverse
	get_inverse <- function() mat_inv
	list(set=set,get=get,set_inverse=set_inverse,
			get_inverse=get_inverse)
}

# check if inverse matrix already available
# compute inverse if not available
# cannot handle non invertible matrices
cacheSolve <- function(x,...) {
	mat_inv <- x$get_inverse()
	if(!is.null(mat_inv)) {
		print("getting cached data")
		return(mat_inv)
	}
	data <- x$get()
	mat_inv <- solve(data)
	x$set_inverse(mat_inv)
	mat_inv
}

