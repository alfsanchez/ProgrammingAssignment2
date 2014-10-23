## The two functions below aim to facilitate saving runtime by caching
## the time-consuming process of computing for a matrix inverse.
## More specific information about the functions are in the comments 
## below. Please note that the assumption of these functions is that
## the provided matrix is invertible.


## The makeCacheMatrix function below creates a list of four functions
## that do the following (in this particular order):
## 	1. Set the matrix
##	2. Get the matrix
##	3. Set the inverse of the matrix
##	4. Get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function below checks if the inverse has already been computed
## for the given matrix. If the inverse has already been computed, it
## will get and print the cached inverse. A message is printed out as
## well to indicate that the printed inverse is from cached information.
## If the inverse has not been computed, it will solve and provide the
## inverse of the stated matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
