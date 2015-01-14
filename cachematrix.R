## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        the_inv <- NULL
        set <- function(y) {
                x <<- y
                the_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) the_inv <<- solve
        getinv <- function() the_inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        the_inv <- x$getinv()
        if(!is.null(the_inv)) {
                message("getting cached data")
                return(the_inv)
        }
        the_matrix <- x$get()
        the_inv <- solve(the_matrix)
        x$setinv(the_inv)
        the_inv
}
