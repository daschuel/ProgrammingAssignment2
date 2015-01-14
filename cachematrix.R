## This function, as per the assignment, creates a special matrix object
## which, if it has been calculated, contains a record of its own inverse

## This function makes the cacheable matrix out of an ordinary one.

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


## This checks a cachable matrix as defined by MakeCacheMatrix and returns its invers,
## from the cache if applicable.

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
