## This function creates a special "newobject" matrix that can cache its inverse.

makeCacheMatrix <- function(j = object()) {
        sol <- NULL
        set <- function(k) {
                j <<- k
                sol <<- NULL
        }
        get <- function() j
        setInverse <- function(solveobject) sol <<- solveobject
        getInverse <- function() sol
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## It computes the inverse of "newobject" created by makeCacheMatrix. 
## If the inverse is already calculated (and the matrix has not changed), then it will retrieve the inverse from cache.

cacheSolve <- function(j, ...) {
        ## Return a matrix that is the inverse of 'j'
        sol <- j$getInverse()
        if (!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        obj <- j$get()
        sol <- solve(obj, ...)
        j$setInverse(sol)
        sol
}