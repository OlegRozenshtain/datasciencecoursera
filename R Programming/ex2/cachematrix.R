# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# taking advantage of the scoping rules of the R language create a structure
# which allows to cache inverse matrices and pull them from cache instead of
# recalculating them.


# this function creates a special "matrix" object that can cache its the input
# matrix and its inverse.
# output: a list of 4 functions that allows to manipulate the special "matrix"
# object in its own environment.
makeCacheMatrix <- function(x = matrix()) 
{
    invM <- NULL
    # when setting a new base matrix, initialize the inverse matrix to avoid
    # inconsistency
    set <- function(y)
    {
        # assign a value to an object in an environment that is different from
        # the current environment
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInvM <- function(inv) invM <<- inv
    getInvM <- function() invM
    
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.
cacheSolve <- function(x, ...)
{
    
    invM <- x$getInvM()
    # if invM is null one of two options:
    #   1. inverse matrix wasn't calculated yet.
    #   2. base matrix was changed.
    if(!is.null(invM)) 
    {
        message("getting cached data")
        return(invM)
    }
    
    matrix <- x$get()
    invM <- solve(matrix)   # calculate inverse matrix
    x$setInvM(invM)
    invM
}