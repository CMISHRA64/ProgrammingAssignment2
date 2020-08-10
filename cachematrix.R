## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL                
        }
        get = function() x
        setInv = function(inverse) inv <<- inverse
        getInv = function() inv
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv = x$getInv()
        if (!is.null(inv)) {
                message("getting data from Cache")
                return(inv)
        }
        mat.data = x$get()
        
        inv = solve(mat.data, ...)
        
        x$setInv(inv)
        
        return(inv)
}

