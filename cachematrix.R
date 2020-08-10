## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are advantages in caching the inverse of a matrix rather
## than compute it repeatedly.
## The following two functions are used to create a special object
## which stores a matrix and caches its inverse.

## The following function creates a special "matrix" object which can cache its inverse.


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


## The following function gives the inverse of the "matrix" created by 
## makeCacheMatrix above.
## If the inverse has already been computed and the 
## matrix elemnts have not changed, then it will extract the inverse from the Cache.


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

