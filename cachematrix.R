## makeCacheMatrix creates a matrix that can cache it's inverse and returns
## a list of functions to perform various functions on the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## cacheSolve tests to see if the inverse of the matrix input has been computed
## and if not returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        y <- x$getinv()
        if(!is.null(y)){
                message("retrieving inverse")
                return(y)
        }
        z = x$get()
        y = solve(z)
        x$setinv(y)
        y
}
