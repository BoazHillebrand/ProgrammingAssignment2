## makeCacheMatrix creates the inverse (solution) of a matrix, for later retrieval. 
## cacheSolve retrieves the inverse if available, or creates a new inverse.

## Cache the inverse solution of a matrix

makeCacheMatrix <- function(x = matrix()) {
        mat.invert <- NULL
        set <- function(y){
                x <<- y
                mat.invert <<- NULL
        }
        get <- function()x
        set.inverse <- function(inverse)mat.invert <<- inverse
        get.inverse <- function()mat.invert
        list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## Get the cached solution for a matrix if it exists, or create it otherwise.

cacheSolve <- function(x, ...) {
        mat.invert <- x$get.inverse()
        if(!is.null(mat.invert)){
                return(mat.invert)
        }
        data <- x$get()
        mat.invert <- solve(data,...)
        x$set.inverse(mat.invert)
        mat.invert
}
