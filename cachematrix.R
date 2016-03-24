## Assignment 2-R Programming
## This Project is used to cache and compute inverse matrix

## This function uses lexcial scopinng to create a list

makeCacheMatrix <- function(x = matrix()) {
        inverse_mrx <- NULL
        
        set <- function(mrx){
                x <<- mrx
                inverse_mrx <<- NULL
        } 
        get <- function() x
        setInverse <- function(inverse) inverse_mrx <<- inverse
        getInverse <- function() inverse_mrx
                list(set = set,get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## This function is used to check,compute and set Inverse Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ##compute the inverse matrix and set the inverse value
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
