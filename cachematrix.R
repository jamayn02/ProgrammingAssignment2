## These two functions below will (1) create a matrix that 
## can cache its inverse and (2) compute the inverse of the
## matrix from the first function, or if it has already been
## caclulated, return the inverse from the cache.

## This function creates a "special" matrix that can cache its
## when called

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ##initial inverse matrix set to null
        
        ## This function will set the matrix to a new value and 
        ## set inv variable back to null
        set <- function(y) {
                x <<- y      
                inv <<- NULL
        }
        
        ## This function returns the matrix
        get <- function() x
        
        ## This function will set inverse to a variable called 
        ## "inver" which will be used in the second function 
        ## "cacheSolve" of the file
        setinv <- function(inver) inv <<- inver
        
        ## This function will return the variable "inv"
        getinv <- function() inv
        
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will compute the inverse of the matrix created by the
## function "makeCacheMatrix".  If inverse has already been calculated
## and no change has been made to the matrix, the the function will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() ## "inv" variable set to result of "getinv()"
               
        ## If statement to determine if inverse matrix can be retrieved
        ## from the cache.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## "data" variable set to result of "get()"
        inv <- solve(data) ## Inverse of matrix calculated
        x$setinv(inv) ## Sets inverse to variable "inv"
        inv ## Return a matrix that is the inverse of 'x'
}
