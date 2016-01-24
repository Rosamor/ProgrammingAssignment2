## MakeCaheMatrix has 4 elements: 
        ## put a matrix, 
        ## get the matrix that all ready has, 
        ## put the inverse of the matrix 
        ## get the inverse of the matrix 
        ## if it doesn't have it it will return "NULL"

makeCacheMatrix makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(newMatrix) {
                x <<- newMatrix
                inv <<- NULL
        }
        get <-function() x
        setInverse <-function(inverse) inv<<-inverse
        getInverse <-function() inv
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)

    ## Return a matrix that is the inverse of 'x'

    inv
}
