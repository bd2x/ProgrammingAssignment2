## Put comments here that give an overall description of what your
## functions do

## Constructor function to create matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
        
}


## Caching function retrieves cached matrix inverse if already available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        m
        
}
