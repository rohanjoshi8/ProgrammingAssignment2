## makeCacheMatrix is a list of functions to set a matrix, extract a matrix, set the inverse of a matrix and to get the inverse of a matrix.
## cacheSolve is a fucntion that calculates the inverse of a matrix if it does not exist, else it extract the inverse from the cache.


## makeCacheMatrix is a list of functions to set a matrix, extract a matrix, set the inverse of a matrix and to get the inverse of a matrix.
##  Usage -> Make a variable that holds makeCacheMatrix.
## Access the functions within makeCacheMatrix by using the variable: E.g. a$set

makeCacheMatrix <- function(x = numeric()) {

        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
        
        
}

##  cacheSolve finds the inverse of the matrix. 
##  If inverse is present in cache it is extracted, else a new inverse is calculated,

cacheSolve <- function(x, ...) {

        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}
