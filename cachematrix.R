## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() will save the matrix as well the value of its inverse
## cacheSolve() will compute the inverse if is has not already been cached and call 
## setInverse() to cache the inverse.

##makeCacheMatrix() takes as input a matrix. setInverse() sets the 
## class variable inv to the specified value. getInverse() retrieves 
## previously computed and cached matrix inverse value.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve() will take as input a matrix created by makeCacheMatrix and check if 
## the matrix has already been computed and cached. If it is not, then the inverse is calculated
## and cached using the setInverse() function for future inverse computations.

cacheSolve <- function(x, ...) {
       
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(inv)
        inv
}
