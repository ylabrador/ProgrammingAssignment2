##  This function creates a special "matrix" object that caches its inverse.
##  The return is a list with the followings methods: 
##  get: returns the param x
##  set: change the value of the x matrix
##  getinverse: return a matrix that is the inverse of 'x' (only it is created with the method cacheSolve)
##  setinverse: change the value of the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse_matrix <<- inv
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  This function computes the inverse of the special "matrix". 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
