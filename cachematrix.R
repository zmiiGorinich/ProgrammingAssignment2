## Write a short comment describing this function

# Produces a list of functions managing a matrix and its inverse,
# that are stored in the function's closure 
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        if(!is.null(inv) && !identical(x,y)){
            message("Reset inverse matrix")		
            inv <<- NULL
        }
        x <<- y
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

# Computes inverse of a matrix stored in the environment of makeCacheMatrix.
# Returns a stored inverse matrix once it has been calculated,
# until the original matrix is intact

# Warning: ellipces must not include 'b' parameter since when provided,
# it will be used in the a %*% x = b equation in the call to "solve"! 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

testCacheSolve <- function(){
    m <- makeCacheMatrix(matrix(1:4,2,2))
    inv1 <- cacheSolve(m)
    inv2 <- cacheSolve(m)
    print(m$get() %*% inv1)
    print(m$get() %*% inv2)
}
