## Use the functions to cache the value of matrix and its inverse, so that repeated caculation is not needed 
## when we need the inverse of a caculated matrix in the second function.
## We can test these functions with the following code:
## > x <- matrix(rnorm(25), nrow = 5)          // Create a matrix x
## > cx <- makeCacheMatrix(x)                  // Create our special matrix
## > cx$get()                                  // Return the matrix
## > cacheSolve(cx)                            // Return the inverse
## > cacheSolve(cx)                            // Call the 2nd time, so return

## The first function is used to set the value of the matrix and get its value if need.
## Then the inverse of a matrix, if caculated, is stored. We can get it use the function getinverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The following function is used to caculate the inverse of matrix if it is not caculated yet.
## If the inverse value has been stored, this function can be used to get its value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
