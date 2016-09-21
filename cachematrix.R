## Put comments here that give an overall description of what your
## functions do

# Extra reference: https://www.r-bloggers.com/using-closures-as-objects-in-r/

# we will use use ppor man object approach to encapsulate values in a scope
# using a list of function to interact with these values to use a cache functionality with
# matrix

## Write a short comment describing this function

# we create a function that will be returning a list of functions
# these functions are definied inside the scope of the main function
# in which we will be setting our values inverse and x

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_val) inverse <<- inverse_val
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# this function uses the list of function to access the values defined inside
# the scope of the call to the function makeCacheMatrix. Doing so we can use the
# function as interface for the values. We return the inverse matrix, calculating it if
# it wasn't done before or getting the saved value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
