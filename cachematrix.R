# A pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse
# which is really a list containing a function to: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        # set the 'x' variable enter as parameter in the makeCacheMatrix function
        # so it will be independent of lexiacl scope where the set function
        # will be called
        x <<- y
        # set the 'inverse' variable defined in the makeCacheMatrix function
        # so will be independet of lexical scope where the set function 
        # will be called
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) 
        # set the 'inverse' variable defined in the makeCacheMatrix function
        # so will be independet of lexical scope where the setinverse function 
        # will be called
        inverse <<- inv
    getinverse <- function() 
        inverse

    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise, 
# it calculates the inverse of the data and sets the value of the inverse in the cache via 
# the setinverse function.
# Computing the inverse of a square matrix is done with the solve function in R.
# The further arguments are passed to the solve function.
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    # check to see if the matrix inverse has already been calculated
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        # if it exists then return it
        message("getting cached data")
        return(inverse)
    }
    # if it does not exist
    
    # get the matrix in data
    data <- x$get()
    # compute the matrix inverse
    inverse <- solve(a=data,b=, ...)
    # save the matrix inverse
    x$setinverse(inverse)
    # return the matrix inverse
    inverse
}
