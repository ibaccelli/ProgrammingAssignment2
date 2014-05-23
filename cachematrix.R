## Here is my "makeCacheMatrix" function: it creates an object that is a matrix
## and then it caches the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
        # if an object is called with a method
                inv <- NULL  
                set <- function(y) {
                        x <<- y
                        inv <<- NULL # the inverse of x is set to NULL and is cached
                }
                #  the makeCacheMatrix function has four subfunctions that can be called to work on
                get <- function() x # a function that gets the original matrix x
                setinv <- function(solve) inv <<- solve 
                #a function that calculates the inverse of matrix x and chaches it
                getinv <- function() inv # a function that gets the cached inverse of x and returns it
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
                # makeCacheMatrix returns its 4 subfunctions as a list
}


## Here is my "cacheSlove" function: it computes the inverse of the matrix 
## returned by makeCacheMatrix above. If the inverse has already been calculated,
## then the cacheSolve should retrieve the inverse from the cache.
## Otherwise, it calculates it from scratch.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() # the inverse is to be taken from the getinv function
        if(!is.null(inv)) { #if the value of the inverse of the matrix above is not NULL
                message("getting cached data") #declare that the inverse is taken from the cached data
                return(inv) # and return the value of the inverse without recalculating it
        }
        data <- x$get() # otherwise, get the matrix x
        inv <- solve(data, ...) # calculate its inverse
        x$setinv() # set the value of the inverse of x
        inv # return inv (the inverse of the matrix x)
}

## This is a little example to test that my code is working:
x<-rbind(c(1, -1/4), c(-1/4, 1)) 
xx <- makeCacheMatrix(x)
xx$get()
xx$getinv()
# here it should return NULL, as the inverse has not yet been set
xx$setinv(solve(x))
xx$getinv()
# now that the inverse has been set, the inverse of the matrix will be returned
cacheSolve(xx)
# as the inverse has already been calculated, cacheSolve will retrieve
# the cached inverse without recalculating it.
