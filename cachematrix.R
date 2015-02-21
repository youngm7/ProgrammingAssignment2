##The makeCacheMatrix function sets a special matrix that will cache the inverse matrix and define functions for the cacheInverse
##function

##This function sets the inverse matrix to null in case it was previously set upon being run. It then defines functions that will
##be used by cacheInverse to retrieve i if it's already calculated or set i if it is not after calculating.

makeCacheMatrix <- function(x = matrix()) {
        ##set value of matrix (NULL) in case previously set
        i <- NULL
        ##Define set function within makeCachematrix. Will search upwards through 
        ##environments and assign x the value of y. i is also assigned as NULL
        ##in the makeCachematrix environment. y is formally defined.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ##Defines get function that will simply return x.
        get <- function() x
        ##Defines setinverse function that will set i to be the inverse within the
        ##above makeCacheMatrix environment.
        setinverse <- function(inverse) i <<- inverse
        ##Defines getinverse function that will return i.
        getinverse <- function() i
        ##A list of the defined functions and function handles.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function checks for i within the cache. If the value is not null it will simply retrieve and return it. Otherwise
##i is calculated and cached for future use.

cacheInverse <- function(x, ...) {
        ##If i is already defined; if i is not(!) null returns cached i and leaves
        ##function.
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##Otherwise retrieves matrix and assigns it to data and takes the inverse of 
        ##the data locally assigning it to the value i.
        data <- x$get()
        i <- solve(data, ...)
        ##Then caches this value by assigning the calculated inverse to i and 
        ##returning i.
        x$setinverse(i)
        i
}
