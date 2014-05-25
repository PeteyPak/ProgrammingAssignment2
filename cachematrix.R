## This assignment is an example of caching data with some functions.
## NOTE!
## R uses static scoping, so there are just one set of x and i,
##  not multiple sets for multiple objects.  It's like using the
##  static keyword in C or C++.


## Returns a list of four helper functions to get and set two
##  locally-scoped variables (x and i), which are a matrix, and
##  its inverse.  Setting a new matrix clears the cached inverse.
## This list of functions are treated as a special data structure;
##  a "CacheMatrix".  The original matrix can be get and set, as
##  well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # The scope of this function has two variables (and four functions)
    # x is defined - it arrives in a function argument.
    # i is below.
	i <- NULL
    
    # Set a new matrix to cache.  Clears any cached inverse.
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
    
    # Gets the cached matrix
	get <- function() x
    
    # Set a value for the inverse (calculated elsewhere)
	setinverse <- function(inverse) i <<- inverse
    
    # Get the inverse (or whatever you gave to setinverse())
	getinverse <- function() i
    
    # Return a (named) list of the four functions defined here, which
    #  collectively give you access to the two stored variables.
	list(set = set,
         get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}



## cacheSolve takes a CacheMatrix and attempts to access the inverse.
## If the inverse does not exist (hasn't been previously cached),
##  then it calculates the invers and caches it for next time.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'.

    # Is it already cached?
    # If yes, return the cached value
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

    # If we got this far, it's not cached.
    # Calculate, cache, and return.
	message("calculating and caching new inverse")
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
