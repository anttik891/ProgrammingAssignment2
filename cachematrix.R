## This function takes matrix as a argument and returns a "matrix" object
## with list of list of functions as a return type. Those functions can
## get the matrix and its inverse as well as set a new value to the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y){ 
                x <<- y
                m <<- null
        }
        get <- function(){ 
                x
        }
        setInverse <- function (inverse){
                m <<- inverse
        }
        getInverse <- function(){
                m
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function takes a "matrix" object created in the above function as a parameter
## And returns its cached inverse, if it's not yet inversed this function will inverse it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() #get inversed matrix
        if (!is.null(m)){
                
                message("Getting cached data")
                return (m)
        }
        #if there is no inversed matrix to cache
        data <- x$get()
        m <- solve(x$get(), ...)
        x$setInverse(m)
        m
}
