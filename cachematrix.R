## makeCacheMatrix and cacheSolve are a pair of functions that cache
## the inverse of a matrix.    



## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {    ## set is a function that changes the
                x <<- y         ## matrix stored in the main function.
                inv <<- NULL    ## x is set equal to the input, y.
        }
        
        get <- function() x     ## get is a function that returns the 
                                ## matrix x stored in the main function.
                                ## No input required.
        
        setinverse <- function(mat.inv) inv <<- mat.inv  ## setinverse
                                ## doesn't calculate the inverse; it 
                                ## simply stores the value of the 
                                ## input in a variable 'inv' into the 
                                ## main function, makeCacheMatrix.
        getinverse <- function() inv            ## getinverse returns
                                ## value of the input in a variable 
                                ## 'inv' in makeCacheMatrix.
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)   ## The four functions are stored
                                ## using the function list().  When we 
                                ## assign makeCacheMatrix to an object,
                                ## the object has all four functions.
        
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix using the solve() function.  It is 
## assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Input for cacheSolve is the object where makeCacheMatrix 
        ## is stored.
        
        inv <- x$getinverse()
                
        if (!is.null(inv)) {    ## If inverse has already been 
                                ## calculated, it is returned.
                message("getting cached data")
                return (inv)
        }
        
        data <- x$get()         ## If inverse hasn't been calculated, 
        inv <- solve(data, ...) ## it's calculated here.
        x$setinverse(inv)       ## x$setinverse stores the value 
                                ## generated in makeCacheMatrix.
        inv
        
}
