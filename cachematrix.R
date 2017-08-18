
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. At a high level, this code consists of pair of functions, makeCacheMatrix and cacheSolve, that cache the inverse of a matrix.
## makeCacheMatrix is a function creates a special "matrix" object (of type makeCachMatrix) that can cache its inverse.
## cacheSolve is a function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the 
## cache.

## makeCacheMatrix takes an argument of type matrix.
## The function builds 4 other functions and returns those 4 functions (getters and setters) 
## within a list to the parent environment. 
## Along with the 4 functions, the list also contains 
## variables x and inv, which are objects of type matrix.  x is passed into the function as an arg
## and initialized as a default arg to avoid throwing an error if no arg is passed
## 
makeCacheMatrix <- function(x = matrix()) {
       
        #initialize the matrix var inv
        inv <- NULL
        
        # establish 4 functions (getters and setters) to be returned in the list object created below
        # 
        set <- function(y) {                            # when called, receives the matrix y as an arg,
                x <<- y                                 # and assigns it to x var in parent env ("sets" x in parent env)
                inv <<- NULL                            # when x is set or reset, we set inv to NULL (indicating value is not cached)
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse # when called, receives the inverse matrix as an arg, 
                                                        # and assigns it to inv var in parent env ("sets" inv in parent env) 

        getinverse <- function() inv                    # returns the inverse matrix ("gets" inv from the parent env)

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## cacheSolve is required to calculate/populate or retrieve the inverse matrix from an object of type makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # attempt to retrieve the inverse matrix from the object passed in as an arg        
        inv <- x$getinverse()
        
        # if the value is not NULL, that means the value is cached, and we have a valid matrix, so return that value
        if(!is.null(inv)) {
                message("getting cached data")
                # return the value of inv and terminate the function 
                return(inv)
        }
        # if the value is NULL, the value has not been cached, so we need to get x from the object
        # calculate inverse matrix using the solve function
        # set the inverse in the object
        # and return the inverse (so inverse will no longer be NULL)
        # 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
##-------------------------------------------------execute with test data
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))         # create the object by sending a 2x2 matrix 
my_matrix$get()                                         # show the matrix using get
my_matrix$getinverse()                                  # show that inv is initially set to NULL
cacheSolve(my_matrix)                                   # Use cacheSolve to retrieve and calculate the inverse
cacheSolve(my_matrix)                                   # Use cacheSolve again, this time retrieving the previously calc inverse from cache
my_matrix$getinverse()                                  # show that the inv is no longer NULL since it was cached
