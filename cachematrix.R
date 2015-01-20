## Overall Description:
##      makeCacheMatrix creates a matrix that caches it's inverse.
##      cacheSolve computes the inverse of the matrix. If the inverse has 
##      already been cached, it will not compute but return the cached 
##      matrix instead.


## Function:
##      makeCacheMatrix( x=matrix() )
##
## Desciption: 
##      Used to create a special matrix object that caches it's inverse.
##      
## Example: 
##      A <- matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3,byrow=TRUE)
##      M <- makeCacheMatrix(A)

makeCacheMatrix <- function( x = matrix() ) {     
        ## matrix object's attributes
        inverse <- NULL
        
        ## matrix object's functions
        set_matrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(new_inverse) inverse <<- new_inverse
        get_inverse <- function() inverse
        
        ## returns a list of matrix object's functions
        list(set_matrix = set_matrix,
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## Function:
##      cacheSolve(x, ...)
##
## Desciption: 
##      Used to compute the inverse of a matrix returned by makeCacheMatrix
##      above. If the inverse has been computed (and the matrix has not 
##      changed), then cacheSolve will return the inverse from the cache.
##      
## Example: 
##      cacheSolve(M)   # first time, will compute and store cached inverse mx
##      cacheSolve(M)   # second time, will return the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## if cached inverse is found, return cached matrix directly
        i <- x$get_inverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        
        ## else retrieve matrix value, solve for inverse, store in cache
        m <- x$get_matrix()
        i <- solve(m, ...)
        x$set_inverse(i)
        
        ## returns inverse of matrix
        i
}
