## makeCacheMatrix and cacheSolve functions calculates and cache the Inverse
## of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix()) {
        m <- NULL
        set <- function( y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function( InvMat) m <<- InvMat
        getInverseMatrix <- function() InvMat
        list( set = set,
              get = get,
              setInverseMatrix,
              getInverseMatrix)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieve the inverse 
## from the cache

cacheSolve <- function( x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverseMatrix()
        if( !is.null( invMatrix)) {
                message("getting cached Inversed Matrix")
                return( m)
        }
        matrix <- x$get()
        invMatrix <- solve( matrix)
        x$setInverseMatrix( invMatrix)
        invMatrix
}
