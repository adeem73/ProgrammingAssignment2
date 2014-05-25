## By: Adeem Malik
## The first function, "makeCacheMatrix()" returns a list of 4 functions, for setting and getting a matrix as well as 
## setting and getting the inverse of the matrix. The second function, "cacheSolve(x,...)" first checks if the inverse
## already exists in the cache, if so, it will not compute inverse again and will return the cached inverse.

## This function returns a list of 4 functions, for setting and getting a matrix as well as 
## setting and getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        
        x_inv <- matrix()
        
        setmatrix <- function(y) {
                x <<- y
        }
        
        getmatrix <- function() x
        
        setinverse <- function(y) {
                x_inv <<- solve(y)
        }
        
        getinverse <- function() x_inv
        
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## The function first checks if the inverse
## already exists in the cache, if so, it will not compute inverse again and will return the cached inverse.
## a null matrix will be returned if no matrix is 'set', however if a matrix is 'set' using the "setmatrix()"
## function, running "cacheSolve()" will return inverse - inverse will be computed if it doesn't already exist
## in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()

        if(!is.na(inv[1,1])) {
                message("getting cached inverse of matrix..")

                return(inv)
        }
        
        matrix <- x$getmatrix()
        
        inv <- x$setinverse(matrix)
        
        inv
}
