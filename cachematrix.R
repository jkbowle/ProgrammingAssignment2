## Two functions created here:
## 1) makeCacheMatrix: to create a wrapper arounda  matrix object that can cache inverse
##                     results
## 2) cacheSolve:  To create a function that will take in the matrix wrapper and get 
##                the inverse of the matrix that is stored



## makeCacheMatrix:  This function has an internally stored inverse of the matrix for which
##                    this function wraps.. (stored as i, original matrix stored as m)
##          
##                   creating 4 functions: set (set the matrix), get (get the matrix), 
##                                        setinverse (set the inverse of the given matrix),
##                                        getinverse (get the inverse of the given matrix)
##
##  This code functions much like a class would function in Java or Python

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
      m <<- y
      i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve:  This function takes as an argument the matrix wrapper defined above
##              this wrapper stores the matrix and the inverse result
##              if the inverse is set to null than the inverse for this matrix has never been
##              calculated before
##         
##              FYI:  inverse is reset to NULL when a new value is set for the matrix

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'
    i <- m$getinverse()
    if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- m$get()
    i <- solve(data, ...)
    m$setinverse(i)
    i
}
