## R Programming - Assignment to create a special matrix 
## It has two functions makeCacheMatrix() and cacheSolve()
## More detail on these functions are given below

## This function creates a special matrix with a list of four different functions
## 1. (first element) set function sets the value of the matrix
## 2. (second element)get function can be used to get the values of the matrix
## 3. (third element) sets the inverse of the matrix
## 4. (fourth element) gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ##set x = to the matrix that is input to the function
        x <<- y
        m <<- NULL
    }
    get <- function() x ## to display the matrix
    setInverse <- function(solve) m <<- solve ## to calculate inverse of the matrix
    getInverse <- function() m ## to display the inverse of the matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) ## setting the elements in order
}


## This function checks if the inverse of the matrix created using makeCacheMatrix()
## exists or not
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) { ## condition to check if inverse has already been calculated
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
