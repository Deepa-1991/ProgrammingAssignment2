## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invmat<-NULL
set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setmat <- function(mat) invmat <<- mat
        getmat <- function() invmat
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        invmat <- x$getmat()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }

        data <- x$get()
        invmat <- solve(data)
        x$setmat(invmat)
        invmat
}
