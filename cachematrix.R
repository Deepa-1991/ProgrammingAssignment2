## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function returns the list of functions 
## invmat is inverse of input matrix
##get is a function which returns the value stored in x 
##setmat takes a inverse matrix as input and set the value to the object invmat
##getmat returns the value stored in invmat(inverse matrix)
## When makeCacheMatrix is invoked, it returns the list of functions i.e set,get,setmat,getmat)


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
## cacheSolve takes function makeCacheMatrix as input and checks if the inverse is already cached for the input matrix, if not it will calculate the inverse and cache the value to invmat
## getmat() function gets the inverse matrix if its already cached
## if output of getmat() is not null then inverse of matrix(invmat) is returned from the cache and control exits from the function
##if output of getmat() is null then get() returns the input matrix which is stored to the object 'data'
##solve function returns the inverse of 'data' which is stored in the object 'invmat'
## setmat function is used set the value of 'invmat' into cache

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
