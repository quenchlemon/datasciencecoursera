## Caching the Matrix's inverse Programming assignment 2

## Tests
## test_matrix <- matrix (1:4, 2, 2)
## x <- makeCacheMatrix(test_matrix)
## which returns the output matrix(c(-2, 1, 1.5, -0.5), 2, 2)

## test2 <- matrix(1:4*2, 2, 2)
## cacheSolve(makeCacheMatrix(test2))
## which returns the output matrix(c(-1, 0.5, 0.75, -0.25), 2, 2)


makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special matrix capable of caching its inverse
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) i <<- solve
    get_inverse <- function() i
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
    ## Function computes the inverse of the special matrix created by
    ## makeCacheMatrix function. If the inverse has already been calculated,
    ## it should retrieve the inverse from the cache
    i <- x$get_inverse()
    if(!is.null(i)) {
            message("Getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
}
