## Functions (makeCacheMatrix and cacheSolve) are able to cache computations
## that are time-intensive to do. If contents are recurring, it makes sense to
## set the value and look up from cache instead of performing repeated
## computations.

## makeCacheMatrix creates functions that get or set matrix value and then
## creates and stores the inverse value in cache
makeCacheMatrix <- function(x = matrix()) {

        ## set cache to null
        i <- NULL

        ## set - stores cached value
        set <- function(y) {
            x <<- y
            i <<- NULL
        }

        ## get - returns matrix value
        get <- function() x

        ## setInverse - inverts matrix and stores in cache
        setInverse <- function(inverse) i <<- inverse

        ## getInverse - gets inverted matrix from cache
        getInverse <- function() i

        ## list - returns functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse
        )
}


## cacheSolve displays the inverse of matrix created by makeCacheMatrix function.
## If inverse of the matrix has been created, then cacheSolve will return
## cached value
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
              return(i)
        }
        i <- solve(x$get())
        x$setInverse(i)
        i
}

## tested with some base values
## source code into R
## M <- makeCacheMatrix()
## M$set(matrix(1:4,2,2))
## cacheSolve(M)
## returns following cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
