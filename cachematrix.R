##This function creates a special matrix

makeVector <- function(x = matrix()) {
        ## Initialization of variable to inverse matrix
        inv <- NULL

        ## Set the matrix
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }

        ## Get the matrix
        get <- function() {
          m
        }

        ## Set de inverse matrix
        setInverse <- function(inverse){
          inv <<- inverse
        }

        ##Get de inverse matrix
        getInverse <- function() {
          inv
        }

        ##Return the list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix
cachemean <- function(x, ...) {
        m <- x$getInverse()

        ##verify if the inverse matrix is different of null and return the matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ##Get the matrix will be inverted
        data <- x$get()

        ##Calculate of the inverse matrix
        m <- solve(data, ...)

        ##Set the inverse matrix
        x$setInverse(m)

        ##Return the inverse matrix
        m
}

