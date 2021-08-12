## This function creates a matrix object and can cahe its inverse 

##Assumptions: the matrix supplied is invertible 

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        I <- function(d)
        {
                x <<- d
                inv <<- NULL
        }

        getM <- function() x 


        setI <- function(AI) inv <<- AI


        getI <- function() inv


        list ( I = I, getM = getM, setI = setI, getI = getI)

}


## This function computes the inverse of the matrix created in the makeCacheMatrix function,
## If the inverse has been calculated and hasn't changed, the function retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getI()
        if (!is.null(inv))
        {
                message("Getting inverse from cache")
                return(inv)
        }
        
        data <- x$getM()

        inv <- solve(data,...)

        x$setI(inv)

        inv

}
