## The following functions take a give square matrix
## that is invertible and store its inverse calculation
## in cache memory, so that next time you want to calculate
## its inverse it comes out faster from cache
## than calculate again, assuming the matrix
## didn't change

## makeCacheMatrix takes a matrix and
## makes it a 'special' matrix so that
## its inverse calculation can be eventually
## saved in cache
## it outputs a list of:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # this is an extra step I added 
        #if matrix not square then send a warning message to user
        if (ncol(x)!=nrow(x)) {
                print("matrix not square hence not invertible")
        } else {
                #else proceed with the function
                #below I replaced all 'mean' with 'solve'
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}
}

## The cacheSolve function below
## calculates the inverse of the special matrix
## that was created in the above function
## however, it first checks if the inverse was
## already computed; if this is the case
## it reads it from the cache than computing it
## if not, it calculates it and saves it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #call same functions as stated in makeCacheMatrix function
        #if not null go to cache
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #otherwise compute
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
