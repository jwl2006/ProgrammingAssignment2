## The function I am creating is to cache the inverse of the matrix. Basically the first function makeCacheMatrix is 
   #to creat a function that can get the matrix, calculate the inverse of the matrix using "solve" and cache the inverse of the matrix.
## The second function cacheSolve is to see whether we have calculated the inverse of matrix or not, if we have calculate it
#  in the first function, because we have cached it, so we do not need to calculate it again, and use the calculated inverse
#  directly. If not been calculated, we will calculate it.

## Comments: the first function we created is makeCacheMatrix. The function first set variable m to null. 
#  Inside the function, we created another function set, which will cache the m and x, so we can use m and x in a different environment
#  such as we can use it in cacheSolve function. 
#  Next, we get the matrix we input and assign it to get. The setinverse is a function then calculate the inverse of matrix by
#  the solve function in R, then the inverse of the matrix is saved in m. Then we can display the inverse by function()m and assign
# it to a variable called getinverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


}


## The cacheSolve function is used to see whether the inverse of the matrix has been calculated or not by cacheing m.
#  If it has been calculated, there will be a messenge "getting cached data", and the inverse is displayed by return(m).
# if the inverse has not been calculated at all, it will get the matrix from x, and calculate the inverse by using solve and
# assign it to m.
# 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
}
