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
#this step creat a new function called makeCacheMatrix, and assign x to be a matrix.
        m <- NULL
# assign a null value to m in the local environment.
        set <- function(y) {
                x <<- y
                m <<- NULL
#creat another function inside, and set m and x to be cached and can be used in other environments.
        }
        get <- function() x
#assign the original matrix to get.
        setinverse <- function(solve) m <<- solve
#Calculate the inverse of the matrix and assign the value to m.
        getinverse <- function() m
#display the inverse of the matrix.
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
#Creat a new function called cachesolve
        m <- x$getinverse()
#assign the inverse of matrix calculated in the makecachematrix to m,
        if(!is.null(m)) {
# if m is not null, which means m has been calculated.
                message("getting cached data")
                return(m)
# show the messenge and return the value of m.
        }
       
        data <- x$get()
# if m is null, the assign the original matrix to data. 
        m <- solve(data, ...)
# calculate the inverse of the original matrix and assign it to m
        x$setinverse(m)
# assign the m value to the setinverse function in the makecachematrix.
        m
}
        ## Return a matrix that is the inverse of 'x'
}
