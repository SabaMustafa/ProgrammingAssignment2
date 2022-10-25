# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matri

makeCacheMatrix <- function(x = matrix()) {
         myInv<-NULL
         set<-function(y)
         {
           x <<- y
           myInv <<-NULL
         }
         get<- function() x
         setinverse <-function(inverse) myInv <<- inverse
        getinverse <-function() myInv
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


# This function returns the inverse of the matrix. First it will checks if
# the inverse has already been computed. it gives the result and skips the
# computation. If not computed , , sets the value in the cache by using setinverse function.

cacheSolve <- function(x, ...) {
          myInv <- x$getinverse()
          if(!is.null(myInv)) {
            message("getting cached data")
            return(myInv)
          }
          data <- x$get()
          myInv <- solve(data, ...)
          x$setinverse(myInv)
          myInv
}
