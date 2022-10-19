# Programming-Assignment-2
Lexical Scoping
library(MASS)
> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   get <- function() x
+   setinverse <- function(inverse) inv <<- inverse
+   getinverse <- function(){
+     inver <- ginv(x)
+     inver %*% x
+   }
+   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
+ }
> 
> cacheSolve <- function(x, ...) {
+   inv <- x$getinverse()
+   if(!is.null(inv)) {
+     message("getting cached data!")
+     return(inv)
+   }
+   data <- x$get()
+   inv <- solve(data,...)
+   x$setinverse(inv)
+   inv
+ }
> 
> f <- makeCacheMatrix(matrix(1:8, 2, 4))
> f$get()
     [,1] [,2] [,3] [,4]
[1,]    1    3    5    7
[2,]    2    4    6    8
> f$getinverse()
     [,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7
> cacheSolve(f)
getting cached data!
     [,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7
