##
## Johns Hopkins University
## R Programming - Apr 7 to May 5, 2014
##
## Assignment 2 - Submission due on Sun 27 Apr 11:59 pm
##
## Forked on Apr 23, 2014
## From original 
##    https://github.com/rdpeng/ProgrammingAssignment2
## To this copy
##    https://github.com/rslinford/ProgrammingAssignment2.git

#  Creates a "matrix" object that can cache its inverse. Returns a
#  list of getter and setter functions.
#
#     set(m) - sets the matrix data
#     get()  - returns the matrix dtaa
#
#     setinverse(inverse) - caches the provided inverse
#     getinverse()        - returns the cached inverse
#
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL

   get <- function() x
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }

   getinverse <- function() inv
   setinverse <- function(inverse) inv <<- inverse

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#  Returns the inverse of a "matrix" object that was created by 
#  function makeCacheMatrix. The cached value is returned if
#  present and up to date. Otherwise the inverse is calculated,
#  cached for future use, and then returned.
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("returning cached inverse")
      return(inv)
   }

   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)

   m
}

############################   
##
##   Unit Tests
##

validateConstruction <- function(cm) {
   enforce = ""
   stopifnot(!is.null(cm) || enforce == "makeCacheMatrix() returns non-null")
   stopifnot(is.list(cm) || enforce == "makeCacheMatrix() returns a list")
   stopifnot(!is.null(cm$set) || enforce == "CacheMatrix has attr 'set'")
   stopifnot(!is.null(cm$get) || enforce == "CacheMatrix has attr 'get'")
   stopifnot(!is.null(cm$setinverse) || enforce == "CacheMatrix has attr 'setinverse'")
   stopifnot(!is.null(cm$getinverse) || enforce == "CacheMatrix has attr 'getinverse'")
   stopifnot(!is.null(cm$getinverse) || enforce == "CacheMatrix has attr 'getinverse'")
   stopifnot(is.matrix(cm$get()) || enforce == "CacheMatrix$get returns a matrix")
   stopifnot(is.null(cm$getinverse()) || enforce == "No cached inverse to begin with")
}

validateDefaultConstruction <- function(cm) {
   validateConstruction(cm)
   enforce = ""
   stopifnot(is.na(cm$get()[1,1]) || enforce == "Empty matrix by default")
}

validateMatrixConstruction <- function(cm, firstvalue) {
   validateConstruction(cm)
   enforce = ""
   stopifnot(cm$get()[1,1] == firstvalue || 
      enforce == paste("Matrix constructed with 1,1 as", firstvalue))
}


testSetters <- function(cm) {
   enforce = ""
   cm$set(as.matrix(rbind(c(43, 1), c(0.3, 342))))
   m <- cm$get()
   stopifnot(m[1,1] == 43 || enforce == "Matrix 1,1 set to 43")

   cm$setinverse(rbind(c(.22, .33), c(.44, .55)))
   i <-cm$getinverse()
   stopifnot(i[2,2] == .55 || enforce == "Cached inverse at 2,2 is .55")

   cm$set(as.matrix(rbind(c(1.2, 2.3), c(3.4, 4.5))))
   stopifnot(is.null(cm$getinverse()) || enforce == "Cache cleared by set")
}

validateInv2 <- function(inv2, ci) {
   stopifnot(inv2[1,1] == ci[1,1] || enforce == "cacheSolve 1,1 is 1.0666667")
   stopifnot(inv2[1,2] == ci[1,2] || enforce == "cacheSolve 1,2 is 0.2666667")
   stopifnot(inv2[2,1] == ci[2,1] || enforce == "cacheSolve 2,1 is 1.0666667")
   stopifnot(inv2[2,2] == ci[2,2] || enforce == "cacheSolve 2,2 is 1.0666667")
}

testCacheSolve <- function() {

   # forced cached
   cm <- makeCacheMatrix()
   cm$setinverse(rbind(c(1,2), c(3,4)))
   inv <- cacheSolve(cm)

   enforce = ""
   stopifnot(inv[1,1] == 1 || enforce == "cacheSolve 1,1 is 1.0666667")
   stopifnot(inv[1,2] == 2 || enforce == "cacheSolve 1,2 is 0.2666667")
   stopifnot(inv[2,1] == 3 || enforce == "cacheSolve 2,1 is 1.0666667")
   stopifnot(inv[2,2] == 4 || enforce == "cacheSolve 2,2 is 1.0666667")

   ## Test Data, c its inverse ci
   #
   #       [,1]  [,2]
   # [1,]  1.00 -0.25
   # [2,] -0.25  1.00
   c <- rbind(c(1, -1/4), c(-1/4, 1))
   #
   #           [,1]      [,2]
   # [1,] 1.0666667 0.2666667
   # [2,] 0.2666667 1.0666667
   ci <- solve(c)

   cm2 <- makeCacheMatrix(c)
   validateMatrixConstruction(cm2, 1)
   
   inv2 <- cacheSolve(cm2)
   validateInv2(inv2, ci)

   # cached, a real one this time
   inv2 <- cacheSolve(cm2)
   validateInv2(inv2, ci)

   # Make sure cached values are independent
   stopifnot(inv[1,1] != inv2[1,1] || enforce == "cached value 1,1 is different for inv inv2")
   stopifnot(inv[1,2] != inv2[1,2] || enforce == "cached value 1,2 is different for inv inv2")
   stopifnot(inv[2,1] != inv2[2,1] || enforce == "cached value 2,1 is different for inv inv2")
   stopifnot(inv[2,2] != inv2[2,2] || enforce == "cached value 2,2 is different for inv inv2")

   TRUE
}

testsuite <- function() {
	cm <- makeCacheMatrix()
   validateDefaultConstruction(cm)
   testSetters(cm)
   if (testCacheSolve()) {
      "All tests passed."
   } else {
      "Test suite failed to run correctly."
   }
}


##### Ok people, move along. There's nothing to evaluate below this point. ####
## 
## Example: Caching the Mean of a Vector
##

makeVector <- function(x = numeric()) {
   m <- NULL

   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x

   setmean <- function(mean) m <<- mean
   getmean <- function() m

   list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
   m <- x$getmean()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }

   data <- x$get()
   m <- mean(data, ...)
   x$setmean(m)

   m
}
