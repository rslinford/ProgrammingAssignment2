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

## Creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## TODO
}

## Returns the inverse of a "matrix" object that was created by 
## function makeCacheMatrix. The cached value is returned if
## present and up to date. Otherwise the inverse is calculated,
## cached for future use, and then returned.
cacheSolve <- function(x, ...) {
   ## TODO
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
