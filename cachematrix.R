## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## Given a square matrix as input, inverts the matrix using "solve" and saves it as m
makeCacheMatrix <- function(x = matrix()) {
         m <-NULL                  #initialize m as null, will hold the cached value
         set <- function(y){       #store the matrix
                x <<- y
                m <<- NULL            
         }
         get <- function() x       #get value of matrix 
         setmatrix <- function(solve) m <<- solve   #set the inverse of the matrix
         getmatrix <- function() m                  #get the inverse of the matrix
		 
         list(set=set, get=get,setmatrix=setmatrix, 
               getmatrix=getmatrix)                 #returns a list of each function
}


## Returns a cached inverse matrix if it already exists from makeCacheMatrix, else creates it using "solve"

cacheSolve <- function(x, ...) {
       
	     m <- x$getmatrix()        # get the value of m
         if(!is.null(m)){          #if m is null return the cached version
                message("fetching cached data")
                return(m)
        }
        matrix <- x$get()          #else calculate the new inverse using "solve"
        m <-solve(matrix, ...)
        x$setmatrix(m)
        m
}
