makeCacheMatrix <- function(x = matrix()) 
{
	matrixinverse <- NULL       ## Placeholder for Inverse of Matrix passed. Set to Null          
	set <- function(y)          ## Function to set matrix to new variable y and reset matrixinverse to NULL          
	{
		x <<- y
		matrixinverse <<- NULL
	}
	get <- function() x                                     ## Function to return the matrix when required to calculate inverse        	
	setinverse <- function(solve) matrixinverse <<- solve   ## Set the inverse to matrixinverse
	getinverse <- function() matrixinverse                  ## Function to return the matrixinverse  
	## A list function that contains all the functions that are defined so far.
	z <- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
	
	## Getting the matrixinverse matrix. 
	## It will be NULL if the inverse is not calculated. 
	## If solved already, it will return solve(x)
	matrixinverse <- x$getinverse()    
	message("matrixinverse passed from makeCacheMatrix:")
	print(matrixinverse)
	## This part of the code checks to see if matrixinverse is calculated or not
	## If calculated (not null), then it will print out the message saying the
	## matrixinverse is being looked up from cache data (lexical scoping) and return
	## matrixinverse. Else if matrix inverse is Null, it will go ahead and calculate
	## inverse after obtaining the matrix that was stored 
	if(!is.null(matrixinverse))
		{
			message("R retrieved cached data")
			message("Inverse of Matrix :")
			return(matrixinverse)
		}
	temp <- x$get()                    ## Retrieving the stored matrix passed to the function
	message("Since matrixinverse is NULL, Original matrix passed to CacheSolve Function from makeCacheMatrix Function as below :")
	print(temp)			 ## Error Debug Validation - Make sure the matrix is passed to cacheSolve function
	matrixinverse <- solve(temp,...) ## Calculating Inverse of matrix
	x$setinverse(matrixinverse)	   ## Updating setinverse matrix with the calculated information
	message("Inverse of Matrix :")
	matrixinverse                    ## Return the inverse of the matrix.
}


## THE FUNCTIONS ARE RAN AS SHOWN BELOW
## Step 1: Create a matrix and assign it to mat > mat <- matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
## Step 2: Pass mat to makeCacheMatrix function to cache and store it in m1 > m1 <- makeCacheMatrix(mat)
## Step 3: Run cacheSolve function by passing m1 which has list of all the get, set, getinverse, setinverse functions stored
