library(raster)

w <- matrix(c(1,1,1,1,0,1,1,1,1), nr=3,nc=3)

# Step 1: Create function to apply Conway's rules to results of focal operation
gameOfLife <- function(x) {
  f <- raster::focal(x, w=w, pad=TRUE, padValue=0)
  # cells with less than two or more than three live neighbours die
  x[f<2 | f>3] <- 0
  # cells with three live neighbours become alive
  x[f==3] <- 1
  x
}

# Step 2: Iteratively apply the above function
sim <- function(x, fun, n=20, pause=0.25) {
  for (i in 1:n) {
    x <- fun(x)
    plot(x, legend=FALSE, asp=NA, main=i)
    dev.flush()
    Sys.sleep(pause)
  }
  invisible(x)
}

# Two alternative initializations
# Initialize a random map
randm <- matrix(sample(0:1, 10000, replace=TRUE), nrow=100)
init <- raster(randm)

# Initialize a pattern that produces a Gosper glider gun
m <- matrix(0, nc=48, nr=34)
m[c(40, 41, 74, 75, 380, 381, 382, 413, 417, 446, 452, 480, 
    486, 517, 549, 553, 584, 585, 586, 619, 718, 719, 720, 752, 
    753, 754, 785, 789, 852, 853, 857, 858, 1194, 1195, 1228, 1229)] <- 1
init <- raster(m)

plot(init)

# Run the model
sim(init, gameOfLife, n=150, pause=0.05)
