############################################################
### Simple image processing and clustering of images
### data contains hand-written digits as 28x28 pixels
### data was extracted from http://yann.lecun.com/exdb/mnist/

library(seriation)
fd <- "/home/mnaeem/r.codes/book/"
numbers <- read.csv(paste(fd,"numbers.csv", sep=""), header=TRUE)
dim(numbers)

### helper: take a row of pixels and make it into a 28x28 matrix
toMatrix <- function(x) matrix(as.numeric(x), nrow=28, byrow=TRUE)
### convert a matrix back to a vector
toVector <- function(x) as.vector(t(x))

### test if it works
all(toVector(toMatrix(numbers[2,])) == numbers[2,])

toMatrix(numbers[2,])
pimage(toMatrix(numbers[2,]))

###########################################################
### some basic image processing

### 2D convolution 
### see: 
### http://en.wikipedia.org/wiki/Kernel_%28image_processing%29
### http://graphics.stanford.edu/courses/cs178/applets/convolution.html

### Note: This is very slow O(n^2*k^2) (n=image size, k=kernel size)
###       So we do it in C++
### Source: http://permalink.gmane.org/gmane.comp.lang.r.rcpp/2926
library(Rcpp)
library(inline)
convolve_2d <- cxxfunction(signature(sampleS = "numeric", 
  kernelS = "numeric"), 
  plugin = "Rcpp", '
    Rcpp::NumericMatrix sample(sampleS), kernel(kernelS);
    int x_s = sample.nrow(), x_k = kernel.nrow();
    int y_s = sample.ncol(), y_k = kernel.ncol();
                           
    Rcpp::NumericMatrix output(x_s + x_k - 1, y_s + y_k - 1);
    for (int row = 0; row < x_s; row++) {
     for (int col = 0; col < y_s; col++) {
       for (int i = 0; i < x_k; i++) {
         for (int j = 0; j < y_k; j++) {
           output(row + i, col + j) += sample(row, col) * kernel(i, j);
         }
       }
     }
    }
    return output;
  ')


### blurring images (with a Gaussian Kernel)
blurr <- rbind(
  c(0  , .5, 0 ),
  c(0.5, 1 , .5), 
  c(0  , .5, 0 )
)

blurr <- blurr/sum(blurr) ### normalize to sum to 1
pimage(blurr)

#m <- toMatrix(numbers[1,])
#m <- toMatrix(numbers[2,])
#m <- toMatrix(numbers[3,])
m <- toMatrix(numbers[4,])

pimage(m)
mb <- convolve_2d(m, blurr) # blurr
pimage(mb)
mbb <- convolve_2d(mb, blurr) # blurr some more
pimage(mbb)
### keep 40% of the darkest (non-white) pixels
mbb2 <- mbb>=quantile(mbb[mbb>0], 1-.4)
pimage(mbb2)


### the pattern we are looking for (2 dark pixels in 6 rows)    
conv1 <- rbind(
  c(0,0,1,1,0,0),
  c(0,0,1,1,0,0),
  c(0,0,1,1,0,0),
  c(0,0,1,1,0,0),
  c(0,0,1,1,0,0),
  c(0,0,1,1,0,0)
)
pimage(conv1)
### the horizontal pattern
conv2 <- t(conv1)
pimage(conv2)


### we start with a b/w image
mc1 <- convolve_2d(mbb2, conv1)
pimage(mc1)

### the maximum is 12 (12 1-values in conv1)
pimage(mc1>8)

### find horizontal lines
mc2 <- convolve_2d(mbb2, conv2)
pimage(mc2>8)

### edge detection
conv3 <- rbind(
  c(0 ,-2 ,0 ),
  c(-2, 8 ,-2),
  c(0 ,-2 ,0 )
)

mc3 <- convolve_2d(mbb2, conv3)
pimage(mc3)
pimage(mc3<0)


### corner detection
conv4 <- rbind(
  c(-1 ,-1 ,0 ),
  c(-1, 1 , 1),
  c(0 , 1 , 1 )
)

mc4 <- convolve_2d(m, conv4)
pimage(mc4)
pimage(mc3<0)



### Now let's try clustering. Simple approach: Take a small sample and 
### cluster the pixels directly
s <- numbers[sample(1:nrow(numbers), 1000),]
km <- kmeans(s, c=10, nstart=5)
#km

### how many are in each cluster?
table(km$cluster)

### look at some members of cluster 1
s1 <- s[km$cluster==1,]
pimage(toMatrix(s1[1,]))
pimage(toMatrix(s1[2,]))
pimage(toMatrix(s1[3,]))
pimage(toMatrix(s1[4,]))
pimage(toMatrix(s1[5,]))
pimage(toMatrix(s1[6,]))

### look at centroids
pimage(toMatrix(km$centers[1,]))
pimage(toMatrix(km$centers[2,]))
pimage(toMatrix(km$centers[3,]))
pimage(toMatrix(km$centers[4,]))
pimage(toMatrix(km$centers[5,]))
pimage(toMatrix(km$centers[6,]))
pimage(toMatrix(km$centers[7,]))
pimage(toMatrix(km$centers[8,]))
pimage(toMatrix(km$centers[9,]))
pimage(toMatrix(km$centers[10,]))

