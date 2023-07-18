library(BiBitR)

data <- matrix(sample(c(0,1),100*100,replace=TRUE,prob=c(0.9,0.1)),nrow=100,ncol=100)
data[1:10,1:10] <- 1   # BC1
data[11:20,11:20] <- 1 # BC2
data[21:30,21:30] <- 1 # BC3
data <- as.matrix(data[sample(1:nrow(data),nrow(data)),sample(1:ncol(data),ncol(data))])

write.csv2(x = data, file = "data.csv", sep = ", ", row.names = FALSE)



# loading data into a matrix
data <- read.csv("/Documents and Settings/Jalaj/Documents/Master Thesis/Story_Classification_Data.csv")

#pre-processing data for bi-clustering
matrix <- data[,1:35]
matrix[is.na(matrix)] <- 0
matrix <- as.matrix(matrix)

result1 <- bibit2(matrix = matrix, minr=2, minc=2, noise = 0)
result1
MaxBC(result1)

result2 <- bibit2(data,minr=5,minc=5,noise=0.2)
result2
MaxBC(result2)

result3 <- bibit2(data,minr=5,minc=5,noise=3)
result3
MaxBC(result3)
