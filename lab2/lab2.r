dataset <- read.csv('lab2/dataset.csv', sep=';', header=T, fileEncoding = 'cp1251')
dataset[7, 3:12] <- c(2L, 4L, 1L, 6L, NA, 7L ,9L, 5L, 6L, 3L)
dataset[4, 3:12] <- c(7L, 6L, 4L, 9L, 2L, 7L ,9L, 3L, 6L, 10L)
mins <- apply(dataset[3:12], MARGIN = 2, min)
maxes <- apply(dataset[3:12], MARGIN = 2, max)
means <- sapply(dataset[3:12], MARGIN = 2, mean)
mmm <- summary(dataset)[c(1,4,6),]

`%notin%` <- Negate('%in%')

decreasing_rating <- sort(means, decreasing = TRUE)
