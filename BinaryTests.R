filepath <- "~/Desktop/all 5i work 3 27 2018/900F4430 UCSD/MUD 46.pdz"
test2 <- read.csv("/Users/lee/Desktop/all 5i work 3 27 2018/900F4430 UCSD/CSV/MUD 46.CSV", header=FALSE)


sign_change <- function(first, second){
	if(sign(first)==sign(second)){
		second
	} else if(sign(first)!=sign(second)){
		second*-1+first
	}
}

test <- sapply(integers, function(x) sign_change(x, x[x+1]))

test <- for(i in 1:length(integers)){
	x <- c(1, x)
	if(sign(x[i])==sign(x[i+1])){
		x[i+1]
	}else if(sign(x[i])!=sign(x[i+1])){
		-1*x[i+1]
		}
		x
}



    double <- readBin(con=filepath, what= "double", n=3000, endian="little", signed=FALSE)

    raw <- readBin(con=filepath, what= "raw", n=10000, endian="little", signed=FALSE)
    
    complex <- readBin(con=filepath, complex(),  n=3000, endian="little", signed=FALSE)

    integers <- readBin(con=filepath, what= "int", n=3000, endian="little", signed=FALSE)

    numerics <- readBin(con=filepath, what= "numeric",  n=3000, endian="little", signed=FALSE)

    characters <- readBin(con=filepath, what= "character",  n=3000,  endian="little", signed=FALSE)

test <- as.vector(unlist(int64::as.uint64(integers)))[c(FALSE, TRUE)]

integers.quote <- sapply(integers, as.character)
test <- as.bigz(integers.quote, mod="bigz")

plot(log(asNumeric(numerics)))

test <- asNumeric(raw, signature="bigq")

test <- sapply(raw, int64)

test <- as.uint64(integers)
plot(as.vector(unlist(as.uint64(integers))), type="line")

bob.vec <-as.vector(unlist(as.int64(numerics)))[c(FALSE, TRUE)]


integers <- readBin(con=filepath, "int", 3000, signed=FALSE )

integers <- int64::as.uint64(integers)

unsignedFourByteIntToDouble <- function(i) {
d <- as.numeric(i)
d[d<0] <- d[d<0] + 2^64
d
}

bitsToInt<-function(x) {
    packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

bintodec <- function(y) {
  # find the decimal number corresponding to binary sequence 'y'
  if (! (all(y %in% c(0,1)))) stop("not a binary sequence")
  res <- sum(y*2^((length(y):1) - 1))
  return(res)
}


characters <- readBin(filepath, character(), 3000, signed=FALSE)

testt <- gmp::as.bigz(paste0("0x",paste0(as.character(characters),collapse="")))


numerics2 <- as.int64(numerics)
plot(log(unlist(numerics2)), type="line")

    integers <- readBin(con=filepath, what= "int", n=3000, endian="little")
 integer.sub <- integers[90:2137]
    sequence <- seq(1, length(integer.sub), 1)
    
    time.est <- integer.sub[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integer.sub/(integer.sub[21]/10)
    test <- data.frame(Energy=energy, CPS=counts)


read_csv_filename_x <- function(ret){
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(ret){
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}

test.frame <- data.frame(Energy=read_csv_filename_x(test2), CPS=read_csv_filename_y(test2))
test.frame$Type <- rep("CSV", length(test.frame[,1]))
test.frame$CPS <- test.frame$CPS/(test.frame$CPS[21]/10)

test$Type <- rep("PDZ", length(test[,1]))


test3 <- rbind(test, test.frame)

library(ggplot2)
ggplot(test3, aes(Energy, CPS, colour=Type)) + geom_line()

unfold_one <- function(datum){
    if(datum<0){
        datum+211
    }else{
        datum
    }
}

unfold_two <- function(datum.1, datum.2){
    if(datum.1>100){
        datum.2+211
    }else{
        datum.2
    }
}


Hodder.v <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- y[i+1] - y[i]
        y[1:(n-1)]
        y <- y
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}

counts <- test$CPS

overflow_fix <- function(counts){
    
    counts.pass.1 <- as.vector(sapply(counts, unfold_one))
    counts.overflow <- abs(Hodder.v(Hodder.v(Hodder.v(counts.pass.1))))
    counts.pass.1[which(counts.overflow>100)] <- counts.pass.1[which(counts.overflow>100)]+211
}

which(test$CPS[Hodder.v(test$CPS)<=-50])

test3 <- rbind(test, test.frame)
test3$CPS <- as.vector(sapply(test3$CPS, unfold_one))
test3$Hodder <- Hodder.v(test3$CPS)
test3$CPS <- as.vector(sapply(seq(1, length(test3$CPS), 1), function(x)  unfold_two(datum.1=test3$Hodder[x], datum.2=test3$CPS[x])))
ggplot(test3, aes(Energy, CPS, colour=Type)) + geom_line()





test.frame$Energy2 <- test$Energy
test.frame$CPS2 <- test$CPS

resid <- round(test.frame$CPS-test.frame$CPS2, 0)
test.frame$CPS3 <- test.frame$CPS2 + resid
#test.frame$CPS4 <- int_to_unit(test$CPS, adjustment=2^32)


ggplot(test.frame, aes(Energy, CPS)) + 
geom_line() +
geom_line(aes(Energy, CPS2), linetype=2) +
#geom_line(aes(Energy, CPS4), linetype=3, colour="red") +
theme_light() + scale_x_continuous(limits=c(3, 4))



geom_line(aes(Energy, CPS3), linetype=3, colour="red") 


###why 211?

int_to_unit <- function (x, adjustment=2^32) {
  x <- as.numeric(x)
  signs <- sign(x)
  x[signs < 0] <- x[signs < 0] + adjustment
  x
}

