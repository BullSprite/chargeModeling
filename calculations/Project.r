
#if(FALSE){
#
#path <- "calculation"
#setwd(path)
require(methods)
rm(list = ls())
s <- read.delim(file = "input.txt", header = FALSE, sep=":")

names(s) <- NULL

n <- as.numeric(s[15])

k <- 9 * 10 ^ 9 # ?????? ??? ?? ???
m <- 6.645 * 10^-27 # ????? ????? 
t <- 1e-19 # ????????? ? ????????? 1 ??
time <- 0 # ??? ?? ????
particle <- setClass("particle", slots = c(x="numeric", y="numeric", q="numeric")) # ???? ?????
movingParticle <- setClass("movingParticle", contains = "particle", slots = c(vx="numeric", vy ="numeric", ax="numeric", ay="numeric")) # ????? ?????????? ? ?????
p1 <- new("movingParticle", x = as.numeric(s[1]) , y = as.numeric(s[2]), q = as.numeric(s[3]), vx = 0, vy = 0) # 4 ??? ????? ????? ???? ????
p2 <- new("movingParticle", x = as.numeric(s[4]), y = as.numeric(s[5]), q = as.numeric(s[6]), vx = 0, vy = 0)
p3 <- new("movingParticle", x = as.numeric(s[7]), y = as.numeric(s[8]), q = as.numeric(s[9]), vx = 0, vy = 0)
p0 <- new("movingParticle", x = as.numeric(s[10]), y = as.numeric(s[11]), q = as.numeric(s[12]), vx = as.numeric(s[13]), vy = as.numeric(s[14]))
df <- data.frame(p0@x, p0@y, p1@x, p1@y, p2@x, p2@y, p3@x, p3@y) # ?????????
de <- data.frame(time, p0@vx, p0@vy, p1@vx, p1@vy, p2@vx, p2@vy, p3@vx, p3@vy)
names(df) <- c("x", "y", "x1", "y1", "x2", "y2", "x3", "y3")
names(de) <- c("time", "vx", "vy", "vx1", "vy1", "vx2", "vy2", "vx3", "vy3")

len <- function(x1, y1,x2, y2) return((x2-x1)^2+(y2-y1)^2) # ????????? ????(??????? ???)

fKulona <- function(p0, p1){
  q0 <- p0@q
  q1 <- p1@q
  x0 <- p0@x
  y0 <- p0@y
  x1 <- p1@x
  y1 <- p1@y
  r <- len(x1, y1, x0, y0)^1.5
  
  nr <- 1e-1 # ?????? ?????? ?????????? ???, ??? ??? ???? = 0 (???????, ? ??? ?????????? ? ????? ?????? ? ????? ??? ???)
  
  r <- if (r< nr || r == Inf || is.nan(r)) nr else r # ?????? ????????? ????? 
  f <- -1 * k * q0 * q1 * (c(x1-x0, y1- y0)) / r
  f[1] <- f[1]+x0
  f[2] <- f[2]+y0
  return(f)
} # ???????? ??? (????????)

VectSum <- function(p, fVec01, fVec02){
  v <- c(p@x, p@y)
  delta <- fVec01 - v
  return( fVec02 + delta)
} # ??????? ?????? ? ??? ?????????? ???

newA <- function(p, f){
  x0 <- p@x
  y0 <- p@y
  x1 <- f[1]
  y1 <- f[2]
  f <- c(x1 - x0, y1- y0)/m
  f[1] <- f[1] + x0
  f[2] <- f[2] + y0
  return(f)
} # ???????????????

changer <- function(p0, f){
  a <- newA(p0, f)
  
  p0@ax <- a[1] - p0@x
  p0@ay <- a[2] - p0@y
  
  p0@x <- p0@x + p0@vx * t + p0@ax * t^2
  
  p0@vx <- p0@vx + p0@ax * t
  
  p0@y <- p0@y + p0@vy * t + p0@ay * t^2
  
  p0@vy <- p0@vy  + p0@ay * t
  
  return(p0)
} # ??????????? ????? ???

calculation <- function(n){
  if (n == 0){
    f0 <- VectSum(p0, fKulona(p0,p1), fKulona(p0,p2))
    f0 <- VectSum(p0, f0, fKulona(p0,p3))
    return(f0)
  } else if (n == 1){
    f1 <- VectSum(p1, fKulona(p1,p0), fKulona(p1,p2))
    f1 <- VectSum(p1, f1, fKulona(p1,p3))
    return(f1)
  }else if(n == 2){
    f2 <- VectSum(p2, fKulona(p2,p0), fKulona(p2,p1))
    f2 <- VectSum(p2, f2, fKulona(p2,p3))
    return(f2)
  } else{
    f3 <- VectSum(p3, fKulona(p3,p0), fKulona(p3,p1))
    f3 <- VectSum(p3, f3, fKulona(p3,p2))
    return(f3)
  }
} # ?????????? ??

for(i in 1:n){ # ?????????????
  
  f <- lapply(1, function(x) c(calculation(0), calculation(1), calculation(2), calculation(3)))
  
  p <- lapply(1, function(x)
                   c(changer(p0, c(f[[1]][1], f[[1]][2])), changer(p1, c(f[[1]][3], f[[1]][4])),
                     changer(p2, c(f[[1]][5], f[[1]][6])), changer(p3, c(f[[1]][7], f[[1]][8]))))
  
  p0 <- p[[1]][[1]]
  p1 <- p[[1]][[2]]
  p2 <- p[[1]][[3]]
  p3 <- p[[1]][[4]]
  
  time <- time  + t
  
  df <- rbind(df, c(p0@x, p0@y, p1@x, p1@y, p2@x, p2@y, p3@x, p3@y))
  de <- rbind(de, c(time, p0@vx, p0@vy, p1@vx, p1@vy, p2@vx, p2@vy, p3@vx, p3@vy))
  
}


x0 <- df$x # ???? ????? ??????????
y0 <- df$y
x1 <- df$x1
y1 <- df$y1
x2 <- df$x2
y2 <- df$y2
x3 <- df$x3
y3 <- df$y3
res <- data.frame(x0, y0, x1, y1, x2, y2, x3, y3)
names(res) <- NULL
write.table(res, file = "result.txt",  quote = TRUE,
          sep = ",",eol = ":", row.names = FALSE, col.names = FALSE)


{
jpeg('xy.jpg') #???? ????
plot(df$x, df$y, xlab = "x0", ylab = "y0", type = "l")
dev.off()

jpeg('x1y1.jpg')
plot(df$x1, df$y1, xlab = "x1", ylab = "y1", type = "l")
dev.off()

jpeg('x2y2.jpg')
plot(df$x2, df$y2, xlab = "x2", ylab = "y2", type = "l")
dev.off()

jpeg('x3y3.jpg')
plot(df$x3, df$y3, xlab = "x3", ylab = "y3", type = "l")
dev.off()

jpeg('xt.jpg')
plot(de$time, df$x, xlab = "t", ylab = "x0", type = "l")
dev.off()

jpeg('x1t.jpg')
plot(de$time, df$x1, xlab = "t", ylab = "x1", type = "l")
dev.off()

jpeg('x2t.jpg')
plot(de$time, df$x2, xlab = "t", ylab = "x2", type = "l")
dev.off()

jpeg('x3t.jpg')
plot(de$time, df$x3, xlab = "t", ylab = "x3", type = "l")
dev.off()

jpeg('yt.jpg')
plot(de$time, df$y, xlab = "t", ylab = "y0", type = "l")
dev.off()

jpeg('y1t.jpg')
plot(de$time, df$y1, xlab = "t", ylab = "y1", type = "l")
dev.off()

jpeg('y2t.jpg')
plot(de$time, df$y2, xlab = "t", ylab = "y2", type = "l")
dev.off()

jpeg('y3t.jpg')
plot(de$time, df$y3, xlab = "t", ylab = "y3", type = "l")
dev.off()

jpeg('vxt.jpg')
plot(de$time, de$vx, xlab = "t", ylab = "vx0", type = "l")
dev.off()

jpeg('vx1t.jpg')
plot(de$time, de$vx1, xlab = "t", ylab = "vx1", type = "l")
dev.off()

jpeg('vx2t.jpg')
plot(de$time, de$vx2, xlab = "t", ylab = "vx2", type = "l")
dev.off()

jpeg('vx3t.jpg')
plot(de$time, de$vx3, xlab = "t", ylab = "vx3", type = "l")
dev.off()

jpeg('vyt.jpg')
plot(de$time, de$vy, xlab = "t", ylab = "vy0", type = "l")
dev.off()

jpeg('vy1t.jpg')
plot(de$time, de$vy1, xlab = "t", ylab = "vy1", type = "l")
dev.off()

jpeg('vy2t.jpg')
plot(de$time, de$vy2, xlab = "t", ylab = "vy2", type = "l")
dev.off()

jpeg('vy3t.jpg')
plot(de$time, de$vy3, xlab = "t", ylab = "vy3", type = "l")
dev.off()

jpeg('vxvy.jpg')
plot(de$vx, de$vy, xlab = "vx0", ylab = "vy0", type = "l")
dev.off()

jpeg('vx1vy1.jpg')
plot(de$vx1, de$vy1, xlab = "vx1", ylab = "vy1", type = "l")
dev.off()

jpeg('vx2vy2.jpg')
plot(de$vx2, de$vy2, xlab = "vx2", ylab = "vy2", type = "l")
dev.off()

jpeg('vx3vy3.jpg')
plot(de$vx3, de$vy3, xlab = "vx2", ylab = "vy2", type = "l")
dev.off()

pdf('plots.pdf')

plot(df$x, df$y, xlab = "x0", ylab = "y0", type = "l")

plot(df$x1, df$y1, xlab = "x1", ylab = "y1", type = "l")

plot(df$x2, df$y2, xlab = "x2", ylab = "y2", type = "l")

plot(df$x3, df$y3, xlab = "x3", ylab = "y3", type = "l")

plot(de$time, df$x, xlab = "t", ylab = "x0", type = "l")

plot(de$time, df$x1, xlab = "t", ylab = "x1", type = "l")

plot(de$time, df$x2, xlab = "t", ylab = "x2", type = "l")

plot(de$time, df$x3, xlab = "t", ylab = "x3", type = "l")

plot(de$time, df$y, xlab = "t", ylab = "y0", type = "l")

plot(de$time, df$y1, xlab = "t", ylab = "y1", type = "l")

plot(de$time, df$y2, xlab = "t", ylab = "y2", type = "l")

plot(de$time, df$y3, xlab = "t", ylab = "y3", type = "l")

plot(de$time, de$vx, xlab = "t", ylab = "vx0", type = "l")

plot(de$time, de$vx1, xlab = "t", ylab = "vx1", type = "l")

plot(de$time, de$vx2, xlab = "t", ylab = "vx2", type = "l")

plot(de$time, de$vx3, xlab = "t", ylab = "vx3", type = "l")

plot(de$time, de$vy, xlab = "t", ylab = "vy0", type = "l")

plot(de$time, de$vy1, xlab = "t", ylab = "vy1", type = "l")

plot(de$time, de$vy2, xlab = "t", ylab = "vy2", type = "l")

plot(de$time, de$vy3, xlab = "t", ylab = "vy3", type = "l")

plot(de$vx, de$vy, xlab = "vx0", ylab = "vy0", type = "l")

plot(de$vx1, de$vy1, xlab = "vx1", ylab = "vy1", type = "l")

plot(de$vx2, de$vy2, xlab = "vx2", ylab = "vy2", type = "l")

plot(de$vx3, de$vy3, xlab = "vx2", ylab = "vy2", type = "l")
dev.off()
#print(getwd())

}

#}

#print(getwd())
