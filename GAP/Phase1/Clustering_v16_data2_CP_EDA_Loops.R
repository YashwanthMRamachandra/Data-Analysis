

set.seed(100)
A <- data.frame(X1=rnorm(2,3.4),X2=rnorm(2,3.4),X3=rnorm(2,3.4))
B <- data.frame(Y1=rnorm(2,3.4),Y2=rnorm(2,3.4),Y3=rnorm(2,3.4))
A$pages <- paste("Page",1:2,sep="")
A <- data.frame(Pages=A[,4],A[,1:3])
B$pages <- paste("Page",6:7,sep="")
B <- data.frame(Pages=B[,4],B[,1:3])


par(mfrow=c(1,2),bg="grey52",cex=1.3,mar=c(3,3,1,1),ask=T)
#par(ask=T)
for(i in c(1:nrow(A))){
  new_char<- paste0(gsub("_"," ",as.character(A[,1]))," distribution")
  new_char<-sapply(new_char,function(x){gsub(pattern = "^.",replacement = toupper(substr(x = x,1,1)),x = x)})  
  hist(as.numeric(A[i,-1]),prob=T,xlab=A[i,1],main=new_char[i],breaks=5,col="lightskyblue")
  curve(dnorm(x,mean=mean(as.numeric(A[i,-1])),sd=sd(as.numeric(A[i,-1]))),add=TRUE)  
  for(j in c(1:nrow(B))){
    new_char<- paste0(gsub("_"," ",as.character(B[,1]))," distribution")
    new_char<-sapply(new_char,function(x){gsub(pattern = "^.",replacement = toupper(substr(x = x,1,1)),x = x)})  
    hist(as.numeric(B[j,-1]),prob=T,xlab=B[j,1],main=new_char[j],breaks=5,col="lightskyblue")
    curve(dnorm(x,mean=mean(as.numeric(B[j,-1])),sd=sd(as.numeric(B[j,-1]))),add=TRUE)  
  }
}

dev.off()
