Data <- read.table("C:/Users/yashmr/Downloads/Data and Code to verify.csv",
                  header = TRUE, sep = ",", quote = "\, dec = "." ,fill=TRUE, comment.char=, as.is=TRUE)

Data$RequestTimewithoutGMT <- as.Date(Data$RequestTimewithoutGMT)



RequestTimePosIXlt <- as.POSIXlt(Data$RequestTimewithoutGMT, format="%y-%m-%d %H:%M:%OS", tz=)
op <- options(digits.secs = 6)
RequestTimePosIXlt


## time with fractional seconds
RequestTimeTrunc2 <- strptime(RequestTimeTrunc,%d/%m/%y %H:%M:%OS)
ResponseTimeTrunc2 <- strptime(ResponseTimeTrunc,%d/%m/%y %H:%M:%OS)
# prints without fractional seconds
RequestTimeTrunc2
ResponseTimeTrunc2
class(ResponseTimeTrunc2)
op <- options(digits.secs = 6)
options(op)


DT<- diff(ResponseTimeTrunc2-RequestTimeTrunc2)
DT"
"class(ResponseTime)
rediff<- (ResponseTime-RequestTime)
print(rediff)
Rediff <- as.difftime(c(ResponseTime, RequestTime))
timediffdata <- cbind(test1import,timediff(timediff,newdata = test1import,type = raw))
print(timediffdata)
timediff<- diff.POSIXt(ResponseTime-RequestTime)
print(timediff)
timediff2<- ResponseTime-RequestTime
print(timediff2)
class(blimport$ed)
(systime <- sys.time())
Sys.time()
format(.leap.seconds)"
