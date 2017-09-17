tWeather <- function(wi, tm, size){
	result <- matrix(NA, nrow=size+1, ncol=nrow(wi))
	result[1,] <- t(wi)
	wd <- wi
	for(i in 1:size){
		wd <- tm %*% wd
		result[i + 1, ] <- t(wd)
	}
	return(result)
}

tm <- matrix(c(1/2, 1/2, 2/3, 1/3), ncol=2)
wi <- matrix(c(1.0, 0.0), nrow=2)
size <- 10
wt <- tWeather(wi, tm, size)
wt

matplot(wt,lwd=3, ylab="prob",xlab="t",
type="l",lty=1,col=c("orange","blue"))
par(family="Osaka")
legend(nrow(wt)-4, 1, legend=c("晴れ","雨", col=c("orange","blue"),lty=1)