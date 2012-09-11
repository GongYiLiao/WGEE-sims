require(gamlss)
require(ggplot2)
require(ggExtra)
require(lattice)
require(Hmisc)



## river.data <- read.csv(file="../Data/6hour_forecast_clean.csv",
##                        header=TRUE, stringsAsFactors=FALSE)

river.data <- hour6.hot

river.data <- river.data[order(as.POSIXct(river.data$validtime)), ]
river.data$date <- as.Date(as.POSIXct(river.data$validtime))
daysRange <- river.data$date[nrow(river.data)] - river.data$date[1]
dailyMarks <- as.Date(as.POSIXct(river.data$validtime[1]) + 86400*(1:daysRange))
timesV <- river.data$validtime
dat1 <- data.frame(fv=river.data$fcstvalue[-1], 
                   ov=river.data$obsvalue[-1],
                   ov.lag=river.data$fcstvalue[-nrow(river.data)],
                   er=river.data$errors[-nrow(river.data)],
                   season=river.data$season[-1], 
                   date=as.Date(river.data$validtime[-1]),                   
                   time=as.POSIXct(river.data$validtime[-1]))
dat1.U <- data.frame(fv=NA, 
                     ov=NA,
                     ov.lag=NA,
                     er=NA,
                     season=ifelse(format(dailyMarks, "%b") %in% c("Jan", "Feb", "Mar", "Oct", "Nov", "Dec"),"cold", "hot"),
                     date=dailyMarks,
                     time=(as.POSIXct(river.data$validtime[1]) + 86400*(1:daysRange)))
dat1.U <- dat1.U[!(dat1.U$date %in% dat1$date), ]
dat1 <- rbind(dat1, dat1.U)
dat1 <- dat1[order(dat1$time),]
dat1 <- dat1[difftime(timesV[-1], timesV[-nrow(dat1)], units="days") < 2, ]
dat3 <- dat1[dat1$season=="hot", ]

dat3.1 <- dat1
dat3.1$fv[dat3.1$season=="cold"] <- NA
dat3.1$ov[dat3.1$season=="cold"] <- NA
dat3.1$ov.lag[dat3.1$season=="cold"] <- NA

dat4 <- dat1[dat1$season=="cold", ]

dat4.1 <- dat1
dat4.1$fv[dat4.1$season=="hot"] <- NA
dat4.1$ov[dat4.1$season=="hot"] <- NA
dat4.1$ov.lag[dat4.1$season=="hot"] <- NA

dat3.3 <- dat3.1[as.numeric(format(dat3.1$time, "%Y")) < 2009, ]
dat3.4 <- dat3.1[as.numeric(format(dat4.1$time, "%Y")) >= 2009, ]

dat4.3 <- dat4.1[as.numeric(format(dat3.1$time, "%Y")) < 2009, ]
dat4.3 <- dat4.1[as.numeric(format(dat4.1$time, "%Y")) >= 2009, ]


pdf("../Report/Plots/tripleTimeSeries_6_hour.pdf", width=8, height=6)
par(mfrow=c(2,1))
par(mar=c(0, 1.8, 1.8, 1.8))
plot(na.omit(dat1)$time, na.omit(dat1)$ov,
     ylim=c(0, 20), yaxs='i', 
     type="l", col="blue",
     xlab="", ylab="",
     xaxt="n", yaxt="n")
axis(side=2, at=c(0, 5, 10, 15), labels=c(0, 5, 10, 15))
axis.POSIXct(3, at=seq(dat1$time[1], dat1$time[3294], by="2 years"), format="%Y")
mtext(side=4, 'observed river level', srt=90)
par(mar=c(1.8, 1.8, 0, 1.8))
plot(na.omit(dat1)$time, -1*na.omit(dat1)$fv,
     ylim=c(-20, 0), yaxs='i', 
     type="l", col="brown",
     xlab="", ylab="",
     xaxt="n", yaxt="n")
axis(side=2, at=c(0, -5, -10, -15), labels=c('', 5, 10, 15))
mtext(side=4, 'forecasted river level', srt=90)
axis.POSIXct(1, at=seq(dat1$time[1], dat1$time[3294], by="2 years"), format="%Y")

dev.off()

pdf("../Report/Plots/tripleTimeSeries_6_hour.pdf", width=6, height=4, pointsize=9)
## pdf("../paper_draft_finalising/dualTimeSeries_all_year_mark.pdf", width=6, height=6, pointsize=9)
par(mfrow=c(2,1), oma=c(0,0,0,0))
par(mar=c(0, 3, 2, 0), cex.lab=1.0, mgp = c(1.6, 0.5, 0))
plot(ov ~ time, data=dat3.1,
     ylim=c(0, 20), yaxs='i', 
     type="l", lty=1,
     xlab="", ylab="Observed River Level (in Feet)",
     xaxt="n")
par(new=TRUE)
plot(ov ~ time, data=dat4.1,
     ylim=c(0, 20), yaxs='i', 
     type="l", ## col="grey75",
     xlab="", ylab="",
     xaxt="n")
lines(dat3.1$time, rep(15, length(dat3.1$time)), lty=2, col="gray", lwd=1)
lines(dat3.1$time, rep(10, length(dat3.1$time)), lty=2, col="gray", lwd=1)
lines(dat3.1$time, rep(5, length(dat3.1$time)), lty=2, col="gray", lwd=1)
axis(side=2, at=c(0, 5, 10, 15), labels=c(0, 5, 10, 15))
axis.POSIXct(3, at=seq(min(dat3.1$time), max(dat3.1$time), by="2 year"), format="%Y")
for(i in seq(2003, 2009, by=2))
  abline(v=as.POSIXct(paste(i, "-01-01 18:00:00", sep=""),"EDT"), col="gray", lty=1)
## legend("top", lty=c(1, 3), legend=c("Hot Seasons", "cold Seasons"), bty="n", horiz=TRUE)
## mtext(side=2, 'Observed River Level (in Feet)', srt=90, cex=.7)
par(mar=c(0, 3, 0, 0), cex.lab=1.0, mgp = c(1.6, 0.5, 0))
## plot(-1*fv ~ time, data=dat3.1,
##      ylim=c(-20, 0), yaxs='i', 
##      type="l", lty=1,
##      xlab="", ylab="Forecasted River Level (in Feet)",
##      xaxt="n", yaxt="n")
## par(new=TRUE)
## plot(-1*fv ~ time, data=dat4.1,
##      ylim=c(-20, 0), yaxs='i', 
##      type="l", ## col="grey75",
##      xlab="", ylab="Forecasted River Level (in Feet)",
##      xaxt="n", yaxt="n")
## lines(dat3.1$time, rep(-15, length(dat3.1$time)), lty=2, col="gray", lwd=1)
## lines(dat3.1$time, rep(-10, length(dat3.1$time)), lty=2, col="gray", lwd=1)
## lines(dat3.1$time, rep(-5, length(dat3.1$time)), lty=2, col="gray", lwd=1)
## for(i in seq(2003, 2009, by=2))
##   abline(v=as.POSIXct(paste(i, "-01-01 18:00:00", sep=""),"EDT"), col="gray", lty=1)
par(mar=c(2, 3, 0, 0), cex.lab=1.0, mgp = c(1.6, 0.5, 0))
plot(er ~ time, data=dat3.1,
     ylim=c(-10, 10), yaxs='i', 
     type="l", lty=1,
     xlab="", ylab="Forecasted Error (in Feet)",
     xaxt="n", yaxt="n")
par(new=TRUE)
plot(er ~ time, data=dat4.1,
     ylim=c(-10, 10), yaxs='i', 
     type="l", ## col="grey75",
     xlab="", ylab="Forecasted Error (in Feet)",
     xaxt="n", yaxt="n")
axis(side=2, at=c(-8, -4, 0, 4, 8), labels=c(-8, -4, 0, 4, 8))
lines(dat3.1$time, rep(-5.0, length(dat3.1$time)), lty=2, col="gray", lwd=1)
lines(dat3.1$time, rep(0.0, length(dat3.1$time)), lty=2, col="gray", lwd=1)
lines(dat3.1$time, rep(5.0, length(dat3.1$time)), lty=2, col="gray", lwd=1)
## mtext(side=2, 'Forecasted River Level (in Feet)', srt=90, cex=.7)
axis.POSIXct(1, at=seq(min(dat3.1$time), max(dat3.1$time), by="2 years"), format="%Y")
for(i in seq(2003, 2009, by=2))
  abline(v=as.POSIXct(paste(i, "-01-01 18:00:00", sep=""),"EDT"), col="gray", lty=1)
## legend("bottom", lty=c(1, 3), legend=c("Hot Seasons", "cold Seasons"), bty="n", horiz=TRUE)
dev.off()


pdf("../paper_draft_finalising/forecastVsObserved.pdf", width=6, height=3, pointsize=9)

par(mfrow=c(1,2), oma=c(0,3,0,0))
par(mar=c(3, 0, 0, .25), mgp = c(1.6, 0.5, 0))
plot(ov~fv, data=dat3,
     ylim=c(0, 18), xlim=c(0, 18),
     pch=16, cex=1.0,
     xlab = "Forecast River Livel (ft)", ylab="",
     main="")
lines(1:20, 1:20, lty=2, col="gray", lwd=2)
legend("topleft",
       legend=bquote(hat(rho) == .(round(cor(na.omit(dat3)$fv, na.omit(dat3)$ov), 2))),
       cex=1.5, bty="n", text.col="grey30")
par(mar=c(3, 0.25, 0, 0), mgp = c(1.6, 0.5, 0))
plot(ov~ov.lag, data=dat3,
     ylim=c(0, 18), xlim=c(0, 18),
     pch=16, cex=1.0,
     xlab = "Lag-1 Observed River Level (ft)", ylab="", yaxt="n",
     main="")
lines(1:20, 1:20, lty=2, col="gray", lwd=2)
legend("topleft",
       legend=bquote(hat(rho) == .(round(cor(na.omit(dat3)$ov, na.omit(dat3)$ov.lag), 2))),
       cex=1.5, bty="n", text.col="grey30")
mtext("Observed River Level (ft)", side=2, outer=TRUE, cex=1.0, line=2)

dev.off()


pdf("../paper_draft_finalising/forecastVsObserved_cold.pdf", width=6, height=3, pointsize=9)

par(mfrow=c(1,2), oma=c(0,3,0,0))
par(mar=c(3, 0, 0, .25), mgp = c(1.6, 0.5, 0))
plot(ov~fv, data=dat4,
     ylim=c(0, 18), xlim=c(0, 18),
     pch=16, cex=1.0,
     xlab = "Forecast River Livel (feet)", ylab="",
     main="")
lines(1:20, 1:20, lty=2, col="gray", lwd=2)
legend("topleft",
       legend=bquote(hat(rho) == .(round(cor(na.omit(dat4)$fv, na.omit(dat4)$ov), 2))),
       cex=1.5, bty="n", text.col="grey30")
par(mar=c(3, 0.25, 0, 0), mgp = c(1.6, 0.5, 0))
plot(ov~ov.lag, data=dat4,
     ylim=c(0, 18), xlim=c(0, 18),
     pch=16, cex=1.0,
     xlab = "Lag-1 Observed River Level (ft)", ylab="", yaxt="n",
     main="")
lines(1:20, 1:20, lty=2, col="gray", lwd=2)
legend("topleft",
       legend=bquote(hat(rho) == .(round(cor(na.omit(dat4)$ov, na.omit(dat4)$ov.lag), 2))),
       cex=1.5, bty="n", text.col="grey30")
mtext("Observed River Level (ft)", side=2, outer=TRUE, cex=1.0, line=2)

dev.off()

load('../Data/6hour_forecast_dataFrame.RData')
load('../Data/gamlssSkewT.RData')

load('../Data/st5_gamlss_para_Surface.RData')

fv.lin.df <- NULL
ov.lag.lin.df <- NULL
tau.gamlss.df <- NULL
sigma.gamlss.df <- NULL
nu.gamlss.df <- NULL
mu.gamlss.df <- NULL

for(i in 1:length(fv.lin)) {
  fv.lin.df <- c(fv.lin.df, rep(fv.lin[i], length(ov.lag.lin)))
  ov.lag.lin.df <- c(ov.lag.lin.df, ov.lag.lin)
  tau.gamlss.df <- c(tau.gamlss.df, tau.gamlss[i,])
  mu.gamlss.df <- c(mu.gamlss.df, mu.gamlss[i,])
  nu.gamlss.df <- c(nu.gamlss.df, nu.gamlss[i,])
  sigma.gamlss.df <- c(sigma.gamlss.df, sigma.gamlss[i,])  
}

data.for.wf <- data.frame(fv=fv.lin.df, ov.lag=ov.lag.lin.df,
                          mu=mu.gamlss.df, sigma=sigma.gamlss.df,
                          nu=nu.gamlss.df, tau=tau.gamlss.df)


fv.lin.1 <- fv.lin[seq(1, 98, by=4)]
ov.lag.lin.1 <- ov.lag.lin[seq(1, 98, by=4)]
mu.gamlss.1 <- mu.gamlss[seq(1, 98, by=4), seq(1, 98, by=4)]
sigma.gamlss.1 <- sigma.gamlss[seq(1, 98, by=4), seq(1, 98, by=4)]
nu.gamlss.1 <- nu.gamlss[seq(1, 98, by=4), seq(1, 98, by=4)]
tau.gamlss.1 <- tau.gamlss[seq(1, 98, by=4), seq(1, 98, by=4)]

pdf("../paper_draft_finalising/paraFacePersp.pdf", width=6, height=6, pointsize=9)
par(mfrow=c(2,2), oma=c(0, 0, 0, 0))
par(mar=c(3, 0, 3, 0))
persp(fv.lin.1, ov.lag.lin.1, mu.gamlss.1, phi=20, theta=-60,
      xlab="forecast level (ft)", ylab="lag-1 observed level (ft)", zlab="",
      col="lightblue", shade=.6, expand=.7,
      ticktype="detailed", main=expression(mu))
par(mar=c(3, 0, 3, 0))
persp(fv.lin.1, ov.lag.lin.1, sigma.gamlss.1, phi=20, theta=-60,
      xlab="forecast level (ft)", ylab="lag-1 observed level (ft)", zlab="",
      col="lightblue", shade=.6, expand=.7,
      ticktype="detailed", main=expression(sigma))
par(mar=c(3, 0, 3, 0))
persp(fv.lin.1, ov.lag.lin.1, nu.gamlss.1, phi=20, theta=-60,
      xlab="forecast value (ft)", ylab="lag-1 observed value (ft)", zlab="",
      col="lightblue", shade=.6, expand=.7,
      ticktype="detailed", main=expression(nu))
par(mar=c(3, 0, 3, 0))
persp(fv.lin.1, ov.lag.lin.1, tau.gamlss.1, phi=20, theta=-60,
      xlab="forecast value (ft)", ylab="lag-1 observed value (ft)", zlab="",
      col="lightblue", shade=.6, expand=.7,
      ticktype="detailed", main=expression(tau))
## wireframe(tau~fv*ov.lag, data=data.for.wf,
##           scales = list(arrows=FALSE, cex= .45, col = "black", font = 3),
##           drape = TRUE, colorkey = FALSE,
##           screen = list(z = 45, x = -75),
##           xlab="forecast value (feet)", ylab="lag-1 observed value (feet)", zlab=expression(tau),
##           ticktype="detailed")
dev.off()

upr95 <- NULL
lwr05 <- NULL
dat4.3 <- na.omit(dat4.3)
for (i in c(1:151, 153:nrow(dat4.3))) {
  tmpGamlss <- predictAll(st2, newdata=data.frame(fv=dat4.3$fv[i], ov.lag=dat4.3$ov.lag[i]))
  upr95 <- c(upr95, qST1(0.95, mu=tmpGamlss$mu, sigma=tmpGamlss$sigma, nu=tmpGamlss$nu, tau=tmpGamlss$tau))
  lwr05 <- c(lwr05, qST1(0.05, mu=tmpGamlss$mu, sigma=tmpGamlss$sigma, nu=tmpGamlss$nu, tau=tmpGamlss$tau))
}



pdf("../paper_draft_finalising/DIIn2009to2010.pdf", width=6, height=6, pointsize=9)

par(mfrow=c(2, 1), oma=c(0,3,0,0))
par(mar=c(.2, 0, 2, .5), mgp = c(0, 0.5, 0))
plot(dat4.3$time[c(1:151,153:193)], dat4.3$ov[c(1:151,153:193)] - dat4.3$fv[c(1:151,153:193)],
     type="l", ylim=c(-1, 1), xlab="", ylab="", xaxt="n")
axis.POSIXct(3, at=seq(min(dat4.3$time[c(1:151,153:193)]), max(dat4.3$time[c(1:151,153:193)]), by="month"), format="%b", cex.axis=0.8)
xx1 <- c(dat4.3$time[c(1:151,153:193)], rev(dat4.3$time[c(1:151,153:193)]))
yy1 <- c(lwr05[1:192] - dat4.3$fv[c(1:151,153:193)], rev(upr95[1:192] - dat4.3$fv[c(1:151,153:193)]))
polygon(xx1, yy1, col="gray", border=FALSE)
lines(dat4.3$time[c(1:151,153:193)], dat4.3$ov[c(1:151,153:193)] - dat4.3$fv[c(1:151,153:193)])
legend("bottomright", "coverage rate:90.7%", bty="n", text.col="grey50" )
legend("topright", "2009     ", cex=1.2, text.col="grey60", bty="n")
par(mar=c(2, 0, .2, .5), mgp = c(0, 0.5, 0))
plot(dat4.3$time[194:376], dat4.3$ov[194:376] - dat4.3$fv[194:376], type="l", ylim=c(-1, 1),
     xlab="", ylab="", xaxt="n")
xx1 <- c(dat4.3$time[194:376], rev(dat4.3$time[194:376]))
yy1 <- c(lwr05[193:375] - dat4.3$fv[194:376], rev(upr95[193:375] - dat4.3$fv[194:376]))
polygon(xx1, yy1, col="gray", border=FALSE)
lines(dat4.3$time[194:376], dat4.3$ov[194:376] - dat4.3$fv[194:376])
axis.POSIXct(1, at=seq(min(dat4.3$time[194:376]), max(dat4.3$time[194:376]), by="month"), format="%b", cex.axis=0.8)
## mtext(side=2, '2010 Hot Season', srt=90, cex=1)
legend("bottomright", "coverage rate:90.2%", bty="n", text.col="grey50" )
legend("topright", "2010     ", cex=1.2, text.col="grey60", bty="n")
mtext("Prediction Errors (ft)", WEST<-2, outer=TRUE, cex=1.2, line=2)

dev.off()



