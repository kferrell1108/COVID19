cases <- c(   75,  100,  124,  158,  221,  319,  435,
             541,  704,  994, 1301, 1697, 2247, 2943,
            3680, 4663, 6411, 9259,13789,18500)
day <- 1:20
day <- seq(from = as.Date('2020-03-01'),by = "day", length.out = length(cases))

df <- data.frame(cases, day)
df$logcases = log(cases, 10)

attach(df)
plot(day, cases, main="US COVID-19 Cases", 
     xlab="Day since 2/29",
     ylab="Cases", 
     type="b",
     pch=19, 
     col="blue")
by.h <- 10^(floor(log(max(cases), 10)))/4
abline(h=seq(0, max(cases), by = by.h), lty=3, col="grey")
abline(v=seq(from = as.Date('2020-03-01'), to = max(day), by = "week"), lty=3, col="grey")

max.date = max(day) + 10
lm.fit <- lm(logcases~day, data=df)
new.predict <- data.frame(day = seq(max(day)+1, to=max.date, by="day"))
new.predict$logcases = as.numeric(predict.lm(lm.fit, newdata = new.predict))
max.y <- ceiling(max(new.predict$logcases))+1
jpeg("COVID19 Estomates.jpg")
plot(day, logcases, main="US COVID-19 Cases", 
     xlab="Date",
     type="b",
     pch=19, 
     col="blue", 
     xlim=c(min(day), max.date+3),
     ylim=c(floor(log(min(cases),10)),max.y), ylab="",
     yaxt="n",
     sub=paste("Historical and Estimates (c) Ken Ferrell"))
abline(v=seq(from = as.Date('2020-03-01'), to = max.date, by = "day"), lty=3, col="grey")
abline(h=seq(floor(log(min(cases),10)),max.y), lty=3, col="grey")
text(max.date, seq(floor(log(min(cases),10)),max.y), adj = c(0,1),
     formatC(10^seq(floor(log(min(cases),10)),max.y), digits=0, big.mark=",", format="f"))
abline(lm.fit, col="red", lty=5)
text(day, logcases, formatC(10^logcases, digits=0, big.mark=',', format="f"), col="black", adj=c(-0.2,1), srt=315)
points(new.predict$day, new.predict$logcases, col="dark orange", pch=17, cex=1.5)
text(new.predict$day, new.predict$logcases, 
     formatC(round(10^new.predict$logcases, 0), digits=0, format="f", big.mark = ','), col="dark orange", adj=c(1.1,0.2), srt=315)
dev.off()

