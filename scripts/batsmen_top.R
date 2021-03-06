library(readr)
batsmen <- read_csv("~/R/data_experiments/data/batsmen2.txt", 
                    col_names = FALSE)

bats=array(data = NA, dim = c(73,12))
for (j in 1:12) {
bats[,j]=unlist(sapply(seq(0+j,(864+j),12), function(x) batsmen[x,1]))
}
bats=data.frame(bats)
bats$X8=gsub("\\*", "", bats$X8)

bats[,c(1,4:12)]=sapply(c(1,4:12), function(x) as.numeric(as.character(bats[,x])))
bats[,c(2,3)]=sapply(c(2,3), function(x) as.character(bats[,x]))
colnames(bats)=c("#", "Player", "Country", "Mat", "Inns", "NO", "Runs", "HS", "100s", "50s", "Avg", "crate")

#plot 100s vs 50s
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
plot(bats$`50s`,bats$`100s`,  pch=16, col="blue", xlab = "50s", ylab = "100s", main="100s vs 50s for 73 top batsmen")
points(bats[which(bats$Player=="Bradman"),'50s'], bats[which(bats$Player=="Bradman"),'100s'], pch=16,col="red")
text(x=bats[which(bats$Player=="Bradman"),'50s'], y=bats[which(bats$Player=="Bradman"),'100s'],labels = bats[which(bats$Player=="Bradman"),'Player'], pos = 3, col="red", cex=1.5)
reg1=lm(bats$`100s` ~ bats$`50s`)
abline(reg1, col="gray40", lwd=2.5,lty=2)

text(x=50, y=20,labels = paste("r-sq=", round(summary(reg1)$r.squared,2)), col="blue", cex=1.5)
# text(x=50, y=15,labels = paste("100s/50s slope=", round(summary(reg1)$coefficients[2],2)), col="blue", cex=1.5)

#100s/50s histogram
hist(bats$`100s`/bats$`50s`, breaks=15, xlab="100s/50s", main = "histogram of 100s/50s ratio", col = topo.colors(10))
axis(1, at=seq(0,max(bats$`100s`/bats$`50s`),.25))
abline(v=median(bats$`100s`/bats$`50s`), col="red", lwd=2.5, lty=3)
text(x=.8, y=18,labels = paste("median=", round(median(bats$`100s`/bats$`50s`),2)), pos=4, col="red", cex=1.5)
box()

#plot innings vs 100s
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
plot(bats$`100s`, bats$Inns,  pch=16, col="blue", xlab = "100s", ylab = "# of innings", main="# of innings vs 100s for 73 top batsmen")
points(bats[which(bats$Player=="Bradman"),"100s"], bats[which(bats$Player=="Bradman"),'Inns'], pch=16,col="red")
text(x=bats[which(bats$Player=="Bradman"),'100s'], y=bats[which(bats$Player=="Bradman"),'Inns'],labels = bats[which(bats$Player=="Bradman"),'Player'], pos = 3, col="red", cex=1.5)
reg2=lm(bats$Inns ~ bats$`100s`)
abline(reg2, col="gray40", lwd=2.5,lty=2)
text(x=40, y=150,labels = paste("r-sq=", round(summary(reg2)$r.squared,2)), col="blue", cex=1.5)

#Inns/100 histogram
hist(bats$Inns/bats$`100s`, breaks=12, xlab="Inns/100s", main = "histogram of Inns/100s ratio", col = topo.colors(12))
axis(1, at=seq(1,max(bats$Inns/bats$`100s`),1))
abline(v=median(bats$Inns/bats$`100s`), col="red", lwd=2.5, lty=3)
text(x=9, y=25,labels = paste("median=", round(median(bats$Inns/bats$`100s`),2)), pos=4, col="red", cex=1.5)
box()

#hist averages
hist(bats$Avg, breaks=15, xlab="Average", main = "histogram of Average for top 73 batsmen", col = topo.colors(10), xaxt="n")
axis(1, at=seq(0,100,5))
abline(v=median(bats$Avg), col="gray30", lwd=2.5, lty=3)
text(x=60, y=10,labels = paste("median=", round(median(bats$Avg),2)), pos=4, col="red", cex=1.5)
text(x=60, y=7,labels = paste("p(Bradman)=", formatC(pnorm(q = bats[which(bats$Player=="Bradman"),"Avg"], mean = mean(bats$Avg), sd = sd(bats$Avg), lower.tail = F))), pos=4, col="red", cex=1.5)
box()

#hist #NO/innings
hist(bats$NO/bats$Inns, breaks=15, xlab="fraction of NO innings", main = "histogram of fraction of not-out innings for top 73 batsmen", col = topo.colors(15), xaxt="n")
axis(1, at=seq(0,.5,.05))
abline(v=median(bats$NO/bats$Inns), col="gray30", lwd=2.5, lty=3)
text(x=.08, y=8,labels = paste("median=", round(median(bats$NO/bats$Inns),2)), pos=4, col="red", cex=1.5)
abline(v=bats[bats$Player=="Kallis","NO"]/bats[bats$Player=="Kallis","Inns"], col="darkgreen", lwd=2.5, lty=3)
text(x=bats[bats$Player=="Kallis","NO"]/bats[bats$Player=="Kallis","Inns"], y=10,labels = "Kallis", pos=4, col="darkgreen", cex=1.5)
box()

#hist Highest score
hist(bats$HS, breaks=15, xlab="Highest score", main = "histogram of highest score for top 73 batsmen", col = topo.colors(15), xaxt="n")
axis(1, at=seq(1,400,50))
abline(v=median(bats$HS), col="red", lwd=3.5, lty=3)
text(x=220, y=10,labels = paste("median=", median(bats$HS)), pos=4, col="red", cex=1.5)
abline(v=bats[bats$Player=="Lara","HS"], col="darkgreen", lwd=2.5, lty=3)
text(x=bats[bats$Player=="Lara","HS"]-10, y=10,labels = "Lara", pos=2, col="darkgreen", cex=1.5)
box()

#-------
# read and process 301 batsmen dataset
batsmen <- read_csv("~/R/data_experiments/data/batsmen4.txt", 
                    col_names = FALSE)

bats1=array(data = NA, dim = c(302,13))
for (j in 1:13) {
  bats1[,j]=unlist(sapply(seq(0+j,(3913+j),13), function(x) batsmen[x,1]))
}
bats1=data.frame(bats1)
bats1[,c(1:13)]=sapply(c(1:13), function(x) as.character(bats1[,x]))
bats1$X7=gsub("\\*", "", bats1$X7)
bats1$X10=gsub("\\*", "", bats1$X10)
bats1$X9=gsub("\\+", "", bats1$X9)

te=unlist(bats1[1,])
bats1=bats1[-1,]
bats1[,c(3:13)]=sapply(3:13, function(x) as.numeric(as.character(bats1[,x])))

colnames(bats1)=te
bats1$Player=gsub(bats1$Player,pattern = "ICC\\/",replacement = "")

#PCA
bats1.pca=prcomp(bats1[,c(7:8, 10:12)], center = T, scale. = T)
bats1.pc.pred=predict(bats1.pca)

te=bats1[which(bats1.pc.pred[,1]< (-2.3) & bats1.pc.pred[,2] > (-5)),"Player"]
cat(sapply(1:length(te), function(x) paste(x, ". ", te[x], ";", sep = ""))) #alphabetical list

mean(bats1[which(bats1.pc.pred[,1]> (3) & bats1.pc.pred[,2] > (-5)),"Ave"]) #mean average
mean(bats1[which(bats1.pc.pred[,1]> (3) & bats1.pc.pred[,2] > (-5)),"Runs"]) #mean career runs
mean(bats1[which(bats1.pc.pred[,1]> (3) & bats1.pc.pred[,2] > (-5)),"SR"]) #mean career SR


#plotting PCA
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
j=1; k=2
plot(bats1.pc.pred[,j], bats1.pc.pred[,k], xlab=paste("PCA",j), ylab=paste("PCA",k), pch=16, col="gray35", main = "301 best Test batsmen")
abline(v=c(-2.3,0), h=c(0), col="gray25", lwd=2, lty=2)

points(bats1.pc.pred[which(bats1.pc.pred[,j]< (-2.3) & bats1.pc.pred[,k] > (-5)),j], bats1.pc.pred[which(bats1.pc.pred[,j]< (-2.3) & bats1.pc.pred[,k] > (-5)),k], pch=16, col="red")

points(bats1.pc.pred[ grep(bats1$Player, pattern = "Kapil Dev"),1], bats1.pc.pred[ grep(bats1$Player, pattern = "Kapil Dev"),2], pch="O", cex=1.5, col="blue")

#hist averages 301
hist(bats1$Ave, breaks=50, xlab="Average", main = "histogram of Average for top 301 batsmen", col = topo.colors(31), xaxt="n")
axis(1, at=seq(0,100,5))
abline(v=median(bats1$Ave), col="darkblue", lwd=2.5, lty=3)
text(x=60, y=22,labels = paste("median=", round(median(bats1$Ave),2)), pos=4, col="red", cex=1.5)
text(x=60, y=15,labels = paste("p(Bradman)=", formatC(pnorm(q = bats1[which(bats1$Player=="DG Bradman (AUS)"),"Ave"], mean = mean(bats1$Ave), sd = sd(bats1$Ave), lower.tail = F))), pos=4, col="red", cex=1.5)
box()

#hist strike rate 301
hist(bats1$SR, breaks=50, xlab="SR", main = "histogram of strike rate for top 301 batsmen", col = topo.colors(50), xaxt="n")
axis(1, at=seq(0,100,5))
abline(v=median(bats1$SR), col="darkblue", lwd=2.5, lty=3)
text(x=50, y=30,labels = paste("median=", round(median(bats1$SR),2)), pos=4, col="red", cex=1.5)
abline(v=bats1[grepl(pattern = "Sehwag", bats1$Player),]$SR, col="darkred", lwd=2.5, lty=3)
text(x=75, y=20,labels = "Sehwag", col="red", cex=1.5)
text(x=60, y=18,labels = paste("p=", formatC(pnorm(q = bats1[grepl(pattern = "Sehwag", bats1$Player),"SR"], mean = mean(bats1$SR), sd = sd(bats1$SR), lower.tail = F))), pos=4, col="red", cex=1.5)
box()

#-----------
library(readr)
batsmen <- read_csv("~/R/data_experiments/data/batsmen.csv")
batsmen=batsmen[,-1]
batsmen=subset(x = batsmen,subset = !is.na(batsmen$Runs))
bats.test=batsmen[batsmen$type=="Test",]
bats.test=bats.test[,-1]
bats.test=bats.test[!duplicated(bats.test),]
bats.test=bats.test[,-c(21,22)]
bats.test=bats.test[,-c(9,13)]

#all centuries decay law
bats2=subset(x = bats.test,subset = bats.test$Runs >= (100))
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
par(mfrow=c(1,1))
te=hist(bats2$Runs, breaks=100, xlab="100s", main = "histogram of all scores >=100", col = terrain.colors(101), xaxt="n")
axis(1, at=seq(0,400,50))
box()

ye=cbind(te$counts, te$mids)
ye=ye[ye[,1] != 0,]
reg3=glm(log10(ye[,1]) ~ (ye[,2]))
reg4=glm(log10(ye[,1]) ~ log10(ye[,2]))
curve(10^reg3$coefficients[1]*10^(reg3$coefficients[2]*x), from = 100,to=400, col="darkred", lwd=2, lty=2, add = T)
text(x=250,y=600, labels = expression(paste("y= k",10^(ax))), cex=3, col="darkred")
text(x=250,y=450, labels = paste("k= ",formatC(10^reg3$coefficients[1]), "; a= ", round(reg3$coefficients[2], 2)), cex=2, col="darkred")
curve(10^reg4$coefficients[1]*x^(reg4$coefficients[2]), from = 100,to=400, col="darkblue", lwd=2, lty=2, add = T)
text(x=250,y=300, labels = expression(paste("y= k",x^a)), cex=3, col="darkblue")
text(x=250,y=150, labels = paste("k= ",formatC(10^reg4$coefficients[1]), "; a= ", round(reg4$coefficients[2], 2)), cex=2, col="darkblue")

#scores over career
par(mfrow=c(2,3))
for(j in 1:6){
batter=subset(bats.test, grepl(bats.test$Player,pattern = bname[j]))
den=density(batter$Runs)
plot(den, type="n", xlim=c(0, max(den$x)), ylim=c(0,.018), xlab="Runs", main=paste(bname[j], "Per/Inn=", round(sum(batter$Runs)/(length(batter$Runs)-length(which(batter$Notout==T))),2)))
abline(v=seq(0,400,50), h=seq(0,.02, .005), col="gray", lty=2)
points(den, type="l", col="red", lwd=2) 
}

#strikerate over career
bname=c("Sehwag", "Tendulkar", "Dravid", "Laxman", "Kohli", "Ganguly")
par(mfrow=c(2,3))
for(j in 1:6){
  batter=subset(bats.test, grepl(bats.test$Player,pattern = bname[j]))
  batter=batter[batter$Runs>0,]
  
  hist(batter$SR, breaks =30, col=terrain.colors(31), xlab="strike rate", main=paste(bname[j], "; mean=", round(mean(batter$SR, na.rm = T),1)))
  abline(v=mean(batter$SR, na.rm = T), col="darkblue", lwd=2, lty=2)
  box()
}

#runs vs balls (strike rate angle) faced as angle
par(mfrow=c(2,3))
for(j in 1:6){
batter=subset(bats.test, grepl(bats.test$Player,pattern = bname[j]))
te=batter[batter$BF>= 50,]
b=c(max(te$Runs/te$BF, na.rm = T), min(te$Runs/te$BF, na.rm = T))
m=median(batter$Runs/batter$BF, na.rm = T)
ang=(atan(b[1])-atan(b[2]))*180/pi
m.ang=atan(m)*180/pi
plot(batter$BF, batter$Runs, type="n", xlab="balls", ylab = "runs", main=paste(bname[j], ", ang=",round(ang,2), ", m.ang=", round(m.ang,2)))
abline(v=seq(0,400,50), h=seq(0,600,50), col="gray", lty=2)
abline(a=0, b=b[1], col="darkgreen", lwd=2, lty=2)
abline(a=0, b=b[2], col="darkgreen", lwd=2, lty=2)
abline(a=0, b=m, col="blueviolet", lwd=2, lty=2)
points(batter$BF, batter$Runs, pch=16,col="darkred")
}

#distribution 6s and 4s in centuries/50s
par(mfrow=c(2,2))
for(j in c(50, 100, 150, 200)){
te=bats.test[bats.test$Runs >= j,]
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
a=hist((te$X4s*4+te$X6s*6)/te$Runs, breaks=60, plot = F)
plot(x=range(a$mids), y=range(a$counts), type ="n", main = paste(" fraction in 4s+6s in scores >=", j), xlab="# of 4s+6s", ylab="frequency")
abline(v=seq(0,1,.2), h=seq(0,ceiling(max(a$counts)/10^floor(log10(max(a$counts))))*10^floor(log10(max(a$counts))),10^floor(log10(max(a$counts)))/2), col="gray", lwd=2, lty=3)
hist((te$X4s*4+te$X6s*6)/te$Runs, breaks=60, col=heat.colors(70), add=T)
abline(v=mean((te$X4s*4+te$X6s*6)/te$Runs, na.rm = T), col="darkblue", lwd=2)
}

#average number of centuries for innings tests
te=aggregate(Inns ~ Start.Date+Opposition, data = bats.test, unique) # gets all innings
#total number of innings
te=lengths(te$Inns)
inns=sum(te) 

#count of 100s and probability 100 in an innings
c100s=length(bats.test$Runs[bats.test$Runs>=100])
p100s=c100s/inns #probability 100 in an innings

#100s per innings
bats2=subset(x = bats.test,subset = bats.test$Runs >= (100))
te=aggregate(Runs ~ Start.Date+Opposition+Inns, data = bats2, length)
d100s=c((inns-length(te$Runs))/inns, table(te$Runs)/inns)
names(d100s)=0:5

par(mfrow=c(1,1))
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
barplot(d100s, col="lightblue", main = "Fraction of innings with n 100s")
barplot(dpois(0:5, p100s), density = 10, angle =45, col="black", add = T) #compare with Poisson distribution
text(4,.4, labels = substitute(paste(lambda, "=", p), list(p=round(p100s,3))), cex=2.5)
box()

#Distribution of runs scored by bat in an innings
te=aggregate(Runs ~ Start.Date+Opposition+Inns, data = bats.test, sum)
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
hist(te$Runs, breaks=100, col=heat.colors(110), xlab="score", main = paste("histogram of score/innings; ", "median= ", median(te$Runs)))
abline(v=median(te$Runs), col="darkblue", lwd=2)
box()

#Distribution of fours in innings
bats.test$X4s=as.numeric(bats.test$X4s)
te=bats.test[!is.na(bats.test$X4s),]
fours=aggregate(X4s ~ Start.Date+Opposition+Inns, data = te, sum)
fours=fours[fours$X4s>0,]

par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
hist(fours$X4s, breaks=100, col=heat.colors(110), xlab = "# of fours", main = paste("histogram of fours/innings; ", "median= ", median(fours$X4s)))
abline(v=median(fours$X4s), col="darkblue", lwd=2)
box()

#Distribution of sixes in innings
te=bats.test[-is.na(bats.test$X6s),]
sixes=aggregate(X6s ~ Start.Date+Opposition+Inns, data = te, sum)
d6s=table(sixes$X6s)/length(sixes$Inns)
p6s=sum(sixes$X6s)/length(sixes$Inns)
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
barplot(d6s, col=heat.colors(23), xlab = "# of sixes", main = "fraction of inns with n sixes")
box()

#mean strike rate distribution distributions
te=bats.test[bats.test$Runs>10,]
te=te[-is.na(te$SR),]
sr=aggregate(SR ~ Player, data = te, mean)
sr=sr[order(sr$SR, decreasing = T),]
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
hist(sr$SR, breaks = 50, xlim=c(0,150), col=heat.colors(53), xlab = "mean strike rate of a batsman", main = paste("histogram of mean strike rate of a batsman; ", "median= ", round(median(sr$SR),2)))
abline(v=median(sr$SR), col="darkblue", lwd=2)
box()

#innings score distribution
par(mfrow=c(2,2))
for(j in 1:4){
te=bats.test[bats.test$Inns==j,]
te=aggregate(Runs ~ Start.Date+Opposition, data = te, sum)
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
hist(te$Runs, breaks=100, col=heat.colors(110), xlab="score", main = paste("histogram of score/innings:",j , "median= ", median(te$Runs)))
abline(v=median(te$Runs), col="darkblue", lwd=2)
box()
}

