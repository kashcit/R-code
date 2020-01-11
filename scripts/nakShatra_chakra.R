setwd("/home/vvasuki/somasushma/R-code/scripts/")
print(getwd())
nakShatra= readLines("../data/nakShatra_list.txt",encoding="UTF-8")

# windowsFonts(Skt1=windowsFont("Siddhanta"))

e=exp(1)

kAla="-2999 : 1 : 16; 13: 24" #enter time for time stamp
prec.angle=0 #give in radians with respect to kR^ittikA 0 points

# equal division with abhijit or not
w.abh=T
if(w.abh) d=28 else{ 
  d=27
  nakShatra=nakShatra[-20]
 } 
 
ua=nakShatra[19] #get uttarAShADhA 
  
t=2*pi/d
tbox=(0:26)*t + prec.angle

z0=e^(1i*tbox) #nakShatra centers
z=e^(1i*tbox+1i*pi/27) #nakShatra boundaries
zlab=e^(1i*tbox)*1.05 #label position
zcircle=e^(1i*(0:359)*pi/180) # for circle

#abhijit insertion
j=which(nakShatra==ua)
x=225+38/60+(51/60)/60
y=222+52/60+(10/60)/60
abh=e^(1i*(tbox[j]+(x-y)*pi/180))

moon=(99+44/60)*pi/180 #ecliptic longitude of Moon
sun=(279+3/60)*pi/180 #ecliptic longitude of Sun

#moon phases: #1= new; 2=wax.cres; 3= 1st quarter; 4= wax.gibbous; 5= full; 6= wan.gibbous; 7= 2nd quarter; 8= wan.cresent
m.phase=5 
m.phase.box=c("\uD83C\uDF11","\uD83C\uDF12", "\uD83C\uDF13", "\uD83C\uDF14", "\uD83C\uDF15", "\uD83C\uDF16", "\uD83C\uDF17", "\uD83C\uDF18")

sun.pt="\u2600"
  
par(pty="m", mar=c(.5,1,.5,1), mgp=c(1.1,.3,0))
plot(zcircle, type = "l", col="cyan4", xlim=c(-2,2), ylim = c(-2.2,2.2), asp = 1, axes = F, xlab = "", ylab = "")
points(0,0)

z.point=head(which(tbox==min(abs(tbox))),1)#find zero point
points(z0[-z.point], pch=8, col="darkred")

points(z0[z.point], pch=8, col="darkblue", cex=1.25) #mark zero point

points(z, pch=16)
segments(x0 = rep(0, 27), y0= rep(0, 27), x1=Re(z), y1=Im(z))

points(e^(1i*sun)*.825, pch=sun.pt, col="darkorange3", cex=1.5) #Sun
points(e^(1i*moon)*.825, pch=m.phase.box[m.phase], col="black", cex=1) #Moon
points(e^(1i*(3*pi/2+tbox[z.point]))*.85, pch=4, col="red") # winter solstice
points(e^(1i*(pi/2+tbox[z.point]))*.85, pch=4, col="blue") # summer solstice
points(abh, pch=13, cex=1.2, col="deepskyblue4") #abhijit

sapply(0:26, function(x)   {if(t*x*180/pi>90) pos=2 else pos=4; text(nakShatra[x+1],x=Re(zlab[x+1]), y=Im(zlab[x+1]), adj = 0, srt=tbox[x+1]*180/pi, family="Skt1")})
mtext(kAla, side =2,  cex=1)

dev.copy(png, file="../output/figures/phAlgunIfullWithAbhijit2299BCE.png", units="in", res=600, width=8, height=8)
dev.off()
