
allDa=read.csv("data.csv",sep=";")
boxplot(allDa$mean ~  allDa$Selection.s.Time,ylim=c(0,100))

#env="Light Fluctuation"

    co=0
 options("scipen"=100, "digits"=4)

 main <- function(){

     pdf("img/testSurvivingRates.pdf",width=10,height=2.8)

     par(mfrow=c(1,4),oma=c(0,8,0,2))


     sapply(unique(allDa$Provenance),function(env){
	    print("-----------------")
	    print(env)
	    short=map2short[order(map2short)][env]
	    print(as.numeric(env))
	    print(short)
	    LF=allDa[allDa$Provenance == env,]

	    par(mar=c(5,0,3,2))
	    if(as.numeric(env)==2){
		#par(mar=c(5,5,2,2))
		plot(LF$mean ~  LF$Selection.s.Time,ylim=c(0,100),type="n",xlab="Time of Genotype's selection",ylab="",axes=FALSE)
		axis(2,outer=T)
	    }
	    else
	    {
		plot(LF$mean ~  LF$Selection.s.Time,ylim=c(0,100),type="n",xlab="",ylab="",axes=FALSE)
		#	       axis(2,label=F)
	    }
	    axis(1,label=F )
	    text(seq(0,500000,by=100000),par("usr")[3]-8,srt=45,label=seq(0,500000,by=100000),pos=1,xpd=T,cex=.8)
	    mtext(paste(env," (",short,")",sep=""),cex=.7,line=1,col=as.numeric(unique(allDa$Provenance[allDa$Provenance == env])))

	    sapply(as.numeric(unique(LF$Test.Environment)),function(x){
		   arrows(y0=LF$mean[ as.numeric(LF$Test.Environment) == x]-LF$sd[ as.numeric(LF$Test.Environment) == x],
			  x0=LF$Selection.s.Time[ as.numeric(LF$Test.Environment) == x],
			  y1=LF$mean[ as.numeric(LF$Test.Environment) == x]+LF$sd[ as.numeric(LF$Test.Environment) == x],
			  x1=LF$Selection.s.Time[ as.numeric(LF$Test.Environment) == x],
			  ylim=c(0,100),pch=x,col=x,lty=1,code=3,angle=90,length=.01)
		   points(LF$mean[ as.numeric(LF$Test.Environment) == x] ~  LF$Selection.s.Time[ as.numeric(LF$Test.Environment) == x],ylim=c(0,100),pch=x,col=x,type="b")

})

	    if(as.numeric(env)==3)
		legend("bottom",legend=map2short[unique(LF$Test.Environment)],col=as.numeric(unique(LF$Test.Environment)),pch=as.numeric(unique(LF$Test.Environment)),title="Genotype's original \n environment:",bty="n")
})
     #mtext("Time of Genotype's selection",side=2,at=-22,line=9,cex=.8,las=1)
     #mtext("Time of Genotype's selection",side=1,outer=T,line=-1,cex=.7)
     mtext("Rate of Success (%)",side=2,outer=T,line=3,cex=.7)
     mtext("Environment of Test:",side=2,outer=T,cex=.7,las=1,at=.93)
     
     dev.off()

 }
main()

#map2short=c("SE","ScF","LF","SF")
#names(map2short)=unique(allDa$Test.Environment)
#   
##plot(LF$mean ~  LF$Selection.s.Time,ylim=c(0,100),pch=as.numeric(LF$Test.Environment),col=as.numeric(LF$Test.Environment))
#legend("topright",legend=levels(LF$Test.Environment),col=as.numeric(unique(LF$Test.Environment)),pch=as.numeric(unique(LF$Test.Environment)))
#    par(mar=c(5,0,0,3))
