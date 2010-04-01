require(outliers)

#R sparklines
sparkline<-function(ydata=rnorm(100,500,50),width=1.5,height=0.5,sigfigs=4) {

# ydata = vector of data to be plotted
# width = width of sparlkline in inches, including text
# height = height of sparkline in inches
# sigfigs = number of significant figures to round min, max, and last values to

        temppar<-par # store default graphics parameters
        par(mai=c(0.0,0.00,0.0,0.00),fin=c(width,height))
        len<-length(ydata) # determine the length of the data set
        ymin<-min(ydata) # determine the minimum
        tmin<-which.min(ydata) # and its index
        ymax<-max(ydata) # determine the maximum
        tmax<-which.max(ydata) # and its index
        yfin<-signif(ydata[len],sigfigs) #determine most recent data point
        plotrange=c(ymin-0.3*(ymax-ymin),ymax+0.3*(ymax-ymin)) # define plot range to leave enough room for min and max circles and text
        plot(x=1:len,y=ydata,type="l",xlim=c(1,len*1.5),ylim=plotrange,col="gray30",lwd=0.5,ann=FALSE,axes=FALSE) # plot sparkline
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","blue"),cex=0.5) # plot min and max points
        text(x=len,y=ymin,labels=signif(ymin,sigfigs),cex=0.5,pos=4,col="red") # show minimum value
        text(x=len,y=ymax,labels=signif(ymax,sigfigs),cex=0.5,pos=4,col="blue") # show maximum value
        text(x=len,y=(ymin+ymax)/2,labels=yfin,cex=0.5,pos=4) # show most recent value
        par(temppar) # restore graphics defaults
}

stat <- read.csv("stat_db.csv", header=TRUE, sep="\t")
attach(stat)
productivity <- (MutCPU / (MutCPU+GCCPU))
cpunetwork <- (MutCPU / ((Uploaded + Downloaded + 1) / (1024*1024)))
cpupnetwork <- subset(cpunetwork, Downloaded > (1024 * 1024))
max_space <- rm.outlier(MaxBytesUsed, median=TRUE)
peak_alloc <- rm.outlier(PeakMegabytesAlloc, median=TRUE)
productivity <- rm.outlier(productivity, median=TRUE)
cpupnetwork <- rm.outlier(cpupnetwork, median=TRUE)

png(filename="MaxBytesUsed.png", width=320, height=39, bg="transparent")
sparkline(max_space / (1024*1024), width=3, height=0.5)
dev.off()
svg(filename="MaxBytesUsed.svg", width=3, height=0.5, bg="transparent")
sparkline(max_space / (1024*1024), width=3, height=0.5)
dev.off()

png(filename="PeakMegabytes.png", width=320, height=39, bg="transparent")
sparkline(peak_alloc, width=3, height=0.5)
dev.off()
svg(filename="PeakMegabytes.svg", width=3, height=0.5, bg="transparent")
sparkline(peak_alloc, width=3, height=0.5)
dev.off()

png(filename="Productivity.png", width=320, height=39, bg="transparent")
sparkline(productivity, width=3, height=0.5)
dev.off()
svg(filename="Productivity.svg", width=3, height=0.5, bg="transparent")
sparkline(productivity, width=3, height=0.5)
dev.off()

png(filename="CPUNet.png", width=320, height=39, bg="transparent")
sparkline(cpupnetwork, width=3, height=0.5)
dev.off()
svg(filename="CPUNet.svg", width=3, height=0.5, bg="transparent")
sparkline(cpupnetwork, width=3, height=0.5)
dev.off()
