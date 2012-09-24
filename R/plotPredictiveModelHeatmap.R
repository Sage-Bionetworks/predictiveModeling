#' Plot a heatmap displaying predictive features selected by a regression model.
#'
#' Plots a heatmap displaying selected features, with regression weights displayed to the left of each feature, and 
#' response values displayed below the feature heatmap.
#'
#' @author Adam Margolin
#' @export

plotPredictiveModelHeatmap <- 
		function(coefs, featureData, responseData, featureNames=NULL, featurePerc=NULL,
				responseLabel="", suffix, outputFile = NULL, heatmapThresh=3, responseName="sensitivity", cex.factor=1) 
{
	if(is.null(outputFile)) {
    		x11(width=11, height=10)
    } else {
      if(grepl('pdf$', outputFile, perl=TRUE)) {
            pdf(file = outputFile, width = 11, height = 10)
      } else {
            jpeg(file = outputFile, width=11, height=10, units='in', res=600)
	  }   
	}
  
  if(is.matrix(responseData)){
      message("Error: responseData must be a vector")
#     if(nrow(responseData) != 1){
#       message("Error: if responseData is a matrix it must contain 1 row")
#     }else{
#       responseData_tmp <- responseData
#       responseData <- as.numeric(responseData)
#       names(responseData) <- rownames(responseData_tmp)
#     }
  }
	
	isNas <- is.na(responseData)
	featureData <- featureData[,!isNas]
	responseData <- responseData[!isNas]
	featureData <- t(scale(t(featureData)))
	
	redblue.colors <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
					"#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
	
	## coefss
	#coefs <- eNet$beta
	
	coefs <- coefs[coefs != 0 & !is.na(coefs)]
	coefs <- coefs[order(coefs)]
	
	if (NROW(coefs)>50) {
		## 100 first coefss
		coefs100idx <- which(abs(coefs)>=sort(abs(coefs), decreasing=TRUE)[50])
		coefs <- coefs[coefs100idx]
	}
	
	## unscale factors back to binary values
	for (row in which(apply(featureData, 1, function(x) length(unique(x)))==2)) {
		featureData[row,] <- heatmapThresh * (as.numeric(factor(featureData[row,]))-1)
	}
	
	features <- names(coefs)
	if (!is.null(featureNames)){
		hmLabels <- featureNames
	}else{
		hmLabels <- features
	}
	
	if (is.null(featurePerc)) {
		barplotcol <- "darkblue"
	} else {
		## barplotcol <- as.character(lapply(as.numeric(featurePerc)^2, function(n) rgb(0,0,139,(n*256), maxColorValue=256)))
		barplotcol <- unlist(lapply(as.numeric(featurePerc)^2, function(x) rgb(colorRamp(c("#E2E2F2", "darkblue"))(x), maxColorValue=255)))
	}
	
	cl <- intersect(names(responseData), colnames(featureData))
	clustmatrix <- data.matrix(featureData[features,
					names(responseData[order(responseData[cl])]), drop=FALSE])
	
	clustmatrix[clustmatrix < -heatmapThresh] <- -heatmapThresh
	clustmatrix[clustmatrix >  heatmapThresh] <-  heatmapThresh
	
  rep.par <- par(no.readonly=TRUE)
	layout(matrix(c(1,2,0,3),2,2,byrow=TRUE), heights=c(8,1), widths=c(1,5))
	## weights barplot
	par(mar=c(2.5,.5,3.8,0), las=1, mgp=c(3,.6,0))
	plot(0, xlim=c(min(coefs, 0), max(coefs, 0)), xlab="", ylab="", type="n", axes=FALSE)
	par(usr=c(par("usr")[1:2], 0, length(coefs)))
	abline(h=c(0,length(coefs)), col="grey", lty=2, lwd=1)
	barplot(coefs, add=TRUE, horiz=TRUE, border=NA, col=barplotcol, space=0, cex.axis=.7, axes=FALSE)
	axis(1, cex.axis=.7, line=.6)
	mtext("weights", 1, line=2.2)
	## heatmap
	par(mar=c(2.5,2,3.8,11), las=1)
	image(1:ncol(clustmatrix), 1:nrow(clustmatrix), t(clustmatrix), col=rev(redblue.colors(200)), ylab="", xlab="",
			zlim=c(-heatmapThresh,heatmapThresh), main=responseName, yaxt="n", cex.main=1.5*cex.factor)
	if(nrow(clustmatrix)<4) par(yaxp=c(1, nrow(clustmatrix), max(nrow(clustmatrix)-1, 1)))
	axis(2)
	axis(4,
			at=1:length(features), labels=hmLabels,
			cex.axis=cex.factor*min((ifelse(length(features)<=130, .2, 0) + .7/log10(length(features))), 1.3),
			tick=FALSE)
	par(xpd=TRUE)
	segments(-100, par("usr")[3], par("usr")[1], col="grey", lty=2, lwd=1) ; segments(-100, par("usr")[4], par("usr")[1], col="grey", lty=2, lwd=1)
	segments(par("usr")[1], par("usr")[3], par("usr")[1], -100, col="grey", lty=2, lwd=1) ; segments(par("usr")[2], par("usr")[3], par("usr")[2], -100, col="grey", lty=2, lwd=1)
	## colorscale
	par(new=TRUE, mar=c(52,46,.5,.5), mgp=c(3,.7,0), xpd=FALSE)
	image(seq(-heatmapThresh,heatmapThresh,length.out=200),1, as.matrix(seq(-heatmapThresh,heatmapThresh,length.out=200)),
			col=rev(redblue.colors(200)), axes=FALSE, xlab="", ylab="")
	axis(1, cex.axis=.7)
	## Response scatter plot
	par(mar=c(3,2,0,11), las=1, mgp=c(3,2,1), las=2, xpd=TRUE)
	plot(responseData[order(responseData[cl])], xlab="", ylab="", 
			xaxt="n", yaxt="n", cex.axis=.5, type="n", frame.plot=FALSE)
	par(usr=c(0, length(cl), par("usr")[3:4]), xpd=TRUE)
	responseVals <- responseData[order(responseData[cl])]
	abline(v=c(0, length(cl)), col="grey", lty=2, lwd=1)
	#print(responseVals)
	#responseVals <- scale(responseVals)
	#print(responseVals)
	points((1:length(cl))-.5, responseVals,
			col=unlist(lapply(scale01(responseVals), function(x) rgb(RdBlBu.ramp(x), maxColorValue=255))), cex=.9, pch=20)
	## axis(4, at=pretty(range(responseData[,cl]), n=3), labels=pretty(range(responseData[,cl]), n=3))
	axis(4)
	axis(4, at=mean(range(responseData[cl])), labels=paste(responseName, responseName, sep="\n"), tick=FALSE, line=1.8, cex.axis=1.2*cex.factor)
	par(rep.par)
	
	if(!is.null(outputFile)){
		dev.off()
	}
}
