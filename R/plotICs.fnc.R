plotICs.fnc <-
function(
                        ica.object,
                        epoch.length=NA,
                        cor.only=FALSE,
                        whichEOG=c("VEOG","HEOG"),
                        method="by.trial", # or "all")
                        dir.create.path=paste(getwd(),"/figs/",sep=""),
                        dir.create.name="IC.plots",
                        recursive=TRUE,
                        threshold=NA,
                        nplots=c(2,2),
                        ask=TRUE,
                        plot.EOG=TRUE,
                        pdf.it=FALSE,
                        pdf.prefix="ICs_"                      
                      ){
  if(pdf.it){    # if pdf-ing the plots
    dir.create(paste(dir.create.path,dir.create.name,sep="/"),showWarnings=FALSE,recursive=recursive)
    pdf(file=paste(dir.create.path,dir.create.name,"/EOGs_Subject ",unique(ica.object$eeg$Subject),".pdf",sep=""))
    ask=FALSE
    if(plot.EOG){   # set par(mfrow) for the EOG plot depending on the number of EOGs
      if(length(whichEOG)==1){
        par(mfrow=c(1,1))
        plot(ica.object$eeg[,whichEOG],type="l",main=eog)    
      } else if (length(whichEOG)==2){
        par(mfrow=c(1,2))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      } else if (length(whichEOG)==3){
        par(mfrow=c(1,3))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      } else {
        par(mfrow=c(2,2))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      }
    }
    dev.off()
  } else { # if not pdf-ing the plots
    if(plot.EOG){ # if plotting the EOGs
      if(length(whichEOG)==1){
        par(mfrow=c(1,1))
        plot(ica.object$eeg[,whichEOG],type="l",main=eog)    
      } else if (length(whichEOG)==2){
        par(mfrow=c(1,2))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      } else if (length(whichEOG)==3){
        par(mfrow=c(1,3))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      } else {
        par(mfrow=c(2,2))
        for(eog in whichEOG){
          plot(ica.object$eeg[,eog],type="l",main=eog)    
        }
      }
      dev.new()  # if not pdf-ing the plots, open new X11 window for the EOG plots
    }
  }

  if(!pdf.it){ # set par(ask) to 'ask' if not pdf-ing the plots
    par(ask=ask)
  }

  S=ica.object$S # save the source matrix, S, from the ica.object in variable 'S'
  MEANsnr=mean(ica.object$snr.ica[1:32,2]) # calculate mean SNR
  if (is.na(threshold)){ # if threshold was not specified, get it from 'ica.object'
    threshold=ica.object$threshold
  }

  if(pdf.it){ # if pdf-ing the plots, open pdf device
    pdf(file=paste(dir.create.path,dir.create.name,"/",pdf.prefix,"Subject ",unique(ica.object$eeg$Subject),".pdf",sep=""))
    ask=FALSE
    par(mfrow=nplots)      
  } else {
    par(mfrow=nplots)  
  }
  
  for(eog in whichEOG){ # for each EOG, plot the source matrix
    correlations=as.numeric()
    for(i in 1:ncol(S)){ # for EOV 'eog', plot source matrix
      if(mean(S[,i])!=0){
        correlations=c(correlations,cor(ica.object$eeg[,eog],S[,i])) # calculate correlations between EOG and source matrix
      }
    }
    for(i in 1:ncol(S)){ 
      if(is.na(correlations[i])){ # if correlation is NA, reset value to '0'
        correlations[i]=0
      }
      if(abs(correlations[i])>=threshold){
        col="blue" # if correlation is at or above threshold, set color to blue
        if(cor.only){
          plot(S[,i],type="l",col=col,main=paste("Subject",as.character(unique(ica.object$eeg$Subject)),";",eog,"; IC",i,sep=" "),xlab="Time",ylab="Microvolts")
          mtext(paste("Correlation",eog,"-- IC",i,"=",round(correlations[i],3),sep=" "),side=3,line=0,adj=0,cex=0.7,col=col) # add correlation
          mtext(paste("Mean SNR =",round(MEANsnr,4),sep=" "),side=1,line=2,adj=0,cex=0.7,col=col) # add mean SNR
        }
      } else {
        col="grey" # if correlation is below threshold, set color to grey
      }
      if(!cor.only){
        plot(S[,i],type="l",col=col,main=paste("Subject",as.character(unique(ica.object$eeg$Subject)),";",eog,"; IC",i,sep=" "),xlab="Time",ylab="Microvolts")
        mtext(paste("Correlation",eog,"-- IC",i,"=",round(correlations[i],3),sep=" "),side=3,line=0,adj=0,cex=0.7,col=col) # add correlation
        mtext(paste("Mean SNR =",round(MEANsnr,4),sep=" "),side=1,line=2,adj=0,cex=0.7,col=col) # add mean SNR
      }
      if(abs(correlations[i])>=threshold){
        S[,i]=0
      }
    }
  }

  if(pdf.it){ # if pdf-ing plots, close device
    dev.off()
  } else { # if not pdf-ing, reset par(mfrow) to c(1,1) and ask to FALSE
    par(mfrow=c(1,1))
    par(ask=FALSE)
  }

}

