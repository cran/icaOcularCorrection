icaDEN.fnc <-
function(
                  object,
                  epoch.length=NULL,
                  method="by.trial", # or "all"
                  electrodes=defineElectrodeSet.fnc(bottom.eye=NA)$electrodes,
                  eogs=list(VEOG=c("BE","TE"),HEOG=c("LC","RC")),
                  threshold=0.4,
                  n.comp=16,
                  ret.SAWK=TRUE,
                  verbose=TRUE
                  ){
  require(fastICA,quietly=TRUE)
  options(warn=-1)
  date=date() # get date and time for log
  X=object[,electrodes] # select electrodes only
  if(verbose)cat("fastICA ...\n")
  
  # perform ICA decomposition (with pre-whitening and centering)
  a=fastICA(X,n.comp=n.comp,alg.typ="parallel",fun="logcosh",method="R",verbose=verbose)
  
  if (is.list(eogs)){ # if a list is supplied, VEOG and/or HEOG is computed
    for (l in 1:length(names(eogs))){
      object[,names(eogs)[l]]=object[,eogs[[names(eogs)[l]]][1]]-object[,eogs[[names(eogs)[l]]][2]]
    }
    eogs=names(eogs)
  }

  S0=a$S

  if(verbose)cat("Calculating correlations")

  if(method=="by.trial"){ # if method is by trial
    if(is.na(epoch.length)){
      stop("Error: Please supply an epoch length.\n") 
    }
    S.corrected=a$S[-(1:nrow(a$S)),] # initialize corrected source matrix
    for(k in 0:((dim(a$S)[1]/epoch.length)-1)){
      if(verbose)cat(".")
      S.temp=a$S[(1+epoch.length*k):(epoch.length+(epoch.length*k)),] # set-up temporary source matrix with nrow = one epoch
      object.temp=object[(1+epoch.length*k):(epoch.length+(epoch.length*k)),] # set-up temporary object with nrow = one epoch
      for (l in eogs){
        correlations=as.numeric() # initialize correlations variable
        for (m in 1:dim(S.temp)[2]){  # for each one the ICs of the source matrix for a given epoch do the following
            correlations=c(correlations,cor(object.temp[,l],S.temp[,m])) # calculate correlation between each IC and a given EOG and store it in variable 'corelations'. 
        }
        for (m in 1:dim(S.temp)[2]){
          if(is.na(correlations[m])){
            correlations[m]=0 # reset correlations of NA to 0
          }
          if (abs(correlations[m])>=threshold){ # if the correlation between the given EOG and ICs is equal to or greater than the threshold value, zero-out the IC
            S.temp[,m]=0 # zero-out the correlated IC
         }
        }
      }
      S.corrected=rbind(S.corrected,S.temp) # add temporary source matrix to corrected source variable
    }
    a$S=S.corrected # replace original source matrix in object 'a' with corrected source matrix
  } else { # if method is "all"
    for (j in eogs){ 
      correlations=as.numeric() # initialize correlations variable
      for (i in 1:dim(a$S)[2]){ # for each one of the ICs do the following
        if(verbose)cat(".")
        if(mean(a$S[,i])!=0){ # if the correlation is not 0, meaning that the IC was not zeroe-out previously, calculate the correlation between a given EOG and the given IC
          correlations=c(correlations,cor(object[,j],a$S[,i]))
        }
      }
      for (i in 1:dim(a$S)[2]){
        if(verbose)cat(".")
        if(is.na(correlations[i])){ # if correlation is NA, reset to 0
          correlations[i]=0
        }
        if (abs(correlations[i])>=threshold){ # if correlation is at or above threshold, zeor IC out
         a$S[,i]=0 # zero-out correlated IC
        }
      }
    }
  }

  if(verbose)cat("\nCorrecting ... \n")
  x=as.data.frame(a$S%*%a$A) # re-mix corrected data = corrected source matrix * mixing matrix
  colnames(x)=electrodes # give appropriate column names to corrected frame
  
  if(verbose)cat("Replacing with corrected EEG ...")
  for(electrode in electrodes){ # replace EEG columns in original data frame with corrected ones
    if(verbose)cat(".")
    object[,electrode]=x[,electrode]
  }
  if(verbose)cat("\n")

  if(verbose)cat("Calculating SNRs ...\n")
  snr=as.numeric() # initialize SNR variable
  for (i in 1:ncol(X)){ # for each electrode, calculate SNR
    snr=c(snr,sqrt(var(X[,i]))/sqrt(var(x[,i])))
  }
  snr=data.frame(Electrode=electrodes,MeanSNR.ICA=snr) # create SNR data frame

  options(warn=0)

  # return output
  if (ret.SAWK){ 
    return (list(
                eeg=object,
                snr.ica=snr,
                threshold=threshold,
                S=S0,
                A=a$A,
                W=a$W,
                K=a$K
                )
    )
  } else {
    return (list(
                eeg=object
                )
    )
  }
}

