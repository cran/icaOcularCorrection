batch.icaDEN.fnc <-
function(
                          epoch.length=NA,
                          method="by.trial", # which is better for auditory data
                                             # or "all"
                          fn=NA,
                          fn.path="./dataEEG/formattedEEG",
                          fn.pattern="^Formatted.*\\.rda",
                          fn.full.names=TRUE,
                          fn.object.name="eeg",
                          begin=1,
                          finish=NA,
                          threshold=0.4,
                          n.comp=NA,                          
                          dir.create.path=paste(getwd(),"/dataEEG",sep=""),
                          dir.create.name="ICA.denoised",
                          fn.save.prefix="ICA_",
                          other.things.to.do=NULL, # or, e.g., c('eeg$Recalled=as.factor(eeg$Recalled)','eeg$LogitABCD=as.numeric(eeg$LogitABCD)')                 
                          electrodes=defineElectrodeSet.fnc()$electrodes,
                          eogs=list(VEOG=c("BE","TE"),HEOG=c("LC","RC")),
                          recursive=TRUE,
                          ret.SAWK=TRUE,
                          plot.it=TRUE, # plots the IC that correlates with EOG > threshold
                          fn.pdf="default",  # or any other file name
                          verbose=TRUE,
                          gen.log=TRUE
                        ){
    # create log
    if(gen.log){
      sink(file=paste(getwd(),"/logs/batch.icaDEN.fnc_log_",gsub(" ","_",date()),".txt",sep=""),split=TRUE)
    }
    # load list of files to be processed in variable 'fn"
    if (is.na(fn)){
      fn=list.files(path=fn.path,pattern=fn.pattern,full.names=fn.full.names)
    }
# if no value is provided in the 'finish' argument, then the 'finish' variable = length of 'fn', i.e., all files in 'fn'
    if (is.na(finish)){
      finish=length(fn)
    }
    for (i in begin:finish){
      cat("Loading file",fn[i],"(",(abs(begin-i)+1),"of",(finish-begin+1),") ...\n")
      load(fn[i])
      # perform other things to do if 'other.things.to.do' is not NULL
      if(length(other.things.to.do)>0){
        for(l in 1:length(other.things.to.do)){
        if(verbose){
          cat("Other things to do",l,"of",length(other.things.to.do),"...\n")
        }
        eval(parse(text=other.things.to.do[l])) 
        }
      }
      # perform actual ICA denoising
      cat("Performing ICA occular correction ...\n")
      object=eval(parse(text=fn.object.name))
      ica=icaDEN.fnc(object=object,epoch.length=epoch.length,method=method,electrodes=electrodes,eogs=eogs,threshold=threshold,n.comp,ret.SAWK=ret.SAWK,plot.it=plot.it,fn.pdf=fn.pdf,verbose=verbose)
      # save denoised data to '.rda' file
      dir.create(paste(dir.create.path,dir.create.name,sep="/"),showWarnings=FALSE,recursive=recursive)
      if(verbose){
        cat("Saving ...\n")
      }
        save(ica,file=paste(paste(dir.create.path,dir.create.name,fn.save.prefix,sep="/"),sub("^.*/(.*\\.rda$)","\\1",fn[i]),sep=""))
      # create log, continued
      if(gen.log){
        cat("------------------------\n")
        cat("snr.ica\n")
        print(ica$snr.ica[1:32,])
        cat("mean snr.ica = ",round(mean(ica$snr.ica[1:32,2]),4),"\n")
        cat("------------------------\n")
        cat("threshold\n")
        print(ica$threshold)
        cat("------------------------------------------------\n")
        sink(file=NULL)
      }

    }
}

