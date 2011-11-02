batch.icaDEN.fnc <-
function(
                          epoch.length=NULL,
                          method="by.trial", # which is better for auditory data # or "all"
                          fn=NA,
                          fn.path=file.path(getwd(),"data","dataEEG","formatted"),
                          fn.pattern="^formatted.*\\.rda",
                          fn.object.name="eeg",
                          begin=1,
                          finish=NA,
                          threshold=0.4,
                          n.comp=16,                          
                          dir.create.path=file.path(getwd(),"data","dataEEG"),
                          dir.create.name="ICA.denoised",
                          fn.save.prefix="ICA_",
                          other.things.to.do=NULL, # or, e.g., c('eeg$Recalled=as.factor(eeg$Recalled)','eeg$LogitABCD=as.numeric(eeg$LogitABCD)')                 
                          electrodes=defineElectrodeSet.fnc()$electrodes,
                          eogs=list(VEOG=c("BE","TE"),HEOG=c("LC","RC")),
                          recursive=TRUE,
                          ret.SAWK=TRUE,
                          pdf.it=FALSE, # plots the IC that correlates with EOG > threshold
                          pdf.prefix="ICs_",
                          pdf.compress=FALSE,
                          verbose=TRUE,
                          gen.log=c(TRUE,TRUE) # (1) whether to generate a log and (2) whether to split output
                        ){
    # load list of files to be processed in variable 'fn"
    if (is.na(fn[1])){
      fn=list.files(path=fn.path,pattern=fn.pattern)
    }
# if no value is provided in the 'finish' argument, then the 'finish' variable = length of 'fn', i.e., all files in 'fn'
    if (is.na(finish)){
      finish=length(fn)
    }

    for (i in begin:finish){
        # create log
        if(gen.log[1]){
        dir.create(file.path(getwd(),"logs"),showWarnings=FALSE,recursive=TRUE)
        sink(file=file.path(getwd(),"logs",paste("batch.icaDEN.fnc_log_",fn[i],".txt",sep="")),split=gen.log[2])
    }
      if(verbose)cat("Loading file",fn[i],"(",(abs(begin-i)+1),"of",(finish-begin+1),") ...\n")
      load(file.path(fn.path,fn[i]))
      # perform other things to do if 'other.things.to.do' is not NULL
      if(length(other.things.to.do)>0){
        for(l in 1:length(other.things.to.do)){
        if(verbose)cat("Other things to do",l,"of",length(other.things.to.do),"...\n")
        eval(parse(text=other.things.to.do[l])) 
        }
      }
      # perform actual ICA denoising
      if(verbose)cat("Performing ICA occular correction ...\n")
      object=eval(parse(text=fn.object.name))
      ica=icaDEN.fnc(object=object,epoch.length=epoch.length,method=method,electrodes=electrodes,
          eogs=eogs,threshold=threshold,n.comp,ret.SAWK=ret.SAWK,verbose=verbose)
      # save denoised data to '.rda' file
      dir.create(file.path(dir.create.path,dir.create.name),showWarnings=FALSE,recursive=recursive)
      if(verbose)cat("Saving ...\n")
        save(ica,file=file.path(dir.create.path,dir.create.name,paste(fn.save.prefix,sub("^.*/(.*\\.rda$)","\\1",fn[i]),sep="")))
      # create log, continued
      if(gen.log[1]){
        if(verbose)cat("------------------------\n")
        if(verbose)cat("snr.ica\n")
        print(ica$snr.ica[1:32,])
        if(verbose)cat("mean snr.ica = ",round(mean(ica$snr.ica[1:32,2]),4),"\n")
        if(verbose)cat("------------------------\n")
        if(verbose)cat("threshold\n")
        print(ica$threshold)
        if(verbose)cat("------------------------------------------------\n")
        sink(file=NULL)
      }

      if(pdf.it){
                if(is.list(eogs)){
                    whichEOG<-names(eogs)
                }else{
                    whichEOG<-eogs
                }
                plotICs.fnc(ica.object=ica,epoch.length=epoch.length,
                    cor.only=FALSE,whichEOG=whichEOG,method=method,
                    dir.create.path=file.path(getwd(),"figs"),
                    dir.create.name="IC.plots",recursive=TRUE,
                    threshold=threshold,nplots=c(2,2),ask=FALSE,
                    plot.EOG=TRUE,pdf.it=pdf.it,pdf.prefix=pdf.prefix,
                    pdf.compress=pdf.compress)
      }
   }
}

