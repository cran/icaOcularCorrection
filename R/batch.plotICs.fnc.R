batch.plotICs.fnc <-
function(
                          epoch.length=NULL,
                          cor.only=FALSE,
                          method="by.trial", # or "all")
                          fn=NA,
                          fn.path=file.path(getwd(),"data","dataEEG","ICA.denoised"),
                          fn.pattern="^ICA_.*\\.rda",
                          fn.object.name="ica",
                          whichEOG=c("VEOG","HEOG"),
                          threshold=NA,
                          nplots=c(2,2),
                          ask=FALSE,
                          plot.EOG=TRUE,
                          pdf.it=TRUE,
                          pdf.prefix="ICs_",   
                          begin=1,
                          finish=NA,
                          dir.create.path=file.path(getwd(),"figs"),
                          dir.create.name="IC.plots",
                          other.things.to.do=NULL, # or, e.g., c('eeg$Recalled=as.factor(eeg$Recalled)','eeg$LogitABCD=as.numeric(eeg$LogitABCD)')                 
                          recursive=TRUE,
                          verbose=FALSE
                        ){
  if (is.na(fn)){
    fn=list.files(path=fn.path,pattern=fn.pattern)
  }
  if (is.na(finish)){
    finish=length(fn)
  }
  for (i in begin:finish){
    cat("Loading file",fn[i],"(",(abs(begin-i)+1),"of",(finish-begin+1),") ...\n")
    load(file.path(fn.path,fn[i]))
    ica.object=eval(parse(text=fn.object.name))
    
    #perform other 
    if(length(other.things.to.do)>0){
      for(l in 1:length(other.things.to.do)){
      if(verbose){
        cat("Other things to do",l,"of",length(other.things.to.do),"...\n")
      }
      eval(parse(text=other.things.to.do[l])) 
      }
    }
    
    plotICs.fnc(ica.object=ica.object,epoch.length=epoch.length,cor.only=cor.only,whichEOG=whichEOG,method=method,dir.create.path=dir.create.path,dir.create.name=dir.create.name,threshold=threshold,nplots=nplots,ask=ask,pdf.it=pdf.it,pdf.prefix=pdf.prefix)
  }
}

