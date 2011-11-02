mc.batch.icaDEN.fnc <-
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
                          pdf.prefix="ICs_",  # or any other file name
                          pdf.compress=FALSE,
                          verbose=TRUE,
                          gen.log=c(TRUE,FALSE)
                        ){
        if (!"multicore" %in% .packages(all.available = TRUE)) {
                stop("package multicore not available on this machine\n")
        }
        if (!"fastICA" %in% .packages(all.available = TRUE)) {
                stop("package fastICA not available on this machine\n")
        }

        require(multicore,quietly=TRUE)
        require(fastICA,quietly=TRUE)

    # load list of files to be processed in variable 'fn"
    if (is.na(fn)){
      fn=list.files(path=fn.path,pattern=fn.pattern)
    }
    # if no value is provided in the 'finish' argument, then the 'finish' variable = length of 'fn', i.e., all files in 'fn'
    if (is.na(finish)){
      finish=length(fn)
    }
    for (i in begin:finish){
                cat("sending out job",i,"\n")
                mcparallel(batch.icaDEN.fnc(epoch.length=epoch.length,method=method,fn=fn,
                    fn.path=fn.path,fn.pattern=fn.pattern,fn.object.name=fn.object.name,
                    begin=begin,finish=finish,threshold=threshold,n.comp=n.comp,
                    dir.create.path=dir.create.path,dir.create.name=dir.create.name,
                    fn.save.prefix=fn.save.prefix,other.things.to.do=other.things.to.do,
                    electrodes=electrodes,eogs=eogs,recursive=recursive,ret.SAWK=ret.SAWK,
                    pdf.it=pdf.it,pdf.prefix=pdf.prefix,pdf.compress=pdf.compress,
                    verbose=verbose,gen.log=gen.log),name=i)
    }
    cat("waiting for jobs to finish ...\n")
    collect(wait=TRUE)
}

