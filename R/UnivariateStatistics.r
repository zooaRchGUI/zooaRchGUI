# death_age_fun
layout.death_age <- function(e){
    print("death_age layout")
    e$data <- tclVar("Choose one");
    e$labels <- tclVar("Choose one");
    e$dataFrame1<-tclVar("NULL")
    e$dataFrame2<-tclVar("NULL")
    e$models <- tclVar("NULL");
    e$func <- tclVar("Survivorship"); #Selects Survivorship for the "Functions:" secetion (Defaults to Survivorship)
    e$conf.level <- tclVar(95);
    e$iter <- tclVar(" ")


    #Begin GUI Setup
    tkwm.title(e$wnd, "Survivorship Dialog")
    tkconfigure(e$layout, text = "Surviorship")
    columnConfig(e$layout)

    #Data Combobox
    put_label(e$layout, "Data:", 0, 0, sticky = "e")
    data_combo <- ttkcombobox(e$layout, state = "readonly",
                              values = dfs.fun(),
                              textvariable = e$data)
    tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$data, "dataFrame1"))
    tkfocus(data_combo)                      # give focus

    #Labels Combobox
    put_label(e$layout, "Labels:",1,0, sticky = "e")
    data_labels <- ttkcombobox(e$layout, state = "readonly",
                               values = dfs.fun(),
                               textvariable = e$labels)
    tkgrid(data_labels, row = 1, column = 1, sticky="ew", padx = 2)
    tkbind(data_labels, "<<ComboboxSelected>>", function() updateDataFrame(e, e$labels, "dataFrame2"))
    tkfocus(data_labels)

    #Models Combobox
    put_label(e$layout, "Model:",2,0, sticky = "e")
    data_models <- ttkcombobox(e$layout, state = "readonly",
                               values = c(1:5),
                               textvariable = e$models)
    tkgrid(data_models, row = 2, column = 1, sticky="ew", padx = 2)
    tkfocus(data_models)

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0, sticky = "e")
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Function Radiobutton
    put_label(e$layout, "Functions:",4,0, sticky = "e")
    rb_frame <- ttkframe(e$layout)
    sapply(c("Survivorship","Mortality","Epiphyseal Fusion"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$func,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Iterations Spinbox
    put_label(e$layout, "Iterations:",5,0, sticky = "e")
    iter_frame <- tkspinbox(e$layout, from = 10, to = 10000, increment = 10, textvariable = e$iter, width = 6)
    tkgrid(iter_frame, row = 5, column = 1, sticky="w", padx = 2)
}

run.death_age <- function(e) {
    print("death_age run")
    conf.level<-Fusion.groups<-UpperCI<-LowerCI<-Count<-NULL
    # Wrapper for 3 Survival Functions
    runFunc <- function(Data=NULL,func=NULL,labels=NULL, models=NULL, ci=NULL, plot=TRUE, iter=NULL) {

        #Survivorship Function
        if (func=="Survivorship") {
            surv.func<-function(SurviveData=Data, labels=labels, models=models, ci=conf.level, plot=TRUE, iter=iter, usermod=NULL){
                data<-SurviveData$Ageclass
                N.ages<-max(data)
                if(!is.null(labels)){
                    Labels.ageclass <-labels
                }
                if(is.null(labels)){
                    Labels.ageclass <-1:N.ages
                }
                if(!is.null(usermod) & !is.list(usermod)){
                    stop("usmod MUST be entered as a list")
                }
                survivorcurve.Eq4 <- function(data){
                    vector <- rep(1, N.ages+1)
                    for(i in 1:N.ages+1) {
                        vector[i] <- vector[i-1]*(sum(data >= (i-1)) - sum(data == (i-1)))/sum(data >= (i-1))
                    }
                    vector[is.na(vector)] <- 0
                    round(vector,4)
                }
                survivorcurve.Eq4(data)
                survive.matrix <- matrix(NA, ncol = N.ages+1, nrow = iter)
                survive.matrix[1,] <- survivorcurve.Eq4(data)
                for(i in 2:iter){
                    bootstrap <- sample(1:length(data), length(data), replace = TRUE)
                    survive.matrix[i,] <- survivorcurve.Eq4(data[bootstrap])
                }
                ci<-ci/100
                upCI<-apply(survive.matrix[,-1], MARGIN = 2, FUN = quantile,
                            probs = ci+((1-ci)/2))
                loCI<-apply(survive.matrix[,-1], MARGIN = 2, FUN = quantile,
                            probs = ((1-ci)/2))
                if (plot==TRUE){
                    plot(x = (1:N.ages), y = survive.matrix[1,-1], type = "n", xlab = "Age Class",
                         ylab = "Proportion Survived", axes = FALSE, ylim = c(0,1))
                    polygon(c(1:N.ages,rev(1:N.ages)),c(loCI,rev(upCI)),col="light gray",border=NA)

                    #Formating the Axes
                    axis(side = 1, at = 1:N.ages, labels = Labels.ageclass)
                    axis(side = 2)
                    lines(x = (1:N.ages), y = survive.matrix[1,-1], lwd = 2)

                    #Plotting the Confidence Intervals
                    lines(x = 1:N.ages, y = loCI, lty = "dashed", ylim = c(0,1))  #Plotting the lower confidence interval (2.5%)
                    lines(x = 1:N.ages, y = upCI, lty = "dashed", ylim = c(0,1)) #Plotting the upper confidence interval (97.5%)
                    if(!is.null(models) & is.null(usermod)){
                        surv.model<-list(Security = c(.904, .904, .645, .38, .25, .239, .171, .118, 0),
                                         Milk = c(.47, .42, .39, .35, .28, .23, .19, .1, 0),
                                         Wool = c(.85, .75, .65, .63, .57, .50, .43, .20, 0),
                                         Catastrophic = c(.86, .73, .60, .49, .39, .31, .23, .16, .11, .07, .03, .01, 0),
                                         Attritional = c(.76, .53, .48, .46, .45, .42, .39, .34, .29, .20, .11, .03, 0))
                        for(j in 1:length(models)){
                            i<-models[j]
                            lines(x = 1:N.ages, y = surv.model[[i]], col = j+1, lwd = 2, lty = j)
                        }
                        legend(x = "topright", cex = .75, lwd = 2,lty = c(1,2,1:length(models)),
                               col = c(1,1,2:(length(models)+1)),
                               legend = c("Survivorship", paste(ci,"% Confidence Interval",sep=""), names(surv.model)[models]))
                    }
                    if(!is.null(usermod)){
                        surv.model<-usermod
                        models<-1:length(surv.model)
                        for(j in 1:length(models)){
                            i<-models[j]
                            lines(x = 1:N.ages, y = surv.model[[i]], col = j+1, lwd = 2, lty = j)
                        }
                        legend(x = "topright", cex = .75, lwd = 2,lty = c(1,2,1:length(models)),
                               col = c(1,1,2:(length(models)+1)),
                               legend = c("Survivorship", paste(ci,"% Confidence Interval",sep=""), names(surv.model)[models]))
                    }
                }
                data.UpperCI <- upCI
                data.PointValue <- survive.matrix[1,-1]
                data.LowerCI <- loCI
                Taxon <- rep(unique(SurviveData$Genus), times = N.ages)
                Output.Matrix <- data.frame(Taxon = Taxon, AgeClassLabs = Labels.ageclass, LowerCI = data.LowerCI,
                                            PointValue = data.PointValue, UpperCI = data.UpperCI)
                return(Output.Matrix)
            }
            Output.Matrix<- surv.func(SurviveData=Data, labels=labels, models=models, ci=ci, plot=TRUE, iter=iter, usermod=NULL)
        }
        #Mortality Function
        if (func == "Mortality") {
            mort.func<-function(mortData=Data,labels=labels, models=models, ci=conf.level, plot=TRUE, iter=iter, usermod=NULL,lsize=.01,...){
                data<-mortData$Ageclass
                N.ages<-max(data)
                if(!is.null(labels)){
                    Labels.ageclass <-labels
                }
                if(is.null(labels)){
                    Labels.ageclass <-1:N.ages
                }
                if(!is.null(usermod) & !is.list(usermod)){
                    stop("usmod MUST be entered as a list")
                }
                mortprof <- function(data){
                    vector <- rep(NA, N.ages)
                    for(i in 1:N.ages) {
                        vector[i] <- sum(data==i)/length(data)
                    }
                    vector[is.na(vector)] <- 0
                    round(vector,4)
                }
                mortprof(data)
                mortality.matrix <- matrix(NA, ncol = N.ages, nrow = iter)
                mortality.matrix[1,] <- mortprof(data)
                for(i in 2:iter){
                    bootstrap <- sample(1:length(data), length(data), replace = TRUE)
                    mortality.matrix[i,] <- unlist(mortprof(data[bootstrap]))
                }
                ci<-ci/100
                upCI<-apply(mortality.matrix[,], MARGIN = 2, FUN = quantile,
                            probs = ci+((1-ci)/2))
                loCI<-lo<-apply(mortality.matrix[,], MARGIN = 2, FUN = quantile,
                                probs = ((1-ci)/2))
                if (plot==TRUE){
                    ylab="Mortality"
                    xlab = "Age Class"
                    if(is.null(models) & is.null(usermod)){
                        bar<-barplot(mortality.matrix[1,],ylim=c(0,(max(upCI)+.1)),
                                     names=Labels.ageclass,ylab=ylab,
                                     xlab=xlab,beside=T)
                        g<-(max(bar)-min(bar))/110
                        for (i in 1:length(bar)){
                            lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
                            lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
                            lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
                        }
                    }
                    if(!is.null(models) & is.null(usermod)){
                        mort.model<-list(Security = c(.096, 0, .259, .265, .13, .011, .068, .053, .118),
                                         Milk = c(.53, .05, .03, .04, .07, .05, .04, .09, .1),
                                         Wool = c(.15, .10, .10, .02, .06, .07, .07, .23, .20),
                                         Catastrophic = c(.14, .13, .13, .11, .10, .08, .08, .07, .05, .04, .04, .02, .01),
                                         Attritional = c(.24, .23, .05, .02, .01, .03, .03, .05, .05, .09, .09, .08, .03))
                        dat<-rbind(mortality.matrix[1,],
                                   do.call(rbind,lapply(mort.model[models],matrix,
                                                        ncol=length(mortality.matrix[1,]),
                                                        byrow=TRUE)))
                        par(mfrow=c(length(models),1))
                        ylab="Mortality"
                        xlab = "Age Class"
                        for(j in 2:dim(dat)[1]){
                            bar<-barplot(rbind(mortality.matrix[1,],dat[j,]),ylim=c(0,(max(c(upCI,dat))+.1)),names=Labels.ageclass,ylab=ylab,
                                         xlab=xlab,beside=T,col=c("gray","black"))
                            bar<-bar[1,]
                            g<-(max(bar)-min(bar))/110
                            for (i in 1:length(bar))         {
                                lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
                                lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
                                lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
                            }
                            #lines(x = bar, y = dat[j,],  lwd = 2,col="red")
                            legend(x = "topright",
                                   fill=c(NA,"gray","black"),border=c(NA,1,1),lty=c(1,0,0),
                                   ncol=3,cex=.75,y.intersp=lsize,
                                   legend = c(paste(ci*100,"% CI",sep=""), "Observed",names(mort.model)[models][j-1]))
                        }
                        par(mfrow=c(1,1))
                    }
                    if(!is.null(usermod)){
                        mort.model<-usermod
                        models<-1:length(mort.model)
                        dat<-rbind(mortality.matrix[1,],
                                   do.call(rbind,lapply(mort.model[models],matrix,
                                                        ncol=length(mortality.matrix[1,]),
                                                        byrow=TRUE)))
                        par(mfrow=c(length(models),1))
                        ylab="Mortality"
                        xlab = "Age Class"
                        for(j in 2:dim(dat)[1]){
                            bar<-barplot(rbind(mortality.matrix[1,],dat[j,]),ylim=c(0,(max(c(upCI,dat))+.1)),names=Labels.ageclass,ylab=ylab,
                                         xlab=xlab,beside=T,col=c("gray","black") )
                            bar<-bar[1,]
                            g<-(max(bar)-min(bar))/110
                            for (i in 1:length(bar))         {
                                lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
                                lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
                                lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
                            }
                            #lines(x = bar, y = dat[j,],  lwd = 2,col="red")
                            legend(x = "topright",
                                   fill=c(NA,"gray","black"),border=c(NA,1,1),lty=c(1,0,0),
                                   ncol=3,cex=.75,y.intersp=lsize,
                                   legend = c(paste(ci*100,"% CI",sep=""), "Observed",names(mort.model)[models][j-1]))
                        }
                        par(mfrow=c(1,1))
                    }
                }
                data.LowerCI <-loCI
                data.PointValue <- mortality.matrix[1,]
                data.UpperCI <- upCI
                Taxon <- rep(unique(mortData$Genus), times = N.ages)
                Output.Matrix <- data.frame(Taxon = Taxon, AgeClassLabs = Labels.ageclass, LowerCI = data.LowerCI,
                                            PointValue = data.PointValue, UpperCI = data.UpperCI)
                return(Output.Matrix)
            }
            Output.Matrix<- mort.func(mortalityData=Data, labels=labels, models=models, ci=ci, plot=TRUE, iter=iter, usermod=NULL)
        }
        #Epiphyseal Fusion Function
        if (func == "Epiphyseal Fusion") {
            fuse.func<-function(data=Data,iter=1000,ci=95,plotci=TRUE,plot.title=NULL){
                cat(paste("Enter number of fusion groups"), "\n")
                ans<-readLines(n = 1)
                ans <- as.numeric(ans)
                fu.grps<-LETTERS[1:ans]
                ske.n<-numeric(length(fu.grps))
                for(i in 1:length(ske.n)){
                    cat(paste("Enter number of skeletal elements for fusion group"), fu.grps[i],"\n")
                    ans<-readLines(n = 1)
                    ske.n[i] <- as.numeric(ans)
                }
                ele.list<-as.list(rep(NA,length(ske.n)))
                names(ele.list)<-fu.grps
                for(i in 1: length(ske.n)){
                    cat(paste("Enter the", ske.n[i],"names of skeletal elements for fusion group"), fu.grps[i],"then press enter","\n")
                    ele.list[[i]]<-readLines(n = ske.n[i])
                }
                pctfuse<-function(dat){
                    pct.ufu<-n<-numeric(length(ele.list))
                    names(pct.ufu)<-fu.grps
                    wh<-function(it){which(dat$Element==ele.list[[i]][it])}
                    for (i in 1:length(pct.ufu)){
                        tab<-table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))])
                        fu<-tab[which(names(table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))] ))=="Fused")]
                        ufu<-tab[which(names(table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))] ))=="Unfused")]
                        if (is.nan(fu/(fu+ufu))){
                            pct.ufu[i]<-0} else {pct.ufu[i]<-fu/(fu+ufu)}
                        n[i]<-(fu+ufu)
                    }
                    return(list(pct.ufu,n))
                }
                #Bootstrapping
                boot <- matrix(NA, ncol = length(ele.list), nrow = iter)
                boot[1,] <- pctfuse(data)[[1]]
                for(i in 2:iter){
                    data.boot<-data[sample(1:dim(data)[1],dim(data)[1],replace=T),]
                    boot[i,] <- pctfuse(data.boot)[[1]]
                }
                ci<-ci/100
                upCI<-ci+((1-ci)/2)
                loCI<-((1-ci)/2)

                #Creating a Table of the Bootstrap Results
                quantilematrix <- matrix(NA, ncol = 2, nrow = length(fu.grps))
                for(i in 1:ncol(boot)){
                    quantilematrix[i,] <- quantile(boot[,i], probs = c(loCI, upCI), na.rm = T)
                }
                outputtable <- data.frame(Fusion.groups = fu.grps,
                                          Data = round(boot[1,],2),
                                          LowerCI = round(quantilematrix[,1],2), UpperCI = round(quantilematrix[,2],2),
                                          Count = pctfuse(data)[[2]])

                #Plotting the Fusion data
                ciplot<-ggplot(outputtable, aes(x = Fusion.groups, y = Data))+
                    geom_point(size = 3)+ #Adding the Points
                    geom_errorbar(aes(ymax = UpperCI, ymin = LowerCI), width = 0.2)+  #adding the 95% confidence interval bars
                        geom_text(aes(x = Fusion.groups, y = rep(1.05, length(Fusion.groups)), label = Count))+ #add in the sample size label for each fusion group
                            theme(panel.background = element_blank(), #There is no background color for this plot
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_rect(fill=NA, color = "black"),
                                  axis.title = element_text(color = "black", size = 20), #Font Size
                                  axis.text = element_text(color = "black", size = 15),
                                  axis.ticks = element_line(color = "black", size = 0.75), #The Line Thickness
                                  plot.title = element_text(color = "black", size = 24))+

#Formatting the Output
ylab("%Fused")+xlab("Fusion Group")+
ggtitle(plot.title)
                        if(plotci==TRUE){print(ciplot)}
Ouput.Matrix<-list(Output = outputtable, Bootstrap.Data = boot)
return(Ouput.Matrix)
            }
        }
        return(Output.Matrix)
    }

    models <- tclvalue(e$models)
    if (models == "NULL"){
        models <- NULL   #
    } else {
        models=as.numeric(unlist(models))
    }
    iter<-as.numeric(tclvalue(e$iter))
    assign("iter", iter, envir = e)
    conf.level<-as.numeric(tclvalue(e$conf.level))
    assign("conf.level", conf.level, envir = e)
    funcSelect <- tclvalue(e$func)
    func <- switch(funcSelect, "Survivorship" = funcSelect, "Mortality" = funcSelect, "Epiphyseal Fusion" = funcSelect)
    assign("func", funcSelect, envir = e)
    out<-runFunc(Data=e$dataFrame1,func=e$func,labels=unlist(e$dataFrame2), models=models, ci=e$conf.level, plot=TRUE, iter=e$iter)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)
    tkdestroy(e$wnd)
}

# ksfun
layout.ks<-function(e){
    e$ecdf<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "KS test Dialog")
    tkconfigure(e$layout, text = "Kolmogorov-Smirnov Test")

    #ecdfplot Checkbox
    put_label(e$layout, "ecdf plot:", 2, 0, sticky = "e")
    ecdfplot_check <-ttkcheckbutton (e$layout , variable = e$ecdf)
    tkgrid (ecdfplot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
}

run.ks <- function(e){
    varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)

    out<-suppressWarnings(ks.test(e$varValue,y="pnorm")) # suppross warnings for now
    out$data.name<-varName
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)

    #Plotting
    if(as.numeric(tclvalue(e$ecdf)>0)){
        x<-e$varValue
        ref<-rnorm(10000,mean(x),sd(x))
        plot(ecdf(x),xlim=range(c(x,ref)),main=paste("ecdf of ",varName))
        lines(ecdf(ref),col="red",lwd=2)
    }
    tkdestroy(e$wnd)
}

# tfun
layout.t<-function(e){
    e$alternative <- tclVar("two.sided");
    e$paired<- tclVar(FALSE);
    e$var <- tclVar(FALSE);
    e$conf.level <- tclVar(95);

    #Begin GUI Setup
    tkwm.title(e$wnd, "2-Sample t-test Dialog")
    tkconfigure(e$layout, text = "2-Sample t-test")

    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0, sticky = "e")
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Function Radiobutton
    put_label(e$layout, "Alternative:",4,0, sticky = "e")
    rb_frame <- ttkframe(e$layout)
    sapply(c("two.sided","less","greater"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Equal Variance Checkbox
    put_label ( e$layout , "var.equal:" , 5 , 0, sticky = "e")
    var_equal_check <-ttkcheckbutton ( e$layout , variable = e$var)
    tkgrid ( var_equal_check , row = 5 , column = 1 , stick = "w" ,padx = 2)

    #Paired or Independent Checkbox
    put_label ( e$layout , "paired:" , 6 , 0, sticky = "e")
    paired_check <-ttkcheckbutton (e$layout , variable = e$paired)
    tkgrid (paired_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
}

run.t <- function(e) {
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)

    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    paired <- as.numeric(tclvalue(e$paired))
    assign("paired", paired, envir = e)
    var<-as.numeric(tclvalue(e$var))
    assign("var", var, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-t.test(data= e$data, x=e$varValue1, y=e$varValue2, conf.level = e$conf.level, alternative = e$alternative, paired = e$paired, var.equal = e$var)
    out$data.name<-paste(varName1, "and", varName2)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)
    tkdestroy(e$wnd)
}

# onesam_tfun
layout.onesam_t<-function(e){
    e$alternative <- tclVar("two.sided");
    e$conf.level <- tclVar(95);
    e$mu <- tclVar(0);

    #Begin GUI Setup
    tkwm.title(e$wnd, "One Sample t-test Dialog")
    tkconfigure(e$layout, text = "One Sample t-test")

    #Mu Entry
    put_label(e$layout, "Mu:",2,0, sticky = "e")
    data_labels2 <- ttkentry(e$layout,
                             textvariable = e$mu, width=5)
    tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
    tkfocus(data_labels2)

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0, sticky = "e")
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Function Radiobutton
    put_label(e$layout, "Alternative:",4,0, sticky = "e")
    rb_frame <- ttkframe(e$layout)
    sapply(c("two.sided","less","greater"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Iterations for when resampling is added
    #put_label(label_frame, "Iterations:",6,0)
    #iter_frame <- tkspinbox(label_frame, from = 10, to = 10000, increment = 10, textvariable = e$iter, width = 6)
    #tkgrid(iter_frame, row = 6, column = 1, sticky="w", padx = 2)
}

run.onesam_t <- function(e) {
    varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)

    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<- t.test(data= e$dataFrame, x=varValue, mu=as.numeric(tclvalue(e$mu)), conf.level = e$conf.level, alternative = e$alternative)
    out$data.name<-paste(varName)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)
    tkdestroy(e$wnd)
}

# shapirofun
layout.shapiro<-function(e){
    e$qq<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Shapiro-Wilk's test Dialog")
    tkconfigure(e$layout, text = "Shapiro-Wilk's Test")

    #qqplot Checkbox
    put_label ( e$layout, "qqplot:", 2, 0, sticky = "e")
    qqplot_check <-ttkcheckbutton (e$layout , variable = e$qq)
    tkgrid (qqplot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
}

run.shapiro<-function(e){
    varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)

    out<-shapiro.test(varValue)
    out$data.name<-varName
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)

    #Plotting
    if(tclvalue(e$qq)>0){
        qqnorm(varValue)
        qqline(varValue, col = 2)
    }
    tkdestroy(e$wnd)
}

# bartlettsfun
layout.bartletts<-function(e){
    e$plot<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Bartlett's test Dialog")
    tkconfigure(e$layout, text = "Bartlett's Variance Test")

    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Plot Checkbox
    put_label ( e$layout , "plot:" , 3 , 0, sticky = "e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 3 , column = 1 , stick = "w" ,padx = 2)
}

run.bartletts<-function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)

    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    out<-bartlett.test(list(x=varValue1, x2=varValue2))
    out$data.name<-paste(varName1, "and", varName2)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)

    #Plotting
    if(tclvalue(e$plot)>0){
        vals<-c(varValue1, varValue2)
        names<-as.factor(c(rep(varName1,length(varValue1)),rep(varName2,length(varValue2))))
        boxplot(vals ~ names)
        stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# F_var_fun
layout.F_var <- function(e){
    e$plot<-tclVar(FALSE)
    e$alternative<-tclVar("two.sided")
    e$conf.level<-tclVar(95)

    #Begin GUI Setup
    tkwm.title(e$wnd, "F Variance Test Dialog")
    tkconfigure(e$layout, text = "F Variance Test")

    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0, sticky = "e")
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Hypothesis Radiobuttons
    put_label(e$layout, "Alternative:",4,0, sticky = "e")
    rb_frame <- ttkframe(e$layout)
    sapply(c("two.sided","less","greater"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Plot Checkbox
    put_label ( e$layout , "plot:" , 5 , 0, sticky = "e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.F_var <- function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)

    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-var.test(x=varValue1, y=varValue2,alternative=e$alternative, conf.level = e$conf.level)
    out$data.name<-paste(varName1, "and", varName2)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)

    #Plotting
    if(tclvalue(e$plot)>0){
        vals<-c(varValue1, varValue2)
        names<-as.factor(c(rep(varName1,length(varValue1)),rep(varName2,length(varValue2))))
        boxplot(vals ~ names)
        stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# LevenesVarTestFun
layout.LevenesVarTest <- function(e){
    e$plot<-tclVar(FALSE)
    e$center<-tclVar("mean")

    #Begin GUI Setup
    tkwm.title(e$wnd, "Levene's Variance Test Dialog")
    tkconfigure(e$layout, text = "Levene's Variance Test")

    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Hypothesis Radiobuttons
    put_label(e$layout, "Center:",3,0)
    rb_frame <- ttkframe(e$layout)
    sapply(c("mean","median"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$center,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 3, column = 1, sticky = "w")

    #Plot Checkbox
    put_label ( e$layout , "plot:" , 5 , 0)
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.LevenesVarTest<-function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)

    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    centselect <- tclvalue(e$center)
    center <- switch(centselect , "mean" = centselect , "median" = centselect )
    assign("center", centselect, envir = e)
    yvars<-c(e$varValue1, e$varValue2)
    factor<-as.factor(c(rep(varName1,length(varValue1)),rep(varName2,length(varValue2))))
    a<-paste("leveneTest(", "yvars", " ~ ", "factor , ", "center=",center,")",sep = "")
    out<-eval(parse(text = a))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(out)

    #Plotting
    if(tclvalue(e$plot)>0){
        boxplot(yvars ~ factor)
        stripchart(yvars ~ factor,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# summar_fun
layout.summar <- function(e){
    #Data Model: environment called "e"
    e$type<-tclVar("Compact")
    #e$conf.level<-tclVar(95)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Data Summary Dialog")
    tkconfigure(e$layout, text = "Data Summary")

    #Summary Radiobuttons
    put_label(e$layout, "Summary Type:",2,0)
    rb_frame <- ttkframe(e$layout)
    sapply(c("Compact","Detailed"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$type,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
}

run.summar <- function(e){
    skew<-function (x, na.rm = TRUE) {
        if (is.matrix(x))
            apply(x, 2, skew, na.rm = na.rm)
        else if (is.vector(x)) {
            if (na.rm)
                x <- x[!is.na(x)]
        n <- length(x)
        (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
        }
        else if (is.data.frame(x))
            sapply(x, skew, na.rm = na.rm)
        else skew(as.vector(x), na.rm = na.rm)
    }
    kurt<-function (x, na.rm = TRUE) {
        if (is.matrix(x))
            apply(x, 2, kurt, na.rm = na.rm)
        else if (is.vector(x)) {
            if (na.rm)
                x <- x[!is.na(x)]
        n <- length(x)
        n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
        }
        else if (is.data.frame(x))
            sapply(x, kurt, na.rm = na.rm)
        else kurt(as.vector(x), na.rm = na.rm)
    }

    printfun<-function(k, varName){
        summout<-c(round(var(k,na.rm=TRUE),2)
                   ,round(sd(k,na.rm = TRUE),2)
                   ,round(IQR(k,na.rm=TRUE),2)
                   ,round(mean(k,na.rm=TRUE),2)
                   ,round(median(k,na.rm=TRUE),2)
                   ,round(skew(k,na.rm=TRUE),2)
                   ,round(kurt(k,na.rm=TRUE),2)
                   )
        names(summout)<-c("Var","SD","IQR","Mean","Median","Skewness", "Kurtosis")
        cat("\n",e$type,"summary statistics of", varName,"\n\n")
        print(summout)
        cat("\n")
        cat("Percentiles","\n")
        print(quantile(k,c(0,.25,.5,.75,.9,.95,.99,1),na.rm=TRUE))
        sumar<-c(summout,quantile(k,c(0,.25,.5,.75,.9,.95,.99,1),na.rm=TRUE))
        return(sumar)
    }

    varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)

    typeselect <- tclvalue(e$type)
    type <- switch(typeselect , "Compact" = typeselect , "Detailed" = typeselect )
    assign("type", typeselect, envir = e)
    out<-printfun(varValue, varName)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}

# Assoc_fun
layout.Assoc <- function(e){
    e$pearson<-tclVar(FALSE)
    e$spearman<-tclVar(FALSE)
    e$kendall<-tclVar(FALSE)
    e$cov<-tclVar(FALSE)
    #e$gkGamma<-tclVar(FALSE)
    e$alternative<-tclVar("two.sided")
    e$conf.level<-tclVar(95)
    e$plot<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Association Test Dialog")
    tkconfigure(e$layout, text = "Association Tests")

    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Hypothesis Radiobuttons
    put_label(e$layout, "Alternative:",4,0)
    rb_frame <- ttkframe(e$layout)
    sapply(c("two.sided","less","greater"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Pearson's R Checkbox
    put_label ( e$layout , "Pearson's r:" , 5 , 0)
    pearson_check <-ttkcheckbutton (e$layout , variable = e$pearson)
    tkgrid (pearson_check , row = 5 , column = 1 , stick = "w" ,padx = 0)

    #Spearman's Rho Checkbox
    put_label ( e$layout , "Spearman's rho:" , 5 , 1)
    spearman_check <-ttkcheckbutton (e$layout , variable = e$spearman)
    tkgrid (spearman_check , row = 5 , column = 2 , stick = "w" ,padx = 0)

    #Kendall's Tau Checkbox
    put_label ( e$layout , "Kendall's tau:" , 7 , 0)
    kendall_check <-ttkcheckbutton (e$layout , variable = e$kendall)
    tkgrid (kendall_check , row = 7 , column = 1 , stick = "w" ,padx = 0)

    #Covariance Checkbox
    put_label ( e$layout , "Covariance:" , 7 , 1)
    cov_check <-ttkcheckbutton (e$layout , variable = e$cov)
    tkgrid (cov_check , row = 7 , column = 2 , stick = "w" ,padx = 0)

    #Plotting Checkbox
    put_label ( e$layout , "Plot:" , 8 , 0)
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 8 , column = 1 , stick = "w" ,padx = 0)

    # Goodman and Kruskal's Gamma Checkbox
    #put_label ( label_frame , "GK's gamma:" , 7 , 1)
    #gkGamma_check <-ttkcheckbutton (label_frame , variable = e$gkGamma)
    #tkgrid (gkGamma_check , row = 7 , column = 2 , stick = "w" ,padx = 0)
}

run.Assoc <- function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)
    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    pearson <- as.numeric(tclvalue(e$pearson))
    assign("pearson", pearson, envir = e)
    spearman <- as.numeric(tclvalue(e$spearman))
    assign("spearman", spearman, envir = e)
    kendall <- as.numeric(tclvalue(e$kendall))
    assign("kendall", kendall, envir = e)
    covar <- as.numeric(tclvalue(e$cov))
    assign("cov", covar, envir = e)
    #gkgamma <- as.numeric(tclvalue(e$gkGamma))
    #assign("gkgamma", gkgamma, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    if(e$pearson==1){
        out1<- cor.test(x=varValue1, y=varValue2,
                        alternative = e$alternative,
                        method = "pearson",
                        exact = NULL, conf.level = e$conf.level)
        out1$data.name<-paste(varName1, "and", varName2)
        pos<-1
        envir <- as.environment(pos)
        assign("Pearson_Results", out1, envir = envir)
        print(out1)
    }
    if(e$spearman==1){
        out2<- cor.test(x=varValue1, y=varValue2,
                        alternative = e$alternative,
                        method = "spearman",
                        exact = TRUE, conf.level = e$conf.level)
        out2$data.name<-paste(varName1, "and", varName2)
        pos<-1
        envir <- as.environment(pos)
        assign("Spearman_Results", out2, envir = envir)
        print(out2)
    }
    if(e$kendall==1){
        out3<- cor.test(x=varValue1, y=varValue2,
                        alternative = e$alternative,
                        method = "kendall",
                        exact = TRUE, conf.level = e$conf.level)
        out3$data.name<-paste(varName1, "and", varName2)
        pos<-1
        envir <- as.environment(pos)
        assign("Kendall_Results", out3, envir = envir)
        print(out3)
    }
    if(e$cov==1){
        out4<- cov(x=varValue1, y=varValue2, use = "everything",
                   method = "pearson")
        names(out4)<-paste("Covariance of ",varName1, "and", varName2)
        pos<-1
        envir <- as.environment(pos)
        assign("Covariance_Results", out4, envir = envir)
        print(out4)
    }
    #Plotting
    if(as.numeric(tclvalue(e$plot))>0){
        plot(x=varValue1, y=varValue2,xlab=varName1, ylab=varName2)
    }
    tkdestroy(e$wnd)
}

# Fisher_exact_fun
layout.Fisher_exact <- function(e){
    e$plot<-tclVar(FALSE)
    e$alternative<-tclVar("two.sided")
    e$conf.level<-tclVar(95)
    e$simulate<-tclVar(FALSE)
    e$nsims<-tclVar(0)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Fisher's Exact Test Dialog")
    tkconfigure(e$layout, text = "Fisher's Exact Test")
    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Conf.level Slider
    put_label(e$layout, "conf.level:", 3, 0)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    conf_level_scale <- ttkscale(conf_level_frame,
                                 from = 75, to = 100,
                                 variable = e$conf.level)
    #Conf.Level Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame,
                                 from = 75, to = 100, increment = 1,
                                 textvariable = e$conf.level, width = 5)
    tkpack(conf_level_scale, side = "left")
    tkpack(conf_level_spin, side = "left")

    #Hypothesis Radiobuttons
    put_label(e$layout, "Alternative:",4,0)
    rb_frame <- ttkframe(e$layout)
    sapply(c("two.sided","less","greater"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
               tkpack(radio_button, side = "left")
                                 })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Monte Carlo Slider
    put_label(e$layout, "Monte Carlo:", 5, 0)
    MC_frame <- ttkframe(e$layout)
    tkgrid(MC_frame, row = 5, column = 1, columnspan = 2,
           sticky = "w")
    MC_scale <- ttkscale(MC_frame,
                         from = 0, to = 10000,
                         variable = e$nsims)
    #Monte Carlo Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    MC_spin <- tkspinbox(MC_frame,
                         from = 0, to = 10000, increment = 1,
                         textvariable = e$nsims, width = 5)
    tkpack(MC_scale, side = "left")
    tkpack(MC_spin, side = "left")

    #Plot Checkbox
    #put_label ( label_frame , "plot:" , 6 , 0)
    #plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
    #tkgrid (plot_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
}

run.Fisher_exact <- function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)
    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)

    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    nsims<-tclvalue(e$nsims)
    assign("nsims", nsims, envir = e)
    simulate<-as.logical(as.numeric(tclvalue(e$simulate)))
    assign("simulate", simulate, envir = e)
    if(e$nsims>0){
        e$simulate<-TRUE
    }
    tab<-table(x=varValue1, y=varValue2)
    names(attributes(tab)$dimnames)<-c(varName1,varName2)
    out<-fisher.test(tab,alternative = e$alternative, conf.int = TRUE, conf.level = e$conf.level, simulate.p.value=e$simulate, B=e$nsims)
    out$data.name<-paste(varName1, "and", varName2)
    out<-list(Table=tab, Test=out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    cat("\nTabulated Data\n")
    print(tab)
    cat("\n")
    print(out$Test)

    #Plotting
    if(tclvalue(e$plot)>0){
        vals<-c(varValue1, varValue2)
        names<-as.factor(c(rep(varName1,length(varValue1)),rep(varName2,length(varValue2))))
        boxplot(vals ~ names)
        stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# chisq_fun
layout.chisq<-function(e){
    e$plot<-tclVar(FALSE)
    e$correct<-tclVar(FALSE)
    e$simulate<-tclVar(FALSE)
    e$nsims<-tclVar(0)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Pearson's Chi-squared Test Dialog")
    tkconfigure(e$layout, text = "Pearson's Chi-squared Test")
    tkconfigure(e$varLabel[[1]], text = "Variable 1:")
    tkconfigure(e$varLabel[[2]], text = "Variable 2:")

    #Monte Carlo Slider
    put_label(e$layout, "Monte Carlo:", 3, 0)
    MC_frame <- ttkframe(e$layout)
    tkgrid(MC_frame, row = 3, column = 1, columnspan = 2,
           sticky = "w")
    MC_scale <- ttkscale(MC_frame,
                         from = 0, to = 10000,
                         variable = e$nsims)
    #Monte Carlo Spinbox
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    MC_spin <- tkspinbox(MC_frame,
                         from = 0, to = 10000, increment = 1,
                         textvariable = e$nsims, width = 5)
    tkpack(MC_scale, side = "left")
    tkpack(MC_spin, side = "left")

    #Yate's Correction Checkbox
    put_label ( e$layout , "Yate's Correction:" , 4 , 0)
    yates_check <-ttkcheckbutton (e$layout , variable = e$correct)
    tkgrid (yates_check , row = 4 , column = 1 , stick = "w" ,padx = 2)

    #Plot Checkbox
    #put_label ( label_frame , "plot:" , 6 , 0)
    #plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
    #tkgrid (plot_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
}

run.chisq<-function(e){
    varName1 <- getVarName(e, 1)
    varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)
    varName2 <- getVarName(e, 2)
    varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)
    correct<-as.logical(as.numeric(tclvalue(e$correct)))
    assign("correct", correct, envir = e)
    nsims<-as.numeric(tclvalue(e$nsims))
    assign("nsims", nsims, envir = e)
    simulate<-as.logical(as.numeric(tclvalue(e$simulate)))
    assign("simulate", simulate, envir = e)
    if(e$nsims>0){
        e$simulate<-TRUE
    }
    tab<-table(x=varValue1, y=varValue2)
    names(attributes(tab)$dimnames)<-c(varName1,varName2)
    out<-chisq.test(tab, correct=e$correct, simulate.p.value=e$simulate, B=e$nsims,rescale.p = FALSE)
    out$pvals<-out$stdres
    out$pvals[out$stdres<0]<-pnorm(out$stdres[out$stdres<0],lower.tail = TRUE)
    out$pvals[out$stdres>0]<-pnorm(out$stdres[out$stdres>0],lower.tail = FALSE)
    out$data.name<-paste(varName1, "and", varName2)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    cat("\nObserved Data\n")
    print(tab)
    cat("\n")
    print(out)

    #Plotting
    if(as.numeric(tclvalue(e$plot))>0){
        vals<-c(varValue1, varValue2)
        names<-as.factor(c(rep(varName1,length(varValue1)),rep(varName2,length(varValue2))))
        boxplot(vals ~ names)
        stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# oneway_fun
layout.oneway<-function(e){
    e$plot<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "One-way ANOVA Dialog")
    tkconfigure(e$layout, text = "One-way ANOVA")
    tkconfigure(e$varLabel[[1]], text = "Response:")
    tkconfigure(e$varLabel[[2]], text = "Variable 1:")

    #Plot Checkbox # not yet operational (within OK function)
    put_label ( e$layout , "plot:" , 5 , 0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.oneway<-function(e){
    respVarName <- getVarName(e, 1)
    varName1 <- getVarName(e, 2)

    f1<-paste(respVarName, "~", varName1)
    f1<-eval(parse(text=f1))
    out<-lm(f1,data=e$dataFrame)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(anova(out))
    print(summary(out))

    #Plotting function bells and whistles after the analytical stuff is up to "snuff"
    if(as.numeric(tclvalue(e$plot))>0){
        ci.bars<-function(ybar,ci,label,ylab) {
            xv<-barplot(ybar,ylim=c(0,max(ybar+ci)),names=label,ylab=ylab,beside=T)
            g<-(max(xv)-min(xv))/50
            for (i in 1:length(xv)){
                lines(c(xv[i],xv[i]),c(ybar[i]+ci[i],ybar[i]-ci[i]))
                lines(c(xv[i]-g,xv[i]+g),c(ybar[i]+ci[i], ybar[i]+ci[i]))
                lines(c(xv[i]-g,xv[i]+g),c(ybar[i]-ci[i], ybar[i]-ci[i]))
            }
        }
        me<-aggregate(f1,data=e$dataFrame,FUN=mean)
        gnames<-as.character(me[,1])
        gmeans<-(me[,2])
        ci<- (aggregate(f1,data=e$dataFrame,FUN=sd)[,2]/aggregate(f1,data=e$dataFrame,FUN=length)[,2])*1.96
        ci.bars(gmeans,ci,gnames,ylab="response")
    }
    tkdestroy(e$wnd)
}

# fact_fun
layout.fact<-function(e){
    e$plot<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Factorial ANOVA Dialog")
    tkconfigure(e$layout, text = "Two way ANOVA")
    tkconfigure(e$varLabel[[1]], text = "Response:")
    tkconfigure(e$varLabel[[2]], text = "Variable 1:")
    tkconfigure(e$varLabel[[3]], text = "Variable 2:")

    #Plot Checkbox #not yet operational (within OK function)
    put_label ( e$layout , "plot:" , 5 , 0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.fact<-function(e){
    if(tclvalue(e$interaction)!="NULL" & getVarName(e, 3)!="Choose Factor 2"){
        f1<-paste(getVarName(e, 1),"~",
                  getVarName(e, 2),
                  tclvalue(e$interaction),
                  getVarName(e, 3))
        f1<-eval(parse(text=f1))
    }
    if(tclvalue(e$interaction)=="NULL"){
        f1<-paste(getVarName(e, 1),"~",
                  getVarName(e, 2))
        f1<-eval(parse(text=f1))
    }
    out<-lm(f1,data=e$dataFrame)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(anova(out))
    print(summary(out))
    tkdestroy(e$wnd)
}

# simp_lmfun
layout.simp_lm<-function(e){
    e$plot<-tclVar(FALSE)

    #Begin GUI Setup
    tkwm.title(e$wnd, "Simple Linear Regression Dialog")
    tkconfigure(e$layout, text = "Simple Linear Regression")
    tkconfigure(e$varLabel[[1]], text = "Response:")
    tkconfigure(e$varLabel[[2]], text = "Predictor Variable:")

    #Plot Checkbox
    put_label ( e$layout , "plot:" , 5 , 0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.simp_lm<-function(e){
    respVarName <- getVarName(e, 1)
    preVarName <- getVarName(e, 2)

    f1<-eval(paste(respVarName, "~", preVarName))
    out<-lm(f1,data=e$dataFrame)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(anova(out))
    print(summary(out))
    #plotting function bells and whistles after the analytical stuff is up to "snuff"
    if(as.numeric(tclvalue(e$plot))>0){
        #graph viewer
        # hscale <- 2    # Horizontal scaling
        #vscale <- 2   # Vertical scaling
        # plot.shap <- function() {
        ###plotting functions
        #}
        #graph <- tkrplot(label_frame, fun = plot.shap,
        #                 hscale = hscale, vscale = vscale)
        #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
        e$dataFrame$pred<-predict(out,se.fit = TRUE)$fit
        e$dataFrame$hici<-e$dataFrame$pred + 1.96*predict(out,se.fit = TRUE)$se.fit
        e$dataFrame$loci<-e$dataFrame$pred - 1.96*predict(out,se.fit = TRUE)$se.fit
        e$dataFrame<-e$dataFrame[order(e$dataFrame$pred),]
        x<-e$dataFrame[,which(names(e$dataFrame)==tclvalue(preVarName))]
        y<-e$dataFrame[,which(names(e$dataFrame)==tclvalue(respVarName))]
        xpred<-data.frame(xpred=seq(min(x),max(x),length.out = 1000))
        colnames(xpred)<-tclvalue(preVarName)
        predi<-predict(out , se.fit=TRUE,newdata = xpred)
        ypred<-predi$fit
        ypredlo<-ypred - predi$se.fit*1.96
        ypredhi<-ypred + predi$se.fit*1.96
        plot(xpred[,1],ypred,type="n",xlab=tclvalue(preVarName),
             ylab=tclvalue(respVarName),ylim=range(c(ypredlo,ypredhi)))
        polygon(c(xpred[,1],rev(xpred[,1])),c(ypredlo,rev(ypredhi))
                ,col="light gray",border=NA)
        points(x, y,bg=" dark gray",pch=21,cex=1)
        lines(xpred[,1], ypred,col="black",lwd=2)
        #ggplot(e$dataFrame, aes(x)) +
        # geom_point(aes(x=x,y=y), colour="black", size=2) +
        #geom_ribbon(aes(ymin=loci, ymax=hici), alpha=0.7,fill="dark gray")+
        #geom_line(aes(y=pred), colour="red",size=1.5) +
        #ylab(tclvalue(e$responsevariablename))+xlab(tclvalue(e$varName1))+
        #theme_bw() +
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #     panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    tkdestroy(e$wnd)
}

# glmfun to be used for simple two-variable regression and two-way anova with interaction effects
layout.glm<-function(e){
    e$distfx<-tclVar("gaussian")
    e$linkfx<-tclVar("identity")
    e$plot<-tclVar(FALSE)  

    #Begin GUI Setup
    tkwm.title(e$wnd, "Generalized Linear Model Dialog")
    tkconfigure(e$layout, text = "Generalized Linear Model")
    tkconfigure(e$varLabel[[1]], text = "Response:")
    tkconfigure(e$varLabel[[2]], text = "Variable 1:")
    tkconfigure(e$varLabel[[3]], text = "Variable 2:")

    #Distribution Combobox (Family)
    put_label(e$layout, "Distribution:",row=3,column=1,sticky="n")
    data_labels5 <- ttkcombobox(e$layout, state="readonly",
                                values = c("gaussian","binomial","poisson", "Gamma"),
                                textvariable = e$distfx,width=25)
    tkgrid(data_labels5, row = 4, column = 1, sticky = "ew", padx = 2)
    tkfocus(data_labels5)

    #Link Combobox (transform function)
    put_label(e$layout, "Link:",row=3,column=2,sticky="n")
    data_labels5 <- ttkcombobox(e$layout, state="readonly",
                                values = c("identity", "logit","log","inverse"),
                                textvariable = e$linkfx,width=25)
    tkgrid(data_labels5, row = 4, column = 2, sticky = "ew", padx = 2)
    tkfocus(data_labels5)

    #Plot Checkbox # not yet operational (within OK function)
    put_label ( e$layout , "plot:" , 5 , 0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
}

run.glm<-function(e){
    if(tclvalue(e$interaction)!="NULL" & getVarName(e, 3)!="Choose Predictor Variable 2"){
        f1<-paste(getVarName(e, 1),"~",
                  getVarName(e, 2),
                  tclvalue(e$interaction),
                  getVarName(e, 3))
        f1<-eval(parse(text=f1))
    }
    if(tclvalue(e$interaction)=="NULL"){
        f1<-paste(getVarName(e, 1),"~",
                  getVarName(e, 2))
        f1<-eval(parse(text=f1))
    }
    #Link Function Inside Distribution
    lin<-eval(parse(text=paste(tclvalue(e$distfx),"(link=",tclvalue(e$linkfx),")",sep="")))
    out<-glm(f1,data=e$dataFrame,family=lin)
    out$anovatab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    print(anova(out))
    print(summary(out))
    tkdestroy(e$wnd)
}

