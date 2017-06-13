# srarefun
layout.s_rare<-function(e){
    e$plot<-tclVar(FALSE)
    e$mval<-tclVar(50)
    e$conf.level<-tclVar(95)

    tkwm.title(e$wnd, "Rarefaction Dialog")
    tkconfigure(e$layout, text = "Single Sample Rarefaction")

    #M Value Slider
    put_label(e$layout, "m value:", 2, 0)
    m_value_frame <- ttkframe(e$layout)
    tkgrid(m_value_frame, row = 2, column = 1, columnspan = 2, 
           sticky = "w")
    m_value_scale <- ttkscale(m_value_frame, 
                              from = 0, to = 1000,  
                              variable = e$mval)
    #M Value Spinbox 
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    m_value_spin <- tkspinbox(m_value_frame, 
                              from = 0, to = 1000, increment = 1, 
                              textvariable = e$mval, width = 5)
    tkpack(m_value_scale, side = "left")
    tkpack(m_value_spin, side = "left")

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

    #Plot Checkbox
    put_label ( e$layout , "plot:" , 4 , 0)
    ecdfplot_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (ecdfplot_check , row = 4 , column = 1 , stick = "w" ,padx = 2)
}

#OK Function
run.s_rare<-function(e){
    rarefac.fun <- function(data=e$dataFrame, m=e$mval, ci = e$conf.level){
        k <- data[data > 0] #species frequencies
        n <- sum(k) #total site size
        f0.est <- function(f1,f2){
            ifelse(f2>0,(f1^2)/(2*f2),(f1*(f1-1)))
        }
        f1 <- sum(k==1)
        f2 <- sum(k==2)
        f0 <- f0.est(f1,f2)
        S.est <- length(k)+f0
        alphakm <- function(k,m,n){
            res <- c()
            for(i in 1:length(k)){
                res[i] <- ifelse(n - m < k[i],0,exp((lfactorial(n-k[i])+lfactorial(n-m))-(lfactorial(n)+lfactorial(n-k[i]-m))))
            }
            res
        }
        se.rare <- function(k,m,n,S.est){
            sqrt(sum((1-alphakm(k,m,n))^2)-(sum(1-alphakm(k,m,n)))^2/S.est)
        }
        extrap <- function(f0,f1, mstar, n){
            f0*(1 - (1 - (f1/(n*f0)))^mstar)
        }
        se.extrap <- function(k, mstar, n, S.est){
            f1.2.low <- matrix(c(sum(k==1),sum(k==2)),nrow=max(k),ncol=2,byrow=TRUE)
            f1.2.high <- matrix(c(sum(k==1),sum(k==2)),nrow=max(k),ncol=2,byrow=TRUE)
            f1.2.low[1,1] <- max(f1.2.low[1,1]-1,0)
            f1.2.low[2,2] <- max(f1.2.low[2,2]-1,0)
            f1.2.high[1,1] <- f1.2.high[1,1]+1
            f1.2.high[2,2] <- f1.2.high[2,2]+1
            ds.df <- c()
            for(i in 1:max(k)){

                #Low Estimate
                f0.low <- f0.est(f1.2.low[i,1],f1.2.low[i,2])
                S.est.low <- length(k)-1+f0.low
                S.low <- length(k)-1+extrap(f0.low,f1.2.low[i,1],mstar,n)

                #High Estimate
                f0.high <- f0.est(f1.2.high[i,1],f1.2.high[i,2])
                S.est.high <- length(k)+1+f0.high
                S.high <- length(k)+1+extrap(f0.high,f1.2.high[i,1],mstar,n)
                ds.df[i] <- (S.high - S.low)/2
            }
            #Calculating the Variances
            if(length(k) == 1){
                var.extrap <- (ds.df[1]^2)*(1 - k/S.est)
            }
            if(length(k) > 1){
                offdag <- c()
                for(i in 1:max(k)-1){
                    for(j in (i+1):max(k)){
                        offdag <- c(offdag, (ds.df[i]*ds.df[j])*(-sum(k==i)*sum(k==j)/S.est))
                    }
                }
                dag <- c()
                for(i in 1:max(k)){
                    dag <- c(dag,(ds.df[i]^2)*sum(k==i)*(1 - sum(k==i)/S.est))
                }
                var.extrap <- sum(dag)+2*sum(offdag)
            }
            sqrt(var.extrap)
        }
        outpt <- data.frame(m = 0, S.ind = 0, SE = 0, CImin = 0, CImax = 0)
        for(i in 1:m){
            if(i < n){ #rarefaction
                outpt[(i+1),1] <- i
                outpt[(i+1),2] <- sum(1-alphakm(k,i,n)) #estimate
                outpt[(i+1),3] <- se.rare(k,i,n,S.est) #standard error
                outpt[(i+1),4] <- outpt[(i+1),2]+qnorm((1-ci)/2)*outpt[(i+1),3]
                outpt[(i+1),5] <- outpt[(i+1),2]+qnorm(1-(1-ci)/2)*outpt[(i+1),3]
            }
            if(i >= n){ #extrapolation
                mstar <- i - n
                outpt[(i+1),1] <- i
                outpt[(i+1),2] <- length(k) + extrap(f0,f1,mstar,n) #estimate
                outpt[(i+1),3] <- se.extrap(k, mstar, n, S.est) #standard error
                outpt[(i+1),4] <- outpt[(i+1),2]+qnorm((1-ci)/2)*outpt[(i+1),3]
                outpt[(i+1),5] <- outpt[(i+1),2]+qnorm(1-(1-ci)/2)*outpt[(i+1),3]
            }
        }
        return(outpt)
    }
	
    variablename<-getVarName(e, 1)
    variablevalue <- get(variablename,e$dataFrame,inherits=TRUE)
	if(is.numeric(variablevalue) == FALSE){
      tkdestroy(e$wnd)
      stop("Variable must be numeric",call. = FALSE)
    }
    assign("variablevalue", variablevalue, envir = e)
    e$mval<-as.numeric(tclvalue(e$mval))
    e$conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    out<-rarefac.fun (data=e$dataFrame, m=e$mval, ci = e$conf.level)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)

    #Plotting
    if(tclvalue(e$plot)>0){
        plot(out$m,out$S.ind,type="n" 
             ,main=paste("Rarefaction of ",variablename)
             ,ylim=range(c(out$CImin,out$CImax))
             ,ylab="S",xlab="m")
        polygon(c(out$m,rev(out$m)),c(out$CImin,rev(out$CImax)),col="light gray",border=NA)
        lines(out$m,out$S.ind,col="red",lwd=2,lty=2)
        lines(out$m,out$CImin,col="black",lwd=2,lty=2)
        lines(out$m,out$CImax,col="black",lwd=2,lty=2)#
    }
    tkdestroy(e$wnd)
}

