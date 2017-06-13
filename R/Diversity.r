# divind_fun
layout.divind<-function(e){
    e$index<-tclVar("shannon")
    e$indval<-tclVar(0)
    e$append<-tclVar(FALSE)

    tkwm.title(e$wnd, "Diversity Index Dialog")
    tkconfigure(e$layout, text = "Diversity Index")

    #Div Index Radiobutton
    put_label(e$layout, "Index:",1,0,sticky="e")  
    rb_frame <- ttkframe(e$layout)
    sapply(c("shannon", "simpson", "isimpson", "margalef", "menhinick"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$index,text = i, value = i)
               tkpack(radio_button, side = "left")
                              })
    tkgrid(rb_frame, row = 1, column = 1, sticky = "w")

    #Append Checkbox
    put_label ( e$layout , "Append:" , 2 , 0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$append)
    tkgrid (plot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
}

#OK Function
run.divind<-function(e){
    divindex<-function (A=e$dataFrame, index=tclvalue(e$index)) {
        INDICES <- c("shannon", "simpson", "isimpson", "margalef", "menhinick")
        index <- match.arg(index, INDICES)
        if (length(dim(A)) > 1) {
            M<-A/rowSums(A)
        }
        d<-switch(index,
                  shannon = -rowSums(M*log(M),na.rm=T),
                  simpson = 1 - rowSums(M*M, na.rm = TRUE),
                  isimpson = 1/rowSums(M*M, na.rm = TRUE),
                  margalef = (rowSums(A>0)-1)/log(rowSums(A, na.rm = TRUE)),
                  menhinick = rowSums(A>0)/sqrt(rowSums(A, na.rm = TRUE)))
        k<-switch(index,
                  shannon = assign("indval", d, envir = e),
                  simpson = assign("indval", d, envir = e),
                  isimpson = assign("indval", d, envir = e),
                  margalef = assign("indval", d, envir = e),
                  menhinick = assign("indval", d, envir = e) )
    }
    out<-divindex()
    cat(tclvalue(e$index),out, "\n")
    if(tclvalue(e$append)>0){
        eval(parse(text=paste("dataFrame$",tclvalue(e$index),"<-out",sep="")))
        pos<-1
        envir <- as.environment(pos)
        assign(tclvalue(e$dataName), e$dataFrame, envir=envir)
        } else {pos<-1
        envir <- as.environment(pos)
        assign("Results", out, envir = envir) 
    }
    tkdestroy(e$wnd)
}

