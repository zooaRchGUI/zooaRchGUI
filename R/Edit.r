# sort
layout.sort<-function(e){
  e$direct1<-tclVar("Increasing");
  e$direct2<-tclVar("Increasing");
  e$direct3<-tclVar("Increasing");
 
  tkwm.title(e$wnd, "Sort Dialog")
  tkconfigure(e$layout, text = "Sort:") 
  
  #Direction Radiobuttons
  #put_label(e$layout, "Direction:",row=1,column=2,sticky="e")
  rb_frame <- ttkframe(e$layout)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$direct1,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 1, column = 2, sticky = "w")
  
  #Direction Radiobuttons
  #put_label(e$layout, "Direction:",row=1,column=2,sticky="e")
  rb_frame2 <- ttkframe(e$layout)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame2, variable = e$direct2,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame2, row = 2, column = 2, sticky = "w")
  
  #Direction Radiobuttons
  #put_label(e$layout, "Direction:",row=1,column=2,sticky="e")
  rb_frame3 <- ttkframe(e$layout)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame3, variable = e$direct3,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame3, row = 3, column = 2, sticky = "w")
}

run.sort<-function(e){
    print(paste(getVarName(e, 1), getVarName(e, 2), getVarName(e, 3), sep=" "))
    nams<-c(getVarName(e, 1), getVarName(e, 2), getVarName(e, 3))
    cnames<-c(getVarName(e, 1), getVarName(e, 2), getVarName(e, 3)) !="Choose Variable"
    nams<-nams[cnames]
    direct<-c(tclvalue(e$direct1), tclvalue(e$direct2), tclvalue(e$direct3))[cnames]=="Decreasing"
    str<-NULL
    if(length(nams)>1){
        for(i in 1:length(nams)){
            str<-c( str,paste("xtfrm(e$dataFrame","$",nams[i],")",sep="") )
        }
        } else {str<-paste("xtfrm(e$dataFrame","$",nams[1],")",sep="")
    }
    if (sum(direct)>=1){
        str[which(direct==TRUE)]<-paste("-",str[which(direct==TRUE)],sep="")
    }
    str<-paste(str,collapse = ",")
    obj<-paste("e$dataFrame","[order(",str,"),]",sep="")
    #obj
    dataFrame<-eval(parse(text=obj))
    pos<-1
    envir <- as.environment(pos)
    assign(tclvalue(e$dataName), dataFrame, envir)
    View(as.name(tclvalue(e$dataName)))
    cat("Display Heads ","\n")
    print(head(dataFrame))
    cat(" ","\n")
    cat("Display Tails ","\n")
    print(tail(dataFrame))
    tkdestroy(e$wnd)
}


