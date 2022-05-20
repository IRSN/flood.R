#' @test map_geo("Geo_Loire_V6.slf")
#'       p = read_poi("Loire.poi")
#'       for (s in names(p)) {
#'         points(p[[s]][1],p[[s]][2], pch=20)
#'         text(p[[s]][1]+0,p[[s]][2]+300,labels=s,cex=0.75)
#'       }
read_poi = function(file.poi, lonlat=NULL) {
  p_txt=readChar(file.poi, file.info(file.poi)$size); 
  p = list()
  for (pi_txt in strsplit(p_txt,"\n",fixed=T)[[1]]) {
    if (!any(is.na(as.numeric(strsplit(gsub("(.*)=","",pi_txt),",",fixed=T)[[1]])))) # not yet supports x1,y1:nx,ny:x2,y2 format
    eval(parse(text=
                 paste0("p$",
                        gsub("=","=c(",pi_txt,fixed=T),")")
                 ))
  }
  if (is.null(lonlat))
    return(p)
  else
    return(lapply(p,lonlat))
}

#' @test map_geo("Geo_Loire_V6.slf")
#'       C = read_calage("user_fortran/calage_TVF_1.f")
#'       for (i in 1:8) {
#'         polygon(x=C[[paste0("XSOM",i)]],y=C[[paste0("YSOM",i)]],col = rgb(0,0,0,0.05),border = 'gray')
#'         text(mean(C[[paste0("XSOM",i)]])+0,mean(C[[paste0("YSOM",i)]])+0,label=paste0("ks",i),col='darkgray',cex=0.5)
#'       }
read_calage = function(calage.f, lonlat=NULL) {
  calage = readChar(calage.f, file.info(calage.f)$size)
  calage = strsplit(calage,"\n",fixed=T)[[1]]
  XY=list()
  for (i in 1:length(calage)) {
    # print(calage_TVF_1[i])
    if (nchar(calage[i])>0)
    if (isTRUE(grep("(.+)\\(\\d+\\)(\\s*)=(\\s*)(\\d+)",calage[i])>0))
    eval(parse(text=
                 paste0("XY$",
                        gsub(" ","",fixed=T,gsub(")","]",fixed=TRUE, gsub("(","[",fixed = TRUE,x=calage[i]))))
               ))
  }
  if (is.null(lonlat))
    return(XY)
  else
    return(lapply(XY,lonlat))
}