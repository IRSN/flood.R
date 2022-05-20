#' @test map_geo("Geo_Loire_V6.slf")
#'       p= read_poi("Loire.poi")
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
