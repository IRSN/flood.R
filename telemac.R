#' @test map_geo("Geo_Loire_V6.slf", xy1=c(664649.1875,	6736072.5),
#'                                 lonlat1=c(2.528116,47.723969),
#'                                 xy2=c(635994.0,	6750523.0),
#'                                 lonlat2=c(2.144066829311161,47.85201791739933))
#' p= read_poi("Loire.poi")
#' for (s in names(p)) {
#'   points(p[[s]][1],p[[s]][2], pch=20)
#'   text(p[[s]][1]+0,p[[s]][2]+300,labels=s,cex=0.75)
#' }
read_poi = function(file.poi) {
  p_txt=readChar(file.poi, file.info(file.poi)$size); 
  p = list()
  for (pi_txt in strsplit(p_txt,"\n",fixed=T)[[1]]) {
    if (!any(is.na(as.numeric(strsplit(gsub("(.*)=","",pi_txt),",",fixed=T)[[1]])))) # not yet supports x1,y1:nx,ny:x2,y2 format
    eval(parse(text=
                 paste0("p$",
                        gsub("=","=c(",pi_txt,fixed=T),")")
                 ))
  }
  return(p)
}

