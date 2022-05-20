# coord converter function (assumed linear)
xy2lonlat = function(xy1,lonlat1,xy2,lonlat2) {
  return(function(xy){
    x=xy[,1]; y=xy[,2]
    cbind(
      (x-xy1[1])/(xy2[1]-xy1[1]) * (lonlat2[1]-lonlat1[1]) +lonlat1[1],
      (y-xy1[2])/(xy2[2]-xy1[2]) * (lonlat2[2]-lonlat1[2]) +lonlat1[2])
  })
}

#' @test map_geo(Geo_Loire_V6.slf, xy1=c(664649.1875,	6736072.5),
#'                                 lonlat1=c(2.528116,47.723969),
#'                                 xy2=c(635994.0,	6750523.0),
#'                                 lonlat2=c(2.144066829311161,47.85201791739933),
#'                                 raster=raster::getData(name="GADM", country="FRA", level=0))
map_geo = function(geo.slf, xy1, lonlat1,xy2,lonlat2, raster=NULL, title=NULL) {
  geo = telemac::read_geo(geo.slf)

  col.fun = colorRamp(c("blue", "green", "red"))
  col.val = ( (geo$elevation-quantile(geo$elevation,0.001)) / (max(geo$elevation)-min(geo$elevation)) )
  cols = col.fun(col.val)/255
  cols[is.na(cols)] = 0

  geo_lonlat = lonlat(cbind(geo$x,geo$y))

  if (is.null(raster)) {
    plot(apply(min,1,geo_lonlat),apply(max,1,geo_lonlat),type='n',main=title)
  } else {
    if (typeof(raster)!="raster") 
      stop("raster should come from raster::getData")
    plot(raster, main=title)
  }

  points(geo_lonlat[,1],geo_lonlat[,2], col = rgb(cols[,1],cols[,2],cols[,3],0.1),pch='.')
}
