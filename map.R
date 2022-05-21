#'' coord converter function (assumed linear)
#' @test lonlat = xy2lonlat(xy1=c(664649.1875,	6736072.5),
#'                          lonlat1=c(2.528116,47.723969),
#'                          xy2=c(635994.0,	6750523.0),
#'                          lonlat2=c(2.144066829311161,47.85201791739933))
xy2lonlat = function(xy1,lonlat1,xy2,lonlat2) {
  return(function(xy){
    xy = matrix(xy,ncol=2)
    x=xy[,1]; y=xy[,2]
    cbind(
      (x-xy1[1])/(xy2[1]-xy1[1]) * (lonlat2[1]-lonlat1[1]) +lonlat1[1],
      (y-xy1[2])/(xy2[2]-xy1[2]) * (lonlat2[2]-lonlat1[2]) +lonlat1[2])
  })
}

#' @test map_geo("Geo_Loire_V6.slf")
#' @test map_geo("Geo_Loire_V6.slf",xy2lonlat(c(664649.1875,	6736072.5),c(2.528116,47.723969),c(635994.0,	6750523.0),c(2.144066829311161,47.85201791739933) ))
#' @test map_geo("Geo_Loire_V6.slf", lonlat, raster=raster::getData(name="GADM", country="FRA", level=5))
map_geo = function(geo.slf, lonlat=NULL, raster="openstreetmap", col.alpha=0.15, pch='.', title=NULL,xlab=if (is.null(lonlat)) "x" else "lon",ylab=if (is.null(lonlat)) "y" else "lat",...) {
  geo = telemac::read_geo(geo.slf)
  
  col.fun = colorRamp(c("blue", "red", "green"))
  col.val = ( (geo$elevation-quantile(geo$elevation,0.001)) / (max(geo$elevation)-min(geo$elevation)) )
  cols = col.fun(col.val)/255
  cols[is.na(cols)] = 0
  
  if (is.null(lonlat))
    geo_lonlat = cbind(geo$x,geo$y)
  else 
    geo_lonlat = lonlat(cbind(geo$x,geo$y))
  
  if (is.null(raster)) {
    plot(apply(geo_lonlat,1,min),apply(geo_lonlat,1,max),type='n',main=title, xlab=xlab,ylab=ylab, ...)
  } else if (raster=="openstreetmap" && !is.null(lonlat)) {
    m = OpenStreetMap::openmap(upperLeft = c(max(geo_lonlat[,2]), min(geo_lonlat[,1])),
                                lowerRight = c(min(geo_lonlat[,2]), max(geo_lonlat[,1])),
                               type="osm",
                                minNumTiles=10)
    plot(OpenStreetMap::openproj(m),main=title, xlab=xlab,ylab=ylab, ...)
  } else {
    #if (typeof(raster)!="raster") 
    #  stop("raster should come from raster::getData")
    plot(raster, xlim=c(min(geo_lonlat[,1]),max(geo_lonlat[,1])),ylim=c(min(geo_lonlat[,2]),max(geo_lonlat[,2])), main=title, xlab=xlab, ylab=ylab, ...)
  }
  
  points(geo_lonlat[,1],geo_lonlat[,2], col = rgb(cols[,1],cols[,2],cols[,3],col.alpha),pch=pch)
}
