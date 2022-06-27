#' @param .points polyline points to interpolate
#' @param .z optional altimetry of points, to be returned as attribute if provided
#' @param p_01 parameter in [0,1] to get the parametric line interpolated coordinates. (XOR p_m)
#' @param p_m parameter in absolute length (m) to get the parametric line interpolated coordinates. (XOR p_01)
#' @test
#' plot(.coords_points)
#' lines(.coords_points)
#' points(matrix(pline_fun(p_01=0.01),ncol=2),col='red',pch=20)
#' points(matrix(pline_fun(p_01=0.1),ncol=2),col='red',pch=20)
#' points(matrix(pline_fun(p_01=0.5),ncol=2),col='red',pch=20)
#' points(matrix(pline_fun(p_01=0.9),ncol=2),col='red',pch=20)
#' points(matrix(pline_fun(p_01=0.99),ncol=2),col='red',pch=20)
#' points(matrix(pline_fun(p_m=0),ncol=2),col='blue',pch=20)
#' points(matrix(pline_fun(p_m=1000),ncol=2),col='blue',pch=20)
#' points(matrix(pline_fun(p_m=8000),ncol=2),col='blue',pch=20)
pline_fun = function(p_m=NULL, p_01=NULL, .points = NULL, .z = NULL, norm=FALSE) {
  if (is.null(.points)) stop("parameter .points must be set")
  if (!is.null(p_m) & !is.null(p_01)) stop("parameter p_m XOR p_01 must be set")
  if (is.null(p_m) & is.null(p_01)) stop("parameter p_m XOR p_01 must be set")
  if (!is.null(p_01)) {
    if (p_01>1 | p_01<0) stop("parameter must stand in [0,1]")
    if (p_01 == 0) return(.points[1,])
    if (p_01 == 1) return(.points[nrow(.points),])
  }
  if (!is.null(p_m)) {
    if (p_m<0) stop("parameter must stand in [0,+inf[")
    if (p_m == 0) return(.points[1,])
  }
  
  vects = .points[-1,] - .points[-nrow(.points),]
  ts = c(0,cumsum(sqrt(rowSums(vects^2))))
  if (!is.null(p_01)) p_m = p_01*ts[length(ts)]
  t = p_m
  i_sup = which(ts >= t)[1]
  i_inf = i_sup - 1
  t_infsup = (t - ts[i_inf]) / (ts[i_sup] - ts[i_inf])
  
  coord = ( .points[i_inf,] + t_infsup * (.points[i_sup,]-.points[i_inf,]) )
  if (!is.null(.z)) attr(coord,"z") <- .z[i_inf] + t_infsup * (.z[i_sup]-.z[i_inf])
  if (isTRUE(norm)) {n <- t_infsup * (.points[i_sup,]-.points[i_inf,]); attr(coord,"norm") <- rbind(-n[2,],n[1,])}
  return(coord)
}

#' @test
#' plot(points)
#' lines(points)
#' print_pline01(0.1)
print_pline01 = function(p_01,...) trimws(paste0(paste0(pline_fun(p_01=p_01,p_m=NULL,...),collapse=" "),"\n"))

print_pline01_seq = function(p0=0,p1=1,n=50,...) trimws(paste0(collapse="\n",Vectorize(print_pline01)(seq(f=p0,t=p1,l=n),...)))

#' @test
#' plot(points)
#' lines(points)
#' print_pline(100)
print_pline = function(p_m,...) trimws(paste0(paste0(pline_fun(p_m=p_m,p_01=NULL,...),collapse=" "),"\n"))

print_pline_seq = function(p_m0=0,p_m1=100,n=50,...) trimws(paste0(collapse="\n",Vectorize(print_pline)(seq(f=p_m0,t=p_m1,l=n),...)))