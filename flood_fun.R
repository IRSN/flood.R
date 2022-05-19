#' @test plot(0:100,flood_gauss(0:100,tm=25,d=90,qmax=42, q0=10))
flood_gauss = function(t, tm=500000, d , qmax ,q0) {
  q0+exp(-(t-tm)^2/2/(d/6)^2)*(qmax-q0)
}

#' @test plot(0:100,flood_tri(0:100,tm=25,d=90,qmax=42, q0=10))
flood_tri = function(t, tm=500000, d , qmax , q0) {
  positive_part = function(x) {x*(x>0)}
  q0 + positive_part( 
    - (qmax-q0) * positive_part(tm-t)/tm +
    (qmax-q0) * ( 1 - positive_part(t-tm)/(d-tm))
  )
}