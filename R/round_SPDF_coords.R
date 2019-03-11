#' The \code{round_SPDF_coords} function rounds the coordinates of a projected SpatialPolygonsDataFrame using parallel processing and then checks that geometry is valid
#' 
#' @title Round coordinates of a projected SpatialPolygonsDataFrame
#' @name round_SPDF_coords
#' @import data.table
#' @import sp
#' @import rgdal
#' @import rgeos
#' @import cleangeo
#' @import parallel
#' @export round_SPDF_coords
#' @param spdf.in SpatialPolygonsDataFrame whose coordinates are to be rounded. Required.
#' @param rnd_num number for which coordinates are to be rounded. Required.
#' @param clus_num number of cluster for parallel processing on Unix and Mac OS. Optional.
#' @return A SpatialPolygonsDataFrame whose coordinates have been rounded
#' @usage round_SPDF_coords(spdf.in, rnd_num, clus_num=2)
#' @examples #create a SpatialPolygonsDataFrame object
#' nn <- 10.2
#' rr <- seq(0,nn*2,by=nn)
#' xx <- rep(rr,length(rr)^2)
#' yy <- rep(rr,each=length(rr)^2)
#' Pls <- lapply(1:length(xx), function(i) Polygons(list(Polygon(cbind(x = c(0, 0, 10, 10, 0) + xx[i], y = c(0, 10, 10, 0, 0) + yy[i]))),ID=i))
#' SPDF <- SpatialPolygonsDataFrame(SpatialPolygons(Pls, proj4string=CRS("+init=epsg:2263")),data.frame(value=paste0("poly_",1:length(xx)), row.names=1:length(xx)))
#' plot(SPDF, border="red")
#' #round coordinates by 2
#' SPDF.2 <- round_SPDF_coords(SPDF,2,2)
#' plot(SPDF.2, border="blue", add=TRUE) 

round_SPDF_coords <- function(spdf.in, rnd_num, clus_num=2){

	if(is.projected(spdf.in)){
		
		###disable scientific notation###
		options(scipen = 999)

		shp.dp <- slot(spdf.in, "polygons")
		
		res <- lapply(1:length(shp.dp), function(i) {
		
			zz <- lapply(slot(shp.dp[[i]], "Polygons"), function(y){
				
				y@coords <- round(y@coords/rnd_num)*rnd_num
				return(y)
				
			})
			return(zz)
		})	
		
		IDs <- row.names(spdf.in) 
		
		###add comment to indicate presence of holes###
		polys <- lapply(1:length(res), function(i){ 
		   xx <- Polygons(res[[i]], ID=IDs[i])
		   comment(xx) <- createPolygonsComment(xx)
		   return(xx)
		   })
		
		spPolygons <- SpatialPolygons(polys, proj4string=CRS(proj4string(spdf.in)))
		
		spdf.out <- SpatialPolygonsDataFrame(spPolygons, match.ID=T, data=spdf.in@data)
		
		###final cleaning of corrupt geometries###
		if(.Platform$OS.type != "windows"){
		
			###optimal number of records to prevent bloat###
			n.r <- nrow(spdf.out)
		
			###assign cores to records###
			spdf.out$pc <- rep(1:clus_num,each=(ceiling(n.r/clus_num)))[1:n.r]
		
			out.list <- mclapply(1:clus_num, function(zz) { cleangeo::clgeo_Clean(spdf.out[spdf.out$pc==zz,],verbose = FALSE)}, mc.cores=clus_num)
			
			spdf.out <- do.call("rbind", out.list)
			
			spdf.out$pc <- NULL
		} else{
			spdf.out <- cleangeo::clgeo_Clean(spdf.out,verbose = FALSE)
		}
		
		invisible(gc())
		
		return(spdf.out)
	} else{
		stop("Error: SpatialPolygonsDataFrame must be in a Projected Coordinate System!")
	}

}