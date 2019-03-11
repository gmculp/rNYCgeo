#' The \code{remove_empty_SPDF_holes} function removes empty holes from a projected SpatialPolygonsDataFrame
#' 
#' @title Remove empty holes from a projected SpatialPolygonsDataFrame
#' @name remove_empty_SPDF_holes
#' @import data.table
#' @import sp
#' @import rgdal
#' @import rgeos
#' @export remove_empty_SPDF_holes
#' @param spdf.in SpatialPolygonsDataFrame whose empty holes are to be removed. Required.
#' @return A SpatialPolygonsDataFrame whose empty holes have been removed.
#' @usage remove_empty_SPDF_holes(spdf.in)
#' @examples #create a SpatialPolygonsDataFrame object
#' polys.1 <- Polygons(list(Polygon(cbind(x = c(0, 0, 100, 100, 0), y = c(0, 100, 100, 0, 0)), hole=FALSE),
#'  Polygon(cbind(x = c(20, 20, 40, 40, 20), y = c(60, 80, 80, 60, 60)), hole=TRUE),
#'  Polygon(cbind(x = c(20, 20, 40, 40, 20), y = c(20, 40, 40, 20, 20)), hole=TRUE)
#'  ), ID=1)
#' 
#' polys.2 <- Polygons(list(Polygon(cbind(x = c(20, 20, 40, 40, 20), y = c(20, 40, 40, 20, 20)), hole=FALSE)
#'  ), ID=2)
#' 
#' SPDF <- SpatialPolygonsDataFrame(SpatialPolygons(list(polys.1,polys.2), 
#'  proj4string=CRS("+init=epsg:2263")),
#'  data.frame(value=paste0("poly_",1:2), row.names=1:2))
#' 
#' #plot 1st polygon in original SpatialPolygonsDataFrame
#' plot(SPDF[1,],col="blue")
#' 
#' #plot 2nd polygon in original SpatialPolygonsDataFrame
#' plot(SPDF[2,],col="green",add=TRUE)
#' 
#' #remove empty holes
#' SPDF.2 <- remove_empty_SPDF_holes(SPDF)
#' 
#' #plot 1st polygon in new SpatialPolygonsDataFrame
#' plot(SPDF.2[1,],col="red") 
#' 
#' #plot 2nd polygon in new SpatialPolygonsDataFrame
#' plot(SPDF.2[2,],col="gold",add=TRUE)

remove_empty_SPDF_holes <- function(spdf.in) {

	if(is.projected(spdf.in)){

		###code adapted from: http://r-sig-geo.2731867.n2.nabble.com/Remove-holes-from-a-SpatialPolygon-td7585464.html###

		shp.dp <- slot(spdf.in, "polygons") 
		
		###convert holes to functioning polygons and see if they contain other functioning polygons###
		holes <- lapply(shp.dp, 
			function(x) 
			{
				zz <- lapply(x@Polygons, 
					function(y){
						if(y@hole==TRUE){
							temp.shp <- SpatialPolygons(list(Polygons(list(Polygon(y@coords[nrow(y@coords):1,], hole=FALSE)), ID=1)), proj4string=CRS(proj4string(spdf.in)))
							shp.surf_pt <- gPointOnSurface(temp.shp)
							sp_cent_jn <- over(shp.surf_pt, spdf.in)
							if(is.na(sp_cent_jn) | round(as.numeric(as.character(y@area)),0)==0){
								TRUE
							} else {
								FALSE
							}
						} else{
							y@hole
						}
					}
				)
				return(unlist(zz))
			})
		
		
		res <- lapply(1:length(shp.dp), function(i) slot(shp.dp[[i]], "Polygons")[!holes[[i]]]) 
		
		#IDs <- row.names(spdf.in)
		IDs <- sapply(slot(spdf.in, "polygons"), function(x) slot(x, "ID"))
		
		###add comment to indicate presence of holes###
		polys <- lapply(1:length(res), function(i){ 
			xx <- Polygons(res[[i]], ID=IDs[i])
			comment(xx) <- createPolygonsComment(xx)
			return(xx)
		})
			
		spPolygons <- SpatialPolygons(polys, proj4string=CRS(proj4string(spdf.in)))
		
		spdf.out <- SpatialPolygonsDataFrame(spPolygons, match.ID=T, data=spdf.in@data)
		
		return(spdf.out)

	} else{
		stop("Error: SpatialPolygonsDataFrame must be in a Projected Coordinate System!")
	}	
}