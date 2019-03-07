###DISABLE SCIENTIFIC NOTATION###
options(scipen = 999)

#############################
###load required libraries###
#############################
library(data.table)
library(stringr)

build_census_lookups <- function(mainDir){

	### mainDir <- "/home/health.dohmh.nycnet/gculp/build_packages/rNYCgeo/data"
	
	
	
	###############################################
	###generate 2000 block to zcta look-up table###
	###############################################

	###load geographic look-up table for NYS from 2000 decennial census summary file 1###
	
	###download file from USCB website as it is too large to host on GitHub###
	tmpdir <- tempdir()
	file_path <- "https://www2.census.gov/census_2000/datasets/Summary_File_1/New_York/nygeo_uf1.zip"
	d.file <- basename(file_path)
	download.file(file_path, d.file)
	unzip(d.file, exdir = tmpdir)		
	input.dt <- data.table(read.csv(file.path(tmpdir,"nygeo.uf1"),header=FALSE, stringsAsFactors=FALSE))
	

	###load table schema###
	s_path <- system.file("raw", "census_geo_tbl_schema_2000.csv", package = "rNYCgeo")
	s.dt <- data.table(read.csv(s_path, stringsAsFactors=FALSE))
	s.dt[, Name := as.character(Name)]
	s.dt[, Size := as.numeric(gsub("\\s", "", as.character(Size)))]
	s.dt[, char.end := cumsum(Size)]
	s.dt[, char.start := char.end - (Size - 1)]


	#############################################################
	###divide text block into fixed-width columns using schema###
	#############################################################

	invisible(sapply(1:nrow(s.dt), function (i) {

		input.dt[, xxx := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", substr(as.character(input.dt$V1), as.numeric(s.dt[i]$char.start), as.numeric(s.dt[i]$char.end)), perl=TRUE)]

		setnames(input.dt, 'xxx', s.dt[i]$Name)
		
	}))

	###remove block text field###
	input.dt[, V1 := NULL]

	###restrict to NYC counties###
	input.dt <- input.dt[COUNTY %in% c("005","061","047","081","085")]

	###create appropriately formatted geo_id columns for tract and block###
	input.dt[, USCB_TRACT := ifelse(TRACT=="","",paste0(as.character(STATE),as.character(COUNTY),as.character(TRACT)))]
	input.dt[, USCB_BLOCK := ifelse(BLOCK=="","",paste0(as.character(STATE),as.character(COUNTY),as.character(TRACT),as.character(BLOCK)))]

	df.ZCTA_2000 <- input.dt[ZCTA5 != "" & USCB_BLOCK != "",c("USCB_TRACT","USCB_BLOCK","ZCTA5","POP100","HU100","AREALAND","AREAWATER"),with=FALSE]
	
	
	
	
	###############################################
	###generate 2010 block to zcta look-up table###
	###############################################
	
	###load geographic look-up table for NYS from 2010 decennial census summary file 1###
	
	###download file from USCB website as it is too large to host on GitHub###
	tmpdir <- tempdir()
	file_path <- "https://www2.census.gov/census_2010/04-Summary_File_1/New_York/ny2010.sf1.zip"
	d.file <- basename(file_path)
	download.file(file_path, d.file)
	unzip(d.file, exdir = tmpdir)		
	input.dt <- data.table(read.csv(file.path(tmpdir,"nygeo2010.sf1"),header=FALSE, stringsAsFactors=FALSE))
	

	###load table schema###
	s_path <- system.file("raw", "census_geo_tbl_schema_2010.csv", package = "rNYCgeo")
	s.dt <- data.table(read.csv(s_path, stringsAsFactors=FALSE))
	s.dt[, id := 1:nrow(s.dt)]
	s.dt[, Name := as.character(Name)]
	s.dt[, Size := as.numeric(gsub("\\s", "", as.character(Size)))]
	s.dt <- s.dt[!(is.na(Size)) & Name !='Name']
	s.dt <- s.dt[order(id)]
	s.dt[, char.end := cumsum(Size)]
	s.dt[, char.start := char.end - (Size - 1)]


	#############################################################
	###divide text block into fixed-width columns using schema###
	#############################################################

	invisible(sapply(1:nrow(s.dt), function (i) {

		input.dt[, xxx := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", substr(as.character(input.dt$V1), as.numeric(s.dt[i]$char.start), as.numeric(s.dt[i]$char.end)), perl=TRUE)]

		setnames(input.dt, 'xxx', s.dt[i]$Name)
		
	}))

	###remove block text field###
	input.dt[, V1 := NULL]

	###restrict to NYC counties###
	input.dt <- input.dt[COUNTY %in% c("005","061","047","081","085")]

	###create appropriately formatted geo_id columns for tract and block###
	input.dt[, USCB_TRACT := ifelse(TRACT=="","",paste0(as.character(STATE),as.character(COUNTY),as.character(TRACT)))]
	input.dt[, USCB_BLOCK := ifelse(BLOCK=="","",paste0(as.character(STATE),as.character(COUNTY),as.character(TRACT),as.character(BLOCK)))]

	df.ZCTA_2010 <- input.dt[ZCTA5 != "" & USCB_BLOCK != "",c("USCB_TRACT","USCB_BLOCK","ZCTA5","POP100","HU100","AREALAND","AREAWATR"),with=FALSE]
	
	###rename so columns match 2000 table###
	
	setnames(df.ZCTA_2010,c("AREAWATR"),c("AREAWATER"))
	
	################################################
	###BUILD DCP TRACT TO NTA/PUMA LOOK-UP TABLES###
	################################################
	
	my_URL <- "https://api.census.gov/data/2016/acs/acs1?get=NAME&for=public%20use%20microdata%20area:*&in=state:36"
	test.dt <- data.table::data.table(jsonlite::fromJSON(URLencode(my_URL)))
	data.table::setnames(test.dt,names(test.dt),gsub(" ","\\.",as.character(test.dt[1,])))
	NYC.dt <- test.dt[grepl("^NYC-",NAME),]
	NYC.dt[,PUMA_NAME := gsub("^.*--(.*) PUMA.*$","\\1",NAME)]
	#NYC.dt[,cd_list := gsub("^.*District (.*)--.*$","\\1",NAME)]
	#NYC.dt[,boro_name := gsub("^NYC-(.*) Community District.*$","\\1",NAME)]
	NYC.dt[,c("NAME","state") := NULL]
	setnames(NYC.dt,c("public.use.microdata.area"),c("PUMA"))
	
	
	s_path <- system.file("raw", "nyc2000census_projectionarea_equiv.csv", package = "rNYCgeo")
	dt.PUMA_NTA_2000 <- data.table(read.csv(s_path, stringsAsFactors=FALSE))
	dt.PUMA_NTA_2000[, USCB_TRACT_2000 := paste0("36",str_pad(as.character(County_FIPS), width=3, pad="0"),str_pad(as.character(CT_2000), width=6, pad="0"))]
	dt.PUMA_NTA_2000[, PUMA_2000 := stringr::str_pad(as.character(PUMA_2000), width=5, pad="0")]
	dt.PUMA_NTA_2000[, c("County_FIPS","Borough","CT_2000") := NULL]
	dt.PUMA_NTA_2000 <- merge(dt.PUMA_NTA_2000, NYC.dt, by.x="PUMA_2000", by.y="PUMA")
	
	setnames(dt.PUMA_NTA_2000,names(dt.PUMA_NTA_2000),c("PUMA_2000","NTA_2000","NTA_NAME_2000","USCB_TRACT_2000","PUMA_NAME_2000"))
	
	###save as RDA file###
	xxx <- "CT_to_PUMA_NTA_2000"
	rda_path <- paste(xxx,"rda",sep=".")
	assign(xxx, dt.PUMA_NTA_2000)
	save(list=xxx, file = paste(mainDir, rda_path, sep="/"))
	
	
	########################################
	########################################
	########################################
	
	s_path <- system.file("raw", "nyc2010census_projectionarea_equiv.csv", package = "rNYCgeo")
	dt.PUMA_NTA_2010 <- data.table(read.csv(s_path, stringsAsFactors=FALSE))
	dt.PUMA_NTA_2010[, USCB_TRACT_2010 := paste0("36",str_pad(as.character(County_FIPS), width=3, pad="0"),str_pad(as.character(CT_2010), width=6, pad="0"))]
	dt.PUMA_NTA_2010[, PUMA_2010 := str_pad(as.character(PUMA_2010), width=5, pad="0")]
	dt.PUMA_NTA_2010[, c("County_FIPS","Borough","CT_2010","boro_code") := NULL]
	dt.PUMA_NTA_2010 <- merge(dt.PUMA_NTA_2010, NYC.dt, by.x="PUMA_2010", by.y="PUMA")
	
	setnames(dt.PUMA_NTA_2010,names(dt.PUMA_NTA_2010),c("PUMA_2010","NTA_2010","NTA_NAME_2010","USCB_TRACT_2010","PUMA_NAME_2010"))
	
	
	###save as RDA file###
	xxx <- "CT_to_PUMA_NTA_2010"
	rda_path <- paste(xxx,"rda",sep=".")
	assign(xxx, dt.PUMA_NTA_2010)
	save(list=xxx, file = paste(mainDir, rda_path, sep="/"))
	
	
	#############################################
	###Data Task Force ZCTA aggregates look-up###
	#############################################
	 
	##################################################
	###load Data Task Force aggregate zip code file###
	##################################################

	s_path <- system.file("raw", "DTF_ZCTA_combos.csv", package = "rNYCgeo")
	zc.agg.dt <- data.table(read.csv(s_path, header = FALSE,stringsAsFactors=FALSE))
	zc.agg.dt[, names(zc.agg.dt) := lapply(.SD, as.character), .SDcols = names(zc.agg.dt)]

	zc.agg.dt <- melt(zc.agg.dt, id.vars = c("V3"), measure.vars = paste0("V",4:16))
	zc.agg.dt <- zc.agg.dt[!(is.na(as.character(value)))]
	setnames(zc.agg.dt, c("V3","value"), c("COMBOZCTA","MODZCTA"))
	zc.agg.dt[, variable := NULL]


	########################################
	###load Data Task Force look-up table###
	########################################
	
	s_path <- system.file("raw", "DTF_ZC_to_ZCTA.csv", package = "rNYCgeo")
	zc2zcta <- data.table(read.csv(s_path,stringsAsFactors=FALSE))
	###make sure all fields are character type###
	zc2zcta[,ZCTA := as.character(ZCTA)]
	zc2zcta[,MODZCTA := as.character(MODZCTA)]
	zc2zcta[,UHFCODE := as.character(UHFCODE)]

	###remove blank fields###
	zc2zcta[, colnames(zc2zcta)[grepl("^X",colnames(zc2zcta))] := NULL]
	zc2zcta <- unique(zc2zcta[,c("ZIPCODE","ZCTA","MODZCTA","UHFCODE"), with=FALSE])

	###create unique dt for join to census blocks
	zc2zcta.u1 <- unique(zc2zcta[,c("ZCTA","MODZCTA","UHFCODE"), with=FALSE])
	zc2zcta.u2 <- unique(zc2zcta[,c("ZIPCODE","MODZCTA","UHFCODE"), with=FALSE])
	setnames(zc2zcta.u2, "ZIPCODE", "ZCTA")
	zc2zcta.u <- unique(rbindlist(list(zc2zcta.u1,zc2zcta.u2)))

	###get legit ZCTA from SF1 GEO files###
	zz <- unique(df.ZCTA_2010$ZCTA5)
	zz2 <- unique(df.ZCTA_2000$ZCTA5)
	zz <- c(zz,zz2[!(grepl("HH$",zz2)) & !(zz2 %in% zz)])
	
	zc2zcta.out <- merge(zc2zcta[ZCTA %in% zz,c("ZIPCODE","ZCTA"),with=FALSE],zc2zcta.u,by="ZCTA")

	aa <- unique(zc2zcta.out$ZCTA)
	bb <- unique(zc2zcta$ZCTA)
	cc <- bb[!(bb %in% aa)]
	zc2zcta.out2 <- zc2zcta[ZCTA %in% cc]
	zc2zcta.out2[,ZCTA := "99999"]
	zc2zcta.out2[,MODZCTA := "99999"]
	zc2zcta.out2[,UHFCODE := "9999"]
	setcolorder(zc2zcta.out, c("ZIPCODE","ZCTA","MODZCTA","UHFCODE"))
	zc2zcta.out <- rbindlist(list(zc2zcta.out,zc2zcta.out2))

	zc2zcta.out <- merge(zc2zcta.out, zc.agg.dt, by="MODZCTA", all.x=TRUE)
	zc2zcta.out[,COMBOZCTA := ifelse(is.na(COMBOZCTA),"999999",COMBOZCTA)]
	setcolorder(zc2zcta.out, c("ZIPCODE","ZCTA","MODZCTA","COMBOZCTA","UHFCODE"))
	setorder(zc2zcta.out,ZIPCODE)
	
	###deal with missing ZCTA###
	missing_ZCTA <- zz[!(zz %in% unique(zc2zcta.out$ZCTA))]
	missing_ZCTA.dt <- zc2zcta.out[ZIPCODE %in% missing_ZCTA]
	missing_ZCTA.dt[,ZCTA := ZIPCODE] 
	missing_ZCTA.dt[,ZIPCODE := NULL] 
	
	zc2zcta.out2 <- rbindlist(list(unique(zc2zcta.out[,c("ZCTA","MODZCTA","COMBOZCTA","UHFCODE")]),missing_ZCTA.dt))
	setorder(zc2zcta.out2,ZCTA)
	
	#################################
	###merge DTF data to USCB data###
	#################################

	##########
	###2000###
	##########
	
	m_2000 <- merge(df.ZCTA_2000,zc2zcta.out2,by.x="ZCTA5",by.y="ZCTA",all.x=TRUE)
	m_2000[,MODZCTA := ifelse(is.na(MODZCTA),"99999",MODZCTA)]
	m_2000[,COMBOZCTA := ifelse(is.na(COMBOZCTA),"999999",COMBOZCTA)]
	m_2000[,UHFCODE := ifelse(is.na(UHFCODE),"9999",UHFCODE)]
	
	setnames(m_2000,c("ZCTA5"),c("ZCTA"))
	setnames(m_2000,names(m_2000),paste0(names(m_2000),"_2000"))
	
	###save as RDA file###
	xxx <- "CB_to_ZCTA_2000"
	rda_path <- paste(xxx,"rda",sep=".")
	assign(xxx, m_2000)
	save(list=xxx, file = paste(mainDir, rda_path, sep="/"))
	
	##########
	###2010###
	##########

	m_2010 <- merge(df.ZCTA_2010,zc2zcta.out2,by.x="ZCTA5",by.y="ZCTA",all.x=TRUE)
	m_2010[,MODZCTA := ifelse(is.na(MODZCTA),"99999",MODZCTA)]
	m_2010[,COMBOZCTA := ifelse(is.na(COMBOZCTA),"999999",COMBOZCTA)]
	m_2010[,UHFCODE := ifelse(is.na(UHFCODE),"9999",UHFCODE)]
	
	setnames(m_2010,c("ZCTA5"),c("ZCTA"))
	setnames(m_2010,names(m_2010),paste0(names(m_2010),"_2010"))
	
	###save as RDA file###
	xxx <- "CB_to_ZCTA_2010"
	rda_path <- paste(xxx,"rda",sep=".")
	assign(xxx, m_2010)
	save(list=xxx, file = paste(mainDir, rda_path, sep="/"))
	
	

}

