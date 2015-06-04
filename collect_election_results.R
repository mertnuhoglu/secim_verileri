library("lubridate")
library("stringr")
library("pryr")
library("data.table")
library("pipeR"); library("plyr"); library("dplyr")
library("tidyr")
library("magrittr")
library("rlist")
library("XML")
library("xml2")
library("reshape2")
library("gtools")
library("rjson")
library("compare")

main = function() {
	download_memurlarnet()
	clean_memurlarnet_json_files()
	df_master = parse_memurlarnet_data_master()
	df_partioylari = parse_memurlarnet_data_partioylari()
}

download_memurlarnet = function() {
	for (i in 1:81) {
		url = sprintf( "http://www.memurlar.net/secim/data/genel/tr-%02s.js", i)
		filename = sprintf( dir_genel_memurlarnet_raw() %+% "%02s.json", i)
		download_file( url, filename )
	}
}

clean_memurlarnet_json_files = function() {
	# var secimVerisi = [
	# {
	# >>>
	# {
	# "secim": [
	for (i in 1:81) {
		filename = sprintf( dir_genel_memurlarnet_raw() %+% "%02s.json", i)
		cmd = sprintf("./clean_memurlarnet_json.sh %s", filename)
		system(cmd)
	}
}

parse_memurlarnet_data_master = function() {
	filenames = sprintf( dir_genel_memurlarnet_clean() %+% "%02s.json", 1:81)
	dl = llply(filenames, list.load, .progress = "text")
	iller = seq_along(dl) %>%
		lapply(
			function(i, dl) {
				dl[[i]] %>% `[[`('secim') %>%
				list.select(ilce, yil, sandik, secmen, oykullanan, gecerlioy) %>%
				rbindlist %>%
				cbind(il=i)
			}, dl
		) %>%
		rbindlist
}

parse_memurlarnet_data_partioylari = function() {
	dl = llply(filenames, list.load, .progress = "text")
	dvl = dl %>%
		lapply(
			. %>% `[[`('secim') %>%
			list.select(partiler, ilce, yil)
		) 

	pkv = seq_along(dvl) %>% 
		llply(
			function(i, dvl) {
				dv = dvl[[i]]
				seq_along(dv) %>>%
					llply(
						function(i, dv) {
							dv[[i]] %>%
								`[[`('partiler') %>%
								rbindlist %>%
								cbind( ilce=dv[[i]][['ilce']], yil=dv[[i]][['yil']] )
						},
						dv
					)
			},
			dvl
		)
	pv = seq_along(pkv) %>%
		llply(
			function(i, pkv) {
				pkv[[i]] %>%
				rbindlist %>%
				cbind(il=i)
			},
			pkv
		) 

	p = pv %>%
		rbindlist
}

