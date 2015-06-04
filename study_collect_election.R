source('collect_election_results.R')

explore_memurlarnet_data = function() {
	data = list.load(filename)
	data %>% str
	names(data)
	data$secim %>% names
	data$secim %>% length
	x = list.filter(data$secim, "2011" %in% yil)
	x %>% length
	x[[1]] %>% str
	x[[2]] %>% names
	list.select(x, ilce)
	list.select(x, ilce) %>% unlist


	# read
	filenames = sprintf( dir_genel_memurlarnet_clean() %+% "%02s.json", 1:81)
	dl = llply(filenames, list.load, .progress = "text")
	length(dl)
	iller_l = dl %>%
		lapply(
			function(al) {
				al[['secim']]
			}
		)
	iller_l[[1]][[1]] %>% names
	df = dl[[1]][[1]] %>% 
		list.select(ilce, yil, sandik, secmen, oykullanan, gecerlioy) %>%
		rbindlist
	iller = dl %>%
		lapply(
			. %>% `[[`('secim') %>%
			list.select(ilce, yil, sandik, secmen, oykullanan, gecerlioy) %>%
			rbindlist
		) %>%
		rbindlist
}

explore_parti_oylari = function() {
	d2011 = data$secim %>%
		list.filter("2011" %in% yil)
	d2011[[1]] %>% names
	pl = d2011 %>% 
		list.select(partiler, ilce, yil)
	length(pl)
	# 15
	#> pl[[1]][[1]] %>% str
	#List of 39
	 #$ :List of 2
		#..$ Parti: chr "AK Parti"
		#..$ Oy   : int 130863
	 #$ :List of 2
		#..$ Parti: chr "ANAP"
		#..$ Oy   : int 0

	# tek bir ilçedeki verileri tablolaştırma
	pt = pl[[1]][[1]] %>%
		rbindlist
	pt_ilceler = llply(pl, 
			. %>%
			`[[`('partiler') %>%
			rbindlist
		) 

	# tüm ilçelerin verilerini birleştirme
	pt = llply(pl, 
			. %>%
			`[[`('partiler') %>%
			rbindlist
		) %>%
		rbindlist
	# ilçe verisini kaybettik

	# her bir ilçe tablosuna ilçenin ismini ve seçim yılını ekle
	pt_ilceler[[1]]$ilce = 'Saimbeyli'
	head(pt_ilceler[[1]])

	# daha önce nasıl ekliyorduk?
	# kaçar tane var her bir grupta?
	row_counts = llply(pt_ilceler, nrow)
	pl[[1]][['ilce']]
	#[1] "Seyhan"
	ilceler = d2011 %>% 
		list.select(ilce) %>%
		unlist
	yillar = d2011 %>% 
		list.select(yil) %>%
		unlist
	rep(ilceler, row_counts)
	rep(yillar, row_counts)
	pt$ilce = rep(ilceler, row_counts)
	pt$yil = rep(yillar, row_counts)
}

study_parti_oylari_combine = function() {
	dl = llply(filenames, list.load, .progress = "text")
	dvl = dl %>%
		lapply(
			. %>% `[[`('secim') %>%
			list.select(partiler, ilce, yil)
		) 
	# > dvl %>% length
	# [1] 81
	# > dvl[[1]] %>% length
	# [1] 88
	# > dvl[[1]] %>% names
	# NULL
	# > dvl[[1]][[1]] %>% names
	# [1] "partiler" "ilce"     "yil"

	# 1
	pkv = dvl %>% 
		llply(
			. %>%
			llply(
				. %>%
				`[[`('partiler') %>%
				rbindlist
			) 
		)

	# 2
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
								cbind(kayit=i)
						},
						dv
					)
			},
			dvl
		)
	# > pkv %>% length
	# [1] 81
	# > pkv[[1]] %>% length
	# [1] 88

	# 1
	pv = pkv %>%
		llply(
			. %>%
			rbindlist %>%
			cbind(il="01")
		) 
	
	# pv'ye illeri ekle
	pv[[1]]$il = '01'

	# 2
	pv = pkv %>%
		llply(
			function(x) {
				x %>%
				rbindlist %>%
				cbind(il="01")
			}
		) 

	# 3 - not working
	pv = pkv %>%
		llply(
			function(x) {
				idx = eval.parent(quote(names(X)))[substitute(x)[[3]]]
				print(idx)
				x %>%
					rbindlist %>%
					cbind(il="01")
			}
		) 

	# 4 - best
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


	row_countsl = pkv %>%
		llply( . %>% 
			llply( . %>% nrow )
		)

	ilcelerl = dvl %>% 
		llply( . %>%
			list.select(ilce) %>%
			unlist
		)
	yillarl = dvl %>% 
		llply( . %>%
			list.select(yil) %>%
			unlist
		)
	ilcel = ilcelerl %>%
		llply( . %>%
			rep(ilceler, row_countsl)
		)
	p$yil = rep(yillar, row_counts)
}

study_parti_oylari_kayit_ref = function() {
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


