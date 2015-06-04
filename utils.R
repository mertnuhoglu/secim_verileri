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

null_fun = function(fun) {
	function(...) {
		if (is.blank(...)) return()
		fun(...)
	}
}

compareo = function(x, y) compare(x, y, ignoreOrder=T)

# filter blank values out
compact = partial(Filter, Negate(is.null))

unlistr = partial(unlist, recursive = F)
pastec = partial(paste, collapse=", ")

# function parameters modified st. data arg is always the first arg (for magrittr)
mapn = function(x, fun) Map(fun, x, names(x))
mapm = function(x, fun, ...) Map(fun, x, ...)
filterm = function(x, fun) Filter(fun, x)
triml = function(x, ch) ltrim_char(ch, x)
trimr = function(x, ch) rtrim_char(ch, x)
duplicatedv = function(x) x %>% duplicated %>% extractm(x)
extractm = function(x, df) extract(df, x)
partialm = function(fun, x) partial(x, fun)
grepm = function(x, pattern, ...) {
	grep(pattern, x, ...)
}
grepv = partial(grepm, value = T)
greplm = function(x, pattern, ...) {
	grepl(pattern, x, ...)
}
vgrep = function(x, patterns, ...) {
	x %>% 
		grepm( patterns %>% paste(collapse="|"), ...) %>%
		unique
}
vgrepv = partial(vgrep, value = T)
subm = function(x, pattern, replacement, ...) {
	sub(pattern, replacement, x, ...)
}
gsubm = function(x, pattern, replacement, ...) {
	gsub(pattern, replacement, x, ...)
}

pre0 = function(x, y) paste0(y, x)
"%+%" = function(...) paste0(...,sep="")

sample_dataframe = function(dt, n = 100) dt[ sample(nrow(dt), size = n), ]
sample_datatable = function(dt, n = 100) dt[ sample(nrow(dt), size = n) ]
sample_with_replace = function(v, n = 100) sample(v, size = n, replace = T)

count_isna = function(x) { sum(is.na(x)) }
count_unique = function(x) { length(unique(x)) }
cu = count_unique
ci = count_isna

is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger
                                     # warnings when used on functions
	 if (is.list(x)) x = unlist(x)
    return(
        is.null(x) ||                # Actually this line is unnecessary since
        length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

# Compare vectors while taking NA as value
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
compareNA <- function(v1,v2) {
    # This function returns TRUE wherever elements are the same, including NA's,
    # and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

is.true  <- function(v) {
	! is.na(v) & v == T
}

# sanitize empty vectors of character() after parsing data
san = function(char) {
	if(length(char)==0) '' else char
}

basename_noext = function(filenames) basename( str_replace(filenames, "\\.\\w+$", "" ) )
ltrim_char = function(ch, x) sub( paste0("^\\", ch, "*"),"",x)
rtrim_char = function(ch, x) sub( paste0("\\",ch,"*$"),"",x)
trim_char = function(x, ch) x %>% triml(ch) %>% trimr(ch)
trim.beforecolon = function(x) sub("^.*:\\s+","",x)
trim = function(x) str_trim(x, side="both")
trimQuotes <- function (x) gsub("^'(.*)'$", "\\1", x)

months.before = function(m) {
	d = m*30
	Sys.Date() - days(d)
}

re = function(regex, x) {
	m = regexpr(regex,x,perl=T)
	regmatches(url,m)
}

config_filenames_real = function() { fread('config/filenames.csv', header=T, stringsAsFactors=F) }
config_filenames = config_filenames_real

.get_directory = function(name_in, test = F, data_root = '') {
	test_in = test
	filename= config_filenames()[name==name_in & test==test_in]$filename
	paste0(data_root, filename)
}                
get_directory_real = partial(.get_directory, data_root = '')
get_directory_test = partial(.get_directory, data_root = 'data/test/')
get_directory = get_directory_real

.get_filename = function(name_in, from = '', ext = '', test = F, file_ext = 'csv', data_root = '') {
	get_filename_from = function(filename, from, ext, file_ext = 'csv'){
		file_ext = ifelse( is.blank(file_ext), '', paste0('.', file_ext) )
		filename = paste0( filename, from, file_ext )
		sub( paste0('\\.', file_ext), paste0( ext, '\\.', file_ext ), filename)
	}
	test_in = test
	filename= config_filenames()[name==name_in & test==test_in]$filename
	filename = paste0(data_root, filename)
	get_filename_from(filename, from, ext, file_ext = file_ext)
}
get_filename_real = partial(.get_filename, data_root = '')
get_filename_test = partial(.get_filename, data_root = "data/test/")
get_filename = get_filename_real

.read.dt = function(file.name, cols = NA, ...) {
	if (is.blank(cols)) {
		cols = NA
	}
	dt = data.table( read.csv(file.name, header=T, stringsAsFactors=F, colClasses=cols, ...) )
}
.read.fread = function(file.name, cols = NULL, ...) {
	dt = fread(file.name, colClasses = cols, ...)
}
.readLines = function(filename, ...) {
	readLines(filename)
}

.read.list.load = function(filename, ...)
	list.load(filename, ...)
.read.json = function(file.name, var_name, read_fun = .read.list.load, ...){
	read_fun(file.name, ...)
}
.read.csv = function(file.name, var_name, read_fun = .read.fread, ...){
	colclasses = config_colclasses()
	cols = NULL
	if (var_name %in% names(colclasses)) {
		cols = colclasses[[var_name]]
	}
	read_fun(file.name, cols, ...)
}
read.file = function(var_name, from = '' , ext = '', test = F, fun_get_filename = get_filename, .read_file_fun = .read.csv, ...) {
	file.name = fun_get_filename(var_name, from, ext, test)
	.read_file_fun(file.name, var_name, ...)
}
read_file_real = partial(read.file, fun_get_filename = get_filename_real)
read_file_with_path = function(file.name, var_name, from = '' , ext = '', test = F, .read_file_fun = .read.csv, ...) {
	.read_file_fun(file.name, var_name, ...)
}

.write.list.save = function(alist, filename, ...) {
	list.save(alist, filename)
}
.write.json = function( alist, file.name, use_table = F, write_fun = .write.list.save, ...) {
	write_fun(alist, file.name, ...)
}
.write.csv = function( df, file.name, use_table = F, ...) {
	dir.create( dirname(file.name), recursive = T )
	if (use_table) {
		write.table(df, file.name, sep=",", ...)
	} else {
		write.csv(df, file.name, append=F, row.names=F, ...)
	} 
}           

write.file = function( df, var_name, from = '', test = F, use_table = F, fun_get_filename = get_filename, .write_file_fun = .write.csv, ...) {
	file.name = fun_get_filename(var_name, from, test = test)
	.write_file_fun(df, file.name, use_table, ...)
}
write_file_real = partial(write.file, fun_get_filename = get_filename_real)
write_file_with_path = function( df, file.name, from = '', test = F, use_table = F, .write_file_fun = .write.csv, ...) {
	.write_file_fun(df, file.name, use_table, ...)
}

.read_data = function(data) {
	fun_name = paste0('read_', data, '()')
	fun = as.expression(parse(text=fun_name))
	df = eval(fun)
}

.write_data = function(data, df) {
	fun_name = paste0('write_', data, '(df)')
	fun = as.expression(parse(text=fun_name))
	eval(fun)
}

read_csv_fun = function(file)
	function(...)
		read.file(file)
write_csv_fun = function(file)
	function(df, ...)
		write.file(df, file, ...)
path_array_fun = function(file) 
	function(arg)
		sprintf(get_filename(file), arg)
read_array_fun = function(file)
	function(arg, ...)
		read_file_with_path(path_array_fun(file)(arg), file, ...) 
write_array_fun = function(file)
	function(df, arg, ...)
		write_file_with_path(df, path_array_fun(file)(arg), ...) 
read_json_fun = function(file)
	function(...)
		read.file(file, fun_get_filename = partial(get_filename, file_ext = 'json'), .read_file_fun = .read.json, ...)
write_json_fun = function(file)
	function(alist, ...)
		write.file(alist, file, fun_get_filename = partial(get_filename, file_ext = 'json'), .write_file_fun = .write.json, ...)

profile.fun = function(from = '', trial = '', write.csv = F, fun, ...) {
	extension = paste0( from, trial )
	fun_name = as.character( substitute(fun) )
	fun_name = gsub("\\.",'_',fun_name,perl=T)
	file_profiler_base = paste0( 'profiler/profiler_', fun_name )
	file_profiler = paste0( file_profiler_base, extension )
	csv_profiler = paste0(file_profiler, '.csv')
	Rprof(NULL)

	Rprof(file_profiler)

	result = fun( ... )

	Rprof(NULL)
	profile = summaryRprof(file_profiler)
	total = profile$by.total
	if ( write.csv ) {
		write.table(total, csv_profiler, sep=',', quote=F)
	}
	return(total)
}

dir_cache_temp = function(path = 'temp/') return(path)

uncache = function() {
	files = list.files(path = dir_cache_temp(), full.names = T) 
	print( sprintf("uncaching %s files", length(files)) )
	r = llply(files, readRDS)
	unlink(files)
	counter_cache_index <<- 0
	return(r)
}

cache_fun = function(fun) 
	function(...) 
		fun(...) %>% cache

cache = function(data)
	saveRDS(data, cache_index())

cache_index = function(path = dir_cache_temp()) {
	dir.create(path, showWarnings = F)
	if (! exists("counter_cache_index") ) {
		counter_cache_index <<- 0
	}
	counter_cache_index <<- counter_cache_index + 1
	counter_cache_index %>% paste0('.rds') %>% pre0(path)
}

llplyp = partial(llply, .progress = "text")

llplyc = function(.data, .fun, ...) {
	llplyp(.data, cache_fun(.fun), ...)
	result = uncache()
}

restore_cache = function() {
	files = list.files(path = dir_cache_temp(), full.names = T) 
	print( sprintf("uncaching %s files", length(files)) )
	r = llply(files, readRDS)
	return(r)
}

llplyc_nouncache = function(.data, .fun, ...) {
	llplyp(.data, cache_fun(.fun), ...)
}                 

setNamesNull = function(object, nm) {
	if ( is.blank(object) ) {
		object
	} else {
		setNames(object, nm) 
	}
}

combine.vectors.by.non.na = function(a, b) {
	a[is.na(a)]  <- b[is.na(a)]
	a
}

log_errors_llplyc = function(.fun) {
	function(filename) {
		return( 
			tryCatch(
				.fun(filename), 
				error = function(e) {
					if (! exists("counter_cache_index") ) {
						counter_cache_index <<- 0
					}
					write(x=counter_cache_index+1,file='error_logs.log',sep="\n", append=T)
				}
			)
		)
	}
}

# compare files in two directories and return difference
setdiff_files_in_dirs = function(dir1, dir2) {
	f1 = list.files(dir1) %>% basename_noext
	f2 = list.files(dir2) %>% basename_noext
	r = setdiff(f1, f2) %>%
		paste0(".txt")
}

basename_ext = function(filename, ext) 
	filename %>% basename_noext %>% paste0('.', ext)

make_trycatch = function(fun)
	function(x)
		tryCatch(
			fun(x),
			error = function(cond) x
		)

"%+%" = function(...) paste0(...)

lapply_stop = function(x, f, ...) {
	force(f)
	out = vector("list", length(x))
	for (i in seq_along(x)) {
		out[[i]] = f(x[[i]], ...)
		if ( ! is.blank(out[[i]]) ) return( out %>% unlist(recursive=F) )
	}
	out %>% unlist(recursive=F)
}

sprintm = function(x, fmt, ...) sprintf(fmt, x, ...)

# test
generate_data = function(base_name, end) {
	1:end %>%
		sprintm("%03s") %>%
		pre0(base_name)
}

safe_extract = function(x, i) {
	if (is.blank(x))
		return(x)
	else
		return(x[i])
}

matches_first = function(x, patterns, ...) 
	lapply(patterns, grepl, x, ...) %>%
		lapply(any) %>%
		unlist %>%
		which %>>%
		safe_extract(1)

download_file = function( url, filename ) {
	dir.create(dirname(filename), recursive=T)
	if ( check_file_exists( filename ) ) {  #@ < zip/formname.zip > txt/formname.txt
		print( paste0( 'skipping ', filename, ' because it exists already' ) )
	} else {
		print( paste0( 'downloading ', filename ) )
		download.file( url, destfile = filename, method="wget" )
	}
}

check_file_exists = function(filename) {
	if (file.exists(filename)) return(TRUE)
}


