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

source('utils.R')

dir_genel_memurlarnet = function() { get_directory('genel_memurlarnet') }
dir_yerel_memurlarnet = function() { get_directory('yerel_memurlarnet') }
dir_cb_memurlarnet = function() { get_directory('cb_memurlarnet') }
dir_genel_memurlarnet_raw = function() { dir_genel_memurlarnet() %+% 'raw/' }
dir_genel_memurlarnet_clean = function() { dir_genel_memurlarnet() %+% 'clean/' }


