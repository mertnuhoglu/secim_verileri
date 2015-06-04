# Toparlanmış Seçim Verileri

[YSK](http://www.ysk.gov.tr/ysk/faces/Haziran122011) ve [TÜİK'in](http://tuikapp.tuik.gov.tr/secimdagitimapp/secim.zul) web sitelerinde tüm seçim verilerini tek bir dosyada bulmak mümkün değil. Her bir ilçe için tek tek verileri indirmek gerekiyor. 

Bu proje, tüm seçim verilerini tek bir dosya içinde toparlayıp, veri analizi için hazır hale getirir.

Veri kaynağı olarak [memurlar.net](http://www.memurlar.net/secim/) sitesindeki verileri kullandım. YSK ve TÜİK'in web sitelerinde verilerin kolay bir şekilde çekilmesi de mümkün değil.

# Çıktı veri kümesi

Nihai çıktıları şuradan indirebilirsiniz:

https://github.com/mertnuhoglu/secim_verileri/tree/master/data/memurlarnet/genel/collected

Burada iki dosya bulunuyor:

	genel_secim_oylar.csv
	genel_secim_sandiklar.csv

`genel_secim_sandiklar.csv` tüm ilçelerdeki sandık verilerini içeriyor. `genel_secim_oylar.csv` tüm ilçeler için her bir partiye çıkan oyları içeriyor. 

`il` kolonunda isim yerine ilin plaka kodu bulunuyor. 

Dosyaları bilgisayarınıza indirmek için linkler:

https://github.com/mertnuhoglu/secim_verileri/blob/master/data/memurlarnet/genel/collected/genel_secim_oylar.csv?raw=true

https://github.com/mertnuhoglu/secim_verileri/raw/master/data/memurlarnet/genel/collected/genel_secim_sandiklar.csv?raw=true

Dosyalar csv metin formatındadır. Excel ve Google Docs ile açabilirsiniz. 

