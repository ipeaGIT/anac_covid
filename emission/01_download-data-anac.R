# combinada
linkc <- "https://www.anac.gov.br/assuntos/setor-regulado/empresas/envio-de-informacoes/microdados/combinada2020-01.zip"

#
# download data
#
`%nin%` = Negate(`%in%`)
dir.create("data-raw")
dir.create("data-raw/anac")

fixed_url <- "https://www.anac.gov.br/assuntos/setor-regulado/empresas/envio-de-informacoes/microdados/"
month <- paste0("basica",rep(2019:2020,each = 5),"-0",rep(1:5,2))
full_url <- paste0(fixed_url,month,".zip")

for(i in c(1,6)){
  message(i)
  download.file(url = full_url[i],destfile = paste0('data-raw/anac/',month[i],'.zip'))
  unzip(zipfile =  paste0('data-raw/anac/',month[i],'.zip'),exdir = 'data-raw/anac/')
}

# 
i = 10
download.file(url = paste0(fixed_url,"basica202006.zip"),destfile = paste0('data-raw/anac/',month[i],'.zip'))
unzip(zipfile =  paste0('data-raw/anac/',month[i],'.zip'),exdir = 'data-raw/anac/')
