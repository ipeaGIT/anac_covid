#
# download data
#
`%nin%` = Negate(`%in%`)
dir.create("data-raw")
dir.create("data-raw/anac")

# combinada
fixed_url <- "https://www.anac.gov.br/assuntos/setor-regulado/empresas/envio-de-informacoes/microdados/"

range_years <- 2017:2020
range_months <- 1:7       # jan-jul
month_basic <- paste0("basica",rep(range_years,each = length(range_months)),"-0",rep(range_months,length(range_years)))
month_comb <- paste0("combinada",rep(range_years,each = length(range_months)),"-0",rep(range_months,length(range_years)))
# full link
url_basic <- paste0(fixed_url,month_basic,".zip")
url_comb <- paste0(fixed_url,month_comb,".zip")
# test link
url_basic

url_comb

for(i in 1:length(url_basic)){ # i means months
  # download basico
  message(month_basic[i])
  download.file(url = url_basic[i],destfile = paste0('data-raw/anac/',month_basic[i],'.zip'))
  unzip(zipfile =  paste0('data-raw/anac/',month_basic[i],'.zip'),exdir = 'data-raw/anac/')
  
  # download combinada
  message(month_comb[i])
  download.file(url = url_comb[i],destfile = paste0('data-raw/anac/',month_comb[i],'.zip'))
  unzip(zipfile =  paste0('data-raw/anac/',month_comb[i],'.zip'),exdir = 'data-raw/anac/')
}
