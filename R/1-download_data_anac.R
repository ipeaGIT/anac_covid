#
# download data
#
rm(list=ls())
`%nin%` = Negate(`%in%`)
dir.create("data-raw")
dir.create("data-raw/anac")

# combinada
fixed_url <- "https://www.anac.gov.br/assuntos/setor-regulado/empresas/envio-de-informacoes/microdados/"

range_years <- 2017:2020
range_months <- 1:11       # jan-jul
month_basic <- paste0("basica",rep(range_years,each = length(range_months)),"-0",rep(range_months,length(range_years)))
month_comb <- paste0("combinada",rep(range_years,each = length(range_months)),"-0",rep(range_months,length(range_years)))

# fix urls month 10,11,12
month_basic <- base::gsub("010","10",month_basic)
month_basic <- base::gsub("011","11",month_basic)
month_basic <- base::gsub("012","12",month_basic)
month_comb <- base::gsub("010","10",month_comb)
month_comb <- base::gsub("011","11",month_comb)
month_comb <- base::gsub("012","12",month_comb)

# full link
url_basic <- paste0(fixed_url,month_basic,".zip")
url_comb <- paste0(fixed_url,month_comb,".zip")

# test link

# url_basic
# url_comb

# temp directory to download .zip files
my_tmpDir <- tempdir()
dtraw_dir <- '../../data-raw/ANAC'


# download function
# with tryCatch error

download_func <- function(url,destfile,exdir){
  
  expr <- function(url,destfile,exdir){
    download.file(url = url,destfile = destfile)
    unzip(zipfile =  destfile,exdir = exdir)
    return(NULL)
  }
  
  
  tryCatch(expr(url = url,destfile = destfile,exdir = exdir),error = function(e){
    message("An error occurred:\n", e)
  })
}

for(i in 1:length(url_basic)){ # i means months 
  
  # download basico
  message(month_basic[i])
  
  tmp_txtfile <- paste0(dtraw_dir,"/",month_basic[i],'.txt')

    if(file.exists(tmp_txtfile) == FALSE){
    
    tryCatch(download_func(url = url_basic[i],
                           destfile = paste0(my_tmpDir,month_basic[i],'.zip'),
                           exdir = dtraw_dir))
    
  }else{
    message("file already downloaded")
  }
  
  #
  # download combinada
  #
  
  message(month_comb[i])
  
  tmp_txtfile <- paste0(dtraw_dir,"/",month_comb[i],'.txt')
  
  if(file.exists(tmp_txtfile)  == FALSE){
    
    download_func(url = url_comb[i],
                  destfile = paste0(my_tmpDir,month_comb[i],'.zip'),
                  exdir = dtraw_dir)
    
  }else{
    message("file already downloaded")
  }
}

