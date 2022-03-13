#
# download data
#
rm(list=ls())
`%nin%` = Negate(`%in%`)
dir.create("data-raw")
dir.create("data-raw/anac")


# years to download
range_years <- c(2017:2020)




# download combinada data ---------------------------------
df_list <- lapply(X = range_years, 
                  FUN = function(i){ 
                        temp <- flightsbr::read_flights(date = i,type = 'combinada')
                        return(temp) }
                  )

# rowbind all years
df <- data.table::rbindlist(df_list)

# save data to disk
data.table::fwrite(x = df, file = './data-raw/ANAC/flights_combinada_2017-2020.csv')




# download basica data ---------------------------------
df_list2 <- lapply(X = range_years, 
                  FUN = function(i){ 
                    temp <- flightsbr::read_flights(date = i,type = 'basica')
                    return(temp) }
                  )

# rowbind all years
df2 <- data.table::rbindlist(df_list2)

# save data to disk
data.table::fwrite(x = df2, file = './data-raw/ANAC/flights_basica_2017-2020.csv')


