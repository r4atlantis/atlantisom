#Testing RNetCDF

library(RNetCDF); library(arrayhelpers)

#May need to check numbering (index starts from 1 in R)

test_load_ncdf <- function(scenario, path = ".", groups, var){
  ncfile <- file.path(path, paste(scenario, '_.nc', sep = ''))
  nc <- open.nc(ncfile)
  all.var.out <- c()
  for(v in 1:length(var)){
    for(gr in 1:length(groups)){
      group.var.out <- c()
      for(co in 1:10){
        group.var <- paste(groups[gr], co, '_', var, sep = '')
        group.var.array <- var.get.nc(nc, group.var)
        group.var.dt <- as.data.table(array2df(group.var.array))
        group.var.dt[, agecl := co]
        group.var.out <- rbindlist(list(group.var.out, group.var.dt))
      }
      setnames(group.var.out, c('group.var.array', 'd1', 'd2', 'd3'),
               c('atoutput', 'layer', 'polygon', 'time'))
      group.var.out[, species := groups[gr]]
      setcolorder(group.var.out, c('species', 'agecl', 'polygon', 'layer', 'time',
                                   'atoutput'))
      all.var.out <- rbindlist(list(all.var.out, group.var.out))
    }
    if(v == 1) load.out <- all.var.out
    if(v == 2){
      load.out <- list(load.out, all.var.out)
      names(load.out)[1:2] <- var[1:2]
    }
    if(v > 2){
      load.out[[v]] <- all.var.out
      names(load.out)[v] <- var[v]
    }

  }

  return(load.out)
}

scenario <- 'neusDynEffort_Summit_2c'
path <- 'C:/Users/Sean.Lucey/Desktop/Atlantis_Summit/Atlantis_Summit_2c_MPA_50'

groups <- c('Pisciv_D_Fish', 'Pisciv_S_Fish', 'Pisciv_B_Fish')
var <- c('Nums', 'ResN', 'StructN')

neus.out <- test_load_ncdf(scenario, path, groups, var)









neus <- open.nc(ncfile)
print(neus)

ndims <- file.inq.nc(neus)$ndims
dimnames <- character(ndims)
for(i in seq_len(ndims)) {
  dimnames[i] <- dim.inq.nc(neus, i-1)$name
}

varnames <- 1:file.inq.nc(nc)$nvars
for(i in 1:length(varnames)){
  varnames[i] <- var.inq.nc(nc, i-1)$name
}

var <- 'Pisciv_D_Fish3_Nums'

var.inq.nc(neus, var)

Nums <- var.get.nc(neus, var)


dietfile <- "C:\\Users\\Sean.Lucey\\Desktop\\Atlantis_Summit\\Poisiden\\RdemoPlotDietTimeseries\\outputCCV3DietCheck.txt"
