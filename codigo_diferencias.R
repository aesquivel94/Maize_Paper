##############################################################################################
##########MINIMUM DIFERENCES
##BY: Eliana Vallejo
#packages
##########################################################################################
list.of.packages <- c("doMC", "dplyr","maptools","plyr","reshape2","reshape2","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
library(doMC);library(dplyr);library(plyr);library(reshape2);library(data.table)
########################################################################################################################################################################
########################################################################################################################################################################
#######paramas
##paths
dir_in<-"//dapadfs/Workspace_cluster_9/Agroclimas/Modelacion_de_cultivos/DSSAT_runs/20170517_Maize"   ###data path
name_csv<-"201709192149_summary"  ##name csv to read
dir_out<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_3_agro-climatic_forecast/crop/Maize"   ##path saved results 
#####################quantiles, etc.
q1<-0.001   ##quantile para hallar las diferencias minimas por corrida y tratamiento por bloques de datos
q2<-0.25   ##quantile para hallar las minimas diferencias en todos los bloques
nc<-2000000 ##number chunks 
var_c<-0 ##If it is "1" only calculates differences with the complete variables without NA
wr_big_d<-"yes"  ###If it is "yes" write big differences that would be wrongs
########################################################################################################################################################################
########################################################################################################################################################################
######FUNCTIONS
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)){stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
##calcula la diferencia entre simulado y observado por variable
var_dir<-function(v,datos){
  datos_c<-datos[,c("RUN","TR",names(datos)[is.na(gsub(var[v],NA,names(datos)))]),with=F]
  names(datos_c)[3:4]<-c("obs","sim")
  datos_c[datos_c==-99]<-NA
  datos_c[datos_c<0]<-NA
  datos_c$difer<-abs(as.numeric(datos_c$obs)-as.numeric(datos_c$sim))/as.numeric(datos_c$obs)
  return(datos_c)
}
####END FUNCTIONS
####variables
col_namess<-mgsub(c("run","treatment","meas.", "out."),c("RUN","TR","M_","S_"),colnames(read.csv(paste0(dir_in,"/",name_csv,".csv"), nrows = 1,sep="\t",header=T)))
col_namess<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "1", col_namess)
exclude_vars<-col_namess[!is.na(mgsub(c("RUN","TR","M1","S1"),rep(NA,4),col_namess))]
##step1: diferencias minimas por cada tratamiento
registros<-nrow(fread(paste0(dir_in,"/",name_csv,".csv"), select = 1L))
sk<-seq(1,registros,nc)
min_difer<-list()
big_difer<-list()
for(i in seq_along(sk)){
  datos<-fread(paste0(dir_in,"/",name_csv,".csv"),skip=sk[i],nrows=2000000,header=F, col.names=col_namess)
  datos<-datos[,-which(colnames(datos)%in%exclude_vars),with=F] 
  var<-unique(mgsub(c("S1","M1"),rep("",2),names(datos)[-1:-2]))
  registerDoMC(cores=4)
  difers<-foreach(v=seq_along(var),.export=c("datos","var")) %dopar%{var_dir(v,datos)}        
  names(difers)<-var
  difers<-ldply(difers)
  names(difers)[1]<-"var"
  big_difer[[i]]<-difers[difers$difer>1,]
  difers<-difers[!is.na(difers$var)&difers$difer<=1,]
  difers<-reshape(transform(difers[,-4:-5], id=1), idvar=c('id',"TR","RUN"), timevar='var', direction='wide')
  difers$id<-NULL
  colnames(difers)<-mgsub(c("difer.NA","difer."),c(NA,""),colnames(difers))
  difers<-difers[,na.omit(colnames(difers))]
  difers$count_na<-rowSums(!is.na(difers[,-1:-2]))/(ncol(difers)-2)
  difers<-difers[difers$count_na>=var_c,];difers$count_na<-NULL
  difers[mapply(is.infinite, difers)] <- NA
  difers$difer<-rowMeans(difers[,-1:-2],na.rm=T)
  difers<-difers[!is.na(difers$difer),]
  difers1<-difers[,c(1,2,ncol(difers))]%>%group_by(TR,RUN)%>%summarise_each(funs(mean(.,na.rm=T)))  ##diferencia promedio
  difers1<-difers1[,-2]%>%group_by(TR)%>%summarise_each(funs(quantile(.,q1,na.rm=T)))
  difers1<-split(difers1,difers1$TR)
  difers<-split(difers,difers$TR)
  difers<-ldply(lapply(seq_along(difers),function(x)difers[[x]][difers[[x]]$difer<=difers1[[x]]$difer,]))
  min_difer[[i]]<-difers
}
rm(datos,difers,difers1)
difer_by_bloq<-unlist(lapply(min_difer,nrow))  
min_difer<-ldply(min_difer)
min_difer<-min_difer[,!is.na(gsub("NA",NA,colnames(min_difer)))]
quantiless<-min_difer[,c("TR","difer")]%>%group_by(TR)%>%summarise_each(funs(quantile(min_difer$difer,q2)))
difer_tr<-split(min_difer,min_difer$TR)
quantiless<-split(quantiless,quantiless$TR)
min_by_tr<-ldply(lapply( levels(as.factor(min_difer$TR)),function(x)difer_tr[[x]][difer_tr[[x]]$difer<=quantiless[[x]]$difer,]))
####min_difer_general
min_g<-min_by_tr[min_by_tr$difer<=quantile(min_by_tr,q2,na.rm=T),]
###errores
big_difer<-ldply(big_difer)
big_difer<-big_difer[!is.na(big_difer$difer),]
######write results
write.csv(min_by_tr,paste0(dir_out,"/min_by_TR_",q1*100,"_",name_csv,".csv"),row.names=F)
write.csv(min_g,paste0(dir_out,"/min_run_",q2*100,"_",name_csv,".csv"),row.names=F)
if(wr_big_d=="yes"){write.csv(big_difer,paste0(dir_out,"/max_difer_",name_csv,".csv"),row.names=F)}
