library(ggplot2)
# library(rstudioapi)
library(dplyr)
library(parallel)
library(data.table)
library(doParallel)
library(parallel)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("/shares/CIBIO-Storage/BCG/scratch2/tirocinanti/tonina/Both_PFB/B_a")
##### parameters 
setwd('parameters/')

file_fun <- readLines('../../../Cell_cycle.fun')

param_func <- function(u,parameter,change){
  
  system(paste0('../../../sim64 ../Cell-cycle_500_normal.prog ../../../Cell_cycle.types B_a_Cell-cycle_500.fun -o=',u,'_B_a_results_500_',parameter,'_',as.factor(change)))
  
  system(paste0('../../../sim64 ../Cell-cycle_50_normal.prog ../../../Cell_cycle.types B_a_Cell-cycle_50.fun -o=',u,'_B_a_results_50_',parameter,'_',as.factor(change)))
  
}

for (par in file_fun[1:40]){
  value = strsplit(par,' ')[[1]][6]
  parameter = strsplit(par,' ')[[1]][2]
  if (value != '1/500' ){
    value = as.numeric(value)
    for (j in c(seq(0.1,1,0.1),seq(2,10,1))){
      change = j * value
      word <- paste0('let ',parameter,' : const = ',as.factor(change),' ;')
      bef1 <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef1, con=paste0("B_a_Cell-cycle_500.fun"))
      
      bef4  <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef4, con=paste0("B_a_Cell-cycle_50.fun"))
      
      mclapply(X=1:100,FUN= function(X)param_func(X,parameter,change),mc.cores = 10)
    }
  }
  
}

par_500 <- data.frame()
par_50 <- data.frame()

for (file in list.files(pattern = '.E.out')){
  
  if(grepl('[[:alpha:]]_50_[a-z]+_[[:graph:]]+.E.out',file)) {
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      numb <-  strsplit(file,'.E.out',fixed = T)[[1]][1] %>% strsplit(split = '_B_a_results_50_',fixed = T)
      numb <- numb[[1]][2] %>% strsplit('_',fixed = T)
      if(length(numb[[1]]) == 3){
        param <- paste0(numb[[1]][1],numb[[1]][2])
        par_50 <- rbind(par_50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 50, Steps = 500 ))
        
      }else{
        param <- numb[[1]][1]
        par_50 <- rbind(par_50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 50, Steps = 500 ))
        
      }
    }
  }else{
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      numb <-  strsplit(file,'.E.out',fixed = T)[[1]][1] %>% strsplit(split = '_B_a_results_500_',fixed = T)
      numb <- numb[[1]][2] %>% strsplit('_',fixed = T)
      if(length(numb[[1]]) == 3){
        param <- paste0(numb[[1]][1],numb[[1]][2])
        par_500 <- rbind(par_500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 500, Steps = 500 ))
        
      }else{
        param <- numb[[1]][1]
        par_500 <- rbind(par_500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 500, Steps = 500 ))
        
      }
    }
    
  }
}

setwd('../normal')
normal500 <- data.frame()
normal50 <- data.frame()

for (file in list.files(pattern = '.E.out')){
  
  if(endsWith(file, "500.E.out")){
    
    upload <- fread(file, col.names =
                      c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      normal500 <- rbind(normal500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], TF_a = 500))
    }
  }else{
    
    upload <- fread(file, col.names =
                      c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      normal50 <- rbind(normal50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], TF_a = 50))
    }
    
  }
}


# plotting parameters  #####

setwd('../plots')

#times
ggplot(data=par_500 %>% group_by(Parameter, Quant_par) %>% summarise(Mean = mean(Time)), aes(Mean, Parameter, fill = Parameter)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal500$Time), colour = 'red', lwd =1) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_par)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Parameters TF_a 500',x='Time' ,y='Parameters')
ggsave('Sensitivity_Analysis_Parameteres_TF_a_500.pdf', plot = last_plot())


ggplot(data=par_50 %>% group_by(Parameter, Quant_par) %>% summarise(Mean = mean(Time)), aes(Mean, Parameter, fill = Parameter)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal50$Time), colour = 'red', lwd =1) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_par)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') +labs(title = 'Sensitivity Analysis Parameters TF_a 50',x='Time' ,y='Parameters')
ggsave('Sensitivity_Analysis_Parameteres_TF_a_50.pdf', plot = last_plot())


# percentage 

moment1 <- par_500 %>% group_by(Parameter, Quant_par) %>% count(Quant_par) %>% mutate(perc = n)
ggplot(moment1, aes(perc,Parameter, fill = Parameter)) + geom_boxplot() + geom_vline(xintercept = nrow(normal500), colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_par)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Parameters TF_a 500',x='Percentage %' ,y='Parameters')
ggsave('Sensitivity_Analysis_Parameteres_TF_a_500_percentage.pdf', plot = last_plot())


moment2 <- par_50 %>% group_by(Parameter, Quant_par) %>% count(Quant_par) %>% mutate(perc = n)
ggplot(moment2, aes(perc,Parameter, fill = Parameter)) + geom_boxplot() + geom_vline(xintercept = nrow(normal50), colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_par)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Parameters TF_a 50',x='Percentage %' ,y='Parameters')
ggsave('Sensitivity_Analysis_Parameteres_TF_a_50_percentage.pdf', plot = last_plot())


save.image("env_file_par.Rdata")