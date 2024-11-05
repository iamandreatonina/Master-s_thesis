library(ggplot2)
# library(rstudioapi)
library(dplyr)
library(parallel)
library(data.table)
library(doParallel)
library(parallel)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("/shares/CIBIO-Storage/BCG/scratch2/tirocinanti/tonina/Both_PFB/B_a")

setwd('normal/')
# normal section 

function_normal <- function(val){
  system(paste0('../../../sim64 ../Cell-cycle_500_normal.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_500'))

  system(paste0('../../../sim64 ../Cell-cycle_50_normal.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_50'))
}

mclapply(X=1:100,FUN = function(X)function_normal(X),mc.cores = 10)

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


setwd('..')

bef <- readLines('Cell-cycle_500_normal.prog')
bef50 <- readLines('Cell-cycle_50_normal.prog')

#inhibitor_p part

species_fun <- function(iteration_x,value,specie){
  
  system(paste0('../../../sim64 B_a_Cell-cycle_500_',specie,'.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',iteration_x,'_B_a_results_500_',specie,'_', value))
  
  system(paste0('../../../sim64 B_a_Cell-cycle_50_',specie,'.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',iteration_x,'_B_a_results_50_',specie,'_', value))
}

setwd('inhibitor_p')

for (value in seq(0,1000,50)){
    word <- paste(as.factor(value), 'inhibitor_p', sep = ' ')

    bef1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef)
    writeLines(bef1, con = paste0("B_a_Cell-cycle_500_inh.prog"))
    
    bef50_1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef50)
    writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_inh.prog"))
  
    mclapply(X=1:100,function(X)species_fun(X,value,'inh'),mc.cores = 10)

}

Initial_val500 <- data.frame()
Initial_val50 <- data.frame()
for (file in list.files(pattern = '.E.out')){
  if(grepl('500_inh_[0-9]+.E.out',file)){
      upload <- fread(file, col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <-  strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val500 <- rbind(Initial_val500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'inhibitor_p',TF_a = 500 ))
    }
  }else{
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val50 <- rbind(Initial_val50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'inhibitor_p',TF_a = 50))
    }
  }
}


# E_1 section
setwd('../E_1')

for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'E_1', sep = ' ')
  
  bef1 <- gsub(pattern = "500 E_1", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_E1.prog"))

  bef50_1 <- gsub(pattern = "500 E_1", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_E1.prog"))

  mclapply(X=1:100,FUN = function(X)species_fun(X,value,'E1'),mc.cores = 10)
}


for (file in list.files(pattern = '.E.out')){
  if(grepl('500_E1_[0-9]+.E.out',file)){
    upload <- fread(file, col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <-  strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val500 <- rbind(Initial_val500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'E_1',TF_a = 500 ))
    }
  }else{
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val50 <- rbind(Initial_val50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'E_1',TF_a = 50))
    }
  }
}


# # E_2 section
setwd('../E_2')

for(value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'E_2', sep = ' ')
  bef1 <- gsub(pattern = "500 E_2", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_E2.prog"))

  bef50_1 <- gsub(pattern = "500 E_2", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_E2.prog"))

  mclapply(X=1:100,FUN = function(X)species_fun(X,value,'E2'),mc.cores = 10)
}


for (file in list.files(pattern = '.E.out')){
  if(grepl('500_E2_[0-9]+.E.out',file)){
    upload <- fread(file, col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <-  strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val500 <- rbind(Initial_val500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'E_2',TF_a = 500 ))
    }
  }else{
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val50 <- rbind(Initial_val50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'E_2',TF_a = 50))
    }
  }
}


#TF section
setwd('../TF_a')

for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'TF_a', sep = ' ')
  
  bef1 <- gsub(pattern = "500 TF_a", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_TF.prog"))

  bef50_1 <- gsub(pattern = "50 TF_a", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_TF.prog"))
  
  
  mclapply(X=1:100,FUN = function(X)species_fun(X,value,'TF'),mc.cores = 10)
}

  
for (file in list.files(pattern = '.E.out')){

  if(grepl('500_TF_[0-9]+.E.out',file)){
    upload <- fread(file, col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <-  strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val500 <- rbind(Initial_val500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1],Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'TF_a',TF_a = 500 ))
    }
  }else{
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      number <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
      Initial_val50 <- rbind(Initial_val50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_spec = as.numeric(number[[1]] %>% last()), Specie= 'TF_a',TF_a = 50))
    }
  }
}

# plotting species #####

setwd('../plots')
#times
ggplot(data=Initial_val500 %>% group_by(Specie, Quant_spec) %>% summarise(Mean = mean(Time)), aes(Mean, Specie, fill = Specie)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal500$Time), colour = 'red', lwd =0.2) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_spec)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Species TF_a 500',x='Time' ,y='Species')

ggsave('Sensitivity_Analysis_Species_TF_a_500.pdf', plot = last_plot())

#NB per 50TFa il TFa è identico a modello a 500 -> minimo per partire è TF_a a 100 
ggplot(data=Initial_val50 %>% group_by(Specie, Quant_spec) %>% summarise(Mean = mean(Time)) %>% filter(Specie != 'TF_a'), aes(Mean, Specie, fill = Specie)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal50$Time), colour = 'red', lwd =0.2) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_spec)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Species TF_a 50',x='Time' ,y='Species')
ggsave('Sensitivity_Analysis_Species_TF_a_50.pdf', plot = last_plot())


# percentage 

moment <- Initial_val500 %>% group_by(Specie, Quant_spec) %>% count(Quant_spec) %>% mutate(perc = n)
ggplot(moment, aes(perc,Specie, fill = Specie)) + geom_boxplot() + geom_vline(xintercept = nrow(normal500), colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_spec)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + labs(title = 'Sensitivity Analysis Species TF_a 500',x='Percentage %' ,y='Species')
ggsave('Sensitivity_Analysis_Species_TF_a_500_percentages.pdf', plot = last_plot())


#NB per 50TFa il TFa è identico a modello a 500 -> minimo per partire è TF_a a 100 
moment50 <- Initial_val50 %>% group_by(Specie, Quant_spec) %>% count(Quant_spec) %>% mutate(perc = n) %>% filter(Specie != 'TF_a')
ggplot(moment50, aes(perc,Specie, fill = Specie)) + geom_boxplot() + geom_vline(xintercept = nrow(normal50), colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_spec)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') +labs(title = 'Sensitivity Analysis Species TF_a 50',x='Percentage %' ,y='Species')
ggsave('Sensitivity_Analysis_Species_TF_a_50_percentages.pdf', plot = last_plot())

save.image("env_file_species.Rdata")







