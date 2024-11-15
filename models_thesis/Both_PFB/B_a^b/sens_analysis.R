library(ggplot2)
# library(rstudioapi)
library(dplyr)
library(parallel)
library(data.table)
library(doParallel)
library(parallel)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("/shares/CIBIO-Storage/BCG/scratch2/tirocinanti/tonina/Both_PFB/B_a^b")

setwd('normal/')

normal500 <- data.frame()
normal50 <- data.frame()

function_normal <- function(val,TF){
  system(paste0('../../../sim64 ../Cell-cycle_',TF,'_normal.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',as.factor(val),'_B_a^b_results_',TF))
  
  upload <- fread(paste0(as.factor(val),'_B_a^b_results_',TF,'.E.out'), col.names =
                    c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                      'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)

  
  system(paste0('rm ',as.factor(val),'_B_a^b_results_',TF,'*'))
  if (any(as.numeric(upload$TR_p) >= 150)){
    return(data.frame(Time = upload$time[which(upload$TR_p >= 150)][1], Specie = 'Normal', TF_a = TF))
  }
  
}

normal500 <- rbind(normal500,do.call(rbind, mclapply(X=1:100,FUN = function(X)function_normal(X,'500'),mc.cores = 10)))
normal50 <- rbind(normal50,do.call(rbind, mclapply(X=1:100,FUN = function(X)function_normal(X,'50'),mc.cores = 10)))

setwd('..')
# 
bef <- readLines('Cell-cycle_500_normal.prog')
bef50 <- readLines('Cell-cycle_50_normal.prog')

Initial_val500 <- data.frame()
Initial_val50 <- data.frame()
# #inhibitor_p part


species_fun <- function(iteration_x,value,specie,TF){
system(paste0('../../../sim64 B_a^b_Cell-cycle_',TF,'_',specie,'.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=',as.factor(iteration_x),'_B_a^b_results_',TF,'_',specie,'_',as.factor(value)))
  
  upload <- fread(paste0(as.factor(iteration_x),'_B_a^b_results_',TF,'_',specie,'_',as.factor(value),'.E.out'), col.names =
                    c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                      'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
  
  
  system(paste0('rm ',as.factor(iteration_x),'_B_a^b_results_',TF,'_',specie,'_',as.factor(value),'*'))
  if (any(as.numeric(upload$TR_p) >= 150)){
    return(data.frame(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_spec = value, Specie = specie, TF_a = TF))
  }
  
}


setwd('inhibitor_p')

for (value in seq(0,1000,50)){
    word <- paste(as.factor(value), 'inhibitor_p', sep = ' ')

    bef1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef)
    writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_inhibitor_p.prog"))

    bef50_1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef50)
    writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_inhibitor_p.prog"))

    Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'inhibitor_p','500'),mc.cores = 10)))
    
    Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'inhibitor_p','50'),mc.cores = 10)))

}

# E_1 section
setwd('../E_1')
# 
for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'E_1', sep = ' ')

  bef1 <- gsub(pattern = "500 E_1", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_E1.prog"))

  bef50_1 <- gsub(pattern = "500 E_1", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_E1.prog"))

  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'E1','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'E1','50'),mc.cores = 10)))
}

# # E_2 section
setwd('../E_2')
# 
for(value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'E_2', sep = ' ')
  bef1 <- gsub(pattern = "500 E_2", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_E2.prog"))

  bef50_1 <- gsub(pattern = "500 E_2", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_E2.prog"))

  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'E2','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'E2','50'),mc.cores = 10)))}

#TF section
setwd('../TF_a')

for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'TF_a', sep = ' ')

  bef1 <- gsub(pattern = "500 TF_a", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_TF_a.prog"))

  bef50_1 <- gsub(pattern = "50 TF_a", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_TF_a.prog"))

  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'TF_a','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'TF_a','50'),mc.cores = 10)))
}

# Chp_i section

setwd('../Chp_i')

for (value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'Chp_i', sep = ' ')
  
  bef1 <- gsub(pattern = "500 Chp_i", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_Chp_i.prog"))
  
  bef50_1 <- gsub(pattern = "500 Chp_i", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_Chp_i.prog"))
  
  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Chp_i','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Chp_i','50'),mc.cores = 10)))
  
}

# Pho section

setwd('../Pho')

for (value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'Pho', sep = ' ')
  
  bef1 <- gsub(pattern = "500 Pho", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_Pho.prog"))
  
  bef50_1 <- gsub(pattern = "500 Chp_i", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_Pho.prog"))
  
  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Pho','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Pho','50'),mc.cores = 10)))
  
}

# Chp_a section

setwd('../Chp_a')

for (value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'Chp_a', sep = ' ')
  
  bef1 <- gsub(pattern = "500 Chp_a", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a^b_Cell-cycle_500_Chp_a.prog"))
  
  bef50_1 <- gsub(pattern = "500 Chp_a", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a^b_Cell-cycle_50_Chp_a.prog"))
  
  Initial_val500 <- rbind(Initial_val500, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Chp_a','500'),mc.cores = 10)))
  
  Initial_val50 <- rbind(Initial_val50, do.call(rbind,mclapply(X=1:100,function(X)species_fun(X,value,'Chp_a','50'),mc.cores = 10)))
  
}

#parameters
setwd('../parameters')

par_500 <- data.frame()
par_50 <- data.frame()

file_fun <- readLines('../../../Cell_cycle.fun')

param_func <- function(iteration_x,val,parameter,TF){
  
  system(paste0('../../../sim64 ../Cell-cycle_',TF,'_normal.prog ../../../Cell_cycle.types B_a^b_Cell-cycle_',TF,'.fun -o=',as.factor(iteration_x),'_B_a^b_results_',TF,'_',parameter,'_',as.factor(val)))
  
  upload <- fread(paste0(as.factor(iteration_x),'_B_a^b_results_',TF,'_',parameter,'_',as.factor(val),'.E.out'), col.names =
                    c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                      'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
  
  
  system(paste0('rm ',as.factor(iteration_x),'_B_a^b_results_',TF,'_',parameter,'_',as.factor(val),'*'))
  if (any(as.numeric(upload$TR_p) >= 150)){
    return(data.frame(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = val, Parameter = parameter, TF_a = TF))
  }
  

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
      writeLines(bef1, con=paste0("B_a^b_Cell-cycle_500.fun"))
      
      bef4  <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef4, con=paste0("B_a^b_Cell-cycle_50.fun"))
      
      par_500 <- rbind(par_500, do.call(rbind,mclapply(X=1:100,function(X)param_func(X,change,parameter,'500'),mc.cores = 10)))
      
      par_50 <- rbind(par_50, do.call(rbind,mclapply(X=1:100,function(X)param_func(X,change,parameter,'50'),mc.cores = 10)))    
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


# # percentage

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

# plotting parameters  #####
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


save.image("env_total.Rdata")
