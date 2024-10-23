library(ggplot2)
library(rstudioapi)
library(dplyr)
library(parallel)
library(data.table)
library(doParallel)
library(parallel)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#computazione normal both steps = 1500 & 500, also both TF 500 e 50 

# system('mkdir normal')
setwd('normal/')
# 
cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)

foreach(val = 1:100)%dopar%{

  word = paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_500_500')
  system(word)

  word2 = paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500_1500.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_1500_500')
  system(word2)

  word3 = paste0('../../../../sim64 ../../TF_a_50/Cell-cycle_50.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_500_50')
  system(word3)

  word4 = paste0('../../../../sim64 ../../TF_a_50/Cell-cycle_50_1500.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',as.factor(val),'_B_a_results_1500_50')
  system(word4)
}

stopCluster(cl)

normal500 <- data.frame()
normal50 <- data.frame()
# count500 = 0
# count50 = 0

for (file in list.files(pattern = '.E.out')){

  if(endsWith(file, "500.E.out")){

    # count500 = count500 +1
    upload <- fread(file, col.names =
                    c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                      'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      normal500 <- rbind(normal500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], TF_a = 500))
    }
  }else{

    # count50 = count50 +1
    upload <- fread(file, col.names =
                      c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      normal50 <- rbind(normal50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], TF_a = 50))
    }

  }
}
median(normal500$Time) #268.4988
mean(normal500$Time) #270.0764

median(normal50$Time) # vuoti 
mean(normal50$Time) # vuoti 

setwd('..')

bef <- readLines('../TF_a_500/Cell-cycle_500.prog')
bef2 <- readLines('../TF_a_500/Cell-cycle_500_1500.prog')
bef50 <- readLines('../TF_a_50/Cell-cycle_50.prog')
bef50_1500 <-  readLines('../TF_a_50/Cell-cycle_50_1500.prog')

setwd('inhibitor_p')

cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)
# system('mkdir inhibitor_p TF_a E_1 E_2 parameters')
for (value in seq(0,1000,50)){
    word <- paste(as.factor(value), 'inhibitor_p', sep = ' ')

    bef1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef)
    writeLines(bef1, con = paste0("B_a_Cell-cycle_500_inh.prog"))
    bef3 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef2)
    writeLines(bef3, con = paste0("B_a_Cell-cycle_1500_500_inh.prog"))
    bef50_1 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef50)
    writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_inh.prog"))
    bef50_3 <- gsub(pattern = "500 inhibitor_p", replace = word, x = bef50_1500)
    writeLines(bef50_3, con = paste0("B_a_Cell-cycle_1500_50_inh.prog"))
    
  foreach (k  = 1:100) %dopar% {
   
  # inhibitor (500 TF -500 steps)
  system(paste0('../../../../sim64 B_a_Cell-cycle_500_inh.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_500_inh_', value))
  
  # Second simulation inhibitor (500 TF - 1500 steps)
  system(paste0('../../../../sim64 B_a_Cell-cycle_1500_500_inh.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_500_inh_', value))
  # Third simulation inhibitor (50 TF - 500 steps )
  system(paste0('../../../../sim64 B_a_Cell-cycle_50_inh.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_50_inh_', value))
  #   #
  # Fourth simulation inhibitor (50 TF - 1500 steps )
  system(paste0('../../../../sim64 B_a_Cell-cycle_1500_50_inh.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_50_inh_', value))
  }
}

stopCluster(cl)


# quasi 1 GB di cartella, chiedere a romanel consiglio su codice 

# count = 0
Initial_val500 <- data.frame()
Initial_val50 <- data.frame()
for (file in list.files(pattern = '.E.out')){
  # n <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
  # print(n[[1]] %>% last())
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

  ###
  
  

# E_1 section
setwd('../E_1')
cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)
    
for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'E_1', sep = ' ')
  
  bef1 <- gsub(pattern = "500 E_1", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_E1.prog"))
  bef3 <- gsub(pattern = "500 E_1", replace = word, x = bef2)
  writeLines(bef3, con = paste0("B_a_Cell-cycle_1500_500_E1.prog"))
  bef50_1 <- gsub(pattern = "500 E_1", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_E1.prog"))
  bef50_3 <- gsub(pattern = "500 E_1", replace = word, x = bef50_1500)
  writeLines(bef50_3, con = paste0("B_a_Cell-cycle_1500_50_E1.prog"))
  
  foreach (k = 1:100) %dopar% {
    
    # inhibitor (500 TF -500 steps)
    
    system(paste0('../../../../sim64 B_a_Cell-cycle_500_E1.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_500_E1_', value))
    
    # # Second simulation inhibitor (500 TF - 1500 steps)
   
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_500_E1.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_500_E1_', value))
    #
    # # Third simulation inhibitor (50 TF - 500 steps )
    
    system(paste0('../../../../sim64 B_a_Cell-cycle_50_E1.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_50_E1_', value))
    #
    # # Fourth simulation inhibitor (50 TF - 1500 steps )
    
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_50_E1.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_50_E1_', value))
  }
}

stopCluster(cl)

for (file in list.files(pattern = '.E.out')){
  # n <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
  # print(n[[1]] %>% last())
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
cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)

for(value in seq(0,1000,50)){
  word <- paste(as.factor(value), 'E_2', sep = ' ')
  bef1 <- gsub(pattern = "500 E_2", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_E2.prog"))
  bef3 <- gsub(pattern = "500 E_2", replace = word, x = bef2)
  writeLines(bef3, con = paste0("B_a_Cell-cycle_1500_500_E2.prog"))
  bef50_1 <- gsub(pattern = "500 E_2", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_E2.prog"))
  bef50_3 <- gsub(pattern = "500 E_2", replace = word, x = bef50_1500)
  writeLines(bef50_3, con = paste0("B_a_Cell-cycle_1500_50_E2.prog"))
  
  foreach (k = 1:100) %dopar% {
    
    # inhibitor (500 TF -500 steps)

    system(paste0('../../../../sim64 B_a_Cell-cycle_500_E2.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_500_E2_', value))
    
    # # Second simulation inhibitor (500 TF - 1500 steps)
   
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_500_E2.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_500_E2_', value))
    #
    # # Third simulation inhibitor (50 TF - 500 steps )
   
    system(paste0('../../../../sim64 B_a_Cell-cycle_50_E2.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_50_E2_', value))
    #
    # # Fourth simulation inhibitor (50 TF - 1500 steps )
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_50_E2.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_50_E2_', value))
  }
}

stopCluster(cl)


for (file in list.files(pattern = '.E.out')){
  # n <- strsplit(file,'.',fixed = T)[[1]][1] %>% strsplit(split = '_',fixed = T)
  # print(n[[1]] %>% last())
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

cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)
    
for (value in seq(0,1000,50)) {
  word <- paste(as.factor(value), 'TF_a', sep = ' ')
  
  bef1 <- gsub(pattern = "500 TF_a", replace = word, x = bef)
  writeLines(bef1, con = paste0("B_a_Cell-cycle_500_TF.prog"))
  bef3 <- gsub(pattern = "500 TF_a", replace = word, x = bef2)
  writeLines(bef3, con = paste0("B_a_Cell-cycle_1500_500_TF.prog"))
  bef50_1 <- gsub(pattern = "50 TF_a", replace = word, x = bef50)
  writeLines(bef50_1, con = paste0("B_a_Cell-cycle_50_TF.prog"))
  bef50_3 <- gsub(pattern = "50 TF_a", replace = word, x = bef50_1500)
  writeLines(bef50_3, con = paste0("B_a_Cell-cycle_1500_50_TF.prog"))
  
  foreach (k = 1:100) %dopar% {
    
    # inhibitor (500 TF -500 steps)
 
    system(paste0('../../../../sim64 B_a_Cell-cycle_500_TF.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_500_TF_', value))
    
    # # Second simulation inhibitor (500 TF - 1500 steps)
    
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_500_TF.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_500_TF_', value))
    #
    # # Third simulation inhibitor (50 TF - 500 steps )
   
    system(paste0('../../../../sim64 B_a_Cell-cycle_50_TF.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_50_TF_', value))
    #
    # # Fourth simulation inhibitor (50 TF - 1500 steps )
    system(paste0('../../../../sim64 B_a_Cell-cycle_1500_50_TF.prog ../../../../Cell_cycle.types ../../../../Cell_cycle.fun -o=',k,'_B_a_results_1500_50_TF_', value))
  }
}
stopCluster(cl)

  
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

#times
ggplot(data=Initial_val500 %>% group_by(Specie, Quant_spec) %>% summarise(Mean = mean(Time)), aes(Mean, Specie, fill = Specie)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal500$Time), colour = 'red', lwd =0.2) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_spec)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Time') + ylab('Species')

#NB per 50TFa il TFa è identico a modello a 500 -> minimo per partire è TF_a a 100 
ggplot(data=Initial_val50 %>% group_by(Specie, Quant_spec) %>% summarise(Mean = mean(Time)), aes(Mean, Specie, fill = Specie)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal50$Time), colour = 'red', lwd =0.2) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_spec)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Time') + ylab('Species')


# percentage 

moment <- Initial_val500 %>% group_by(Specie, Quant_spec) %>% count(Quant_spec) %>% mutate(perc = n/2)
ggplot(moment, aes(perc,Specie, fill = Specie)) + geom_boxplot() + geom_vline(xintercept = nrow(normal500)/2, colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_spec)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Percentage %') + ylab('Species')


#NB per 50TFa il TFa è identico a modello a 500 -> minimo per partire è TF_a a 100 
moment50 <- Initial_val50 %>% group_by(Specie, Quant_spec) %>% count(Quant_spec) %>% mutate(perc = n/2)
ggplot(moment50, aes(perc,Specie, fill = Specie)) + geom_boxplot() + geom_vline(xintercept = nrow(normal50)/2, colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_spec)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Percentage %') + ylab('Species')

##### parameters 
setwd('../parameters')


cl <- makeCluster(detectCores(logical = TRUE)-2)
registerDoParallel(cl)

file_fun <- readLines('../../../../Cell_cycle.fun')


for (par in file_fun[1:40]){
  value = strsplit(par,' ')[[1]][6]
  parameter = strsplit(par,' ')[[1]][2]
  if (value != '1/500' ){
    value = as.numeric(value)
    for (j in c(seq(0.1,1,0.1),seq(2,10,1))){
      # print(j)
      change = j * value
      word <- paste0('let ',parameter,' : const = ',as.factor(change),' ;')
      bef1 <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef1, con=paste0("B_a_Cell-cycle_500.fun"))
      bef3  <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef3, con=paste0("B_a_Cell-cycle_500_1500.fun"))
      bef4  <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef4, con=paste0("B_a_Cell-cycle_50.fun"))
      bef5  <- gsub(pattern = par, replace = word, x = file_fun)
      writeLines(bef5, con=paste0("B_a_Cell-cycle_50_1500.fun"))
      
      # if(grepl('_',parameter)){
      # 
      # }
      foreach(u =  1:100) %dopar% {
       
        system(paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500.prog ../../../../Cell_cycle.types B_a_Cell-cycle_500.fun -o=',u,'_B_a_results_500_',parameter,'_',as.factor(change)))
        
        system(paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500.prog ../../../../Cell_cycle.types B_a_Cell-cycle_500_1500.fun -o=',u,'_B_a_results_1500_500_',parameter,'_',as.factor(change)))
        
        system(paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500.prog ../../../../Cell_cycle.types B_a_Cell-cycle_50.fun -o=',u,'_B_a_results_50_',parameter,'_',as.factor(change)))
      
        system(paste0('../../../../sim64 ../../TF_a_500/Cell-cycle_500.prog ../../../../Cell_cycle.types B_a_Cell-cycle_50_1500.fun -o=',u,'_B_a_results_1500_50_',parameter,'_',as.factor(change)))
        
      }
      
      }
    }
    
  }
  
stopCluster(cl)

par_500 <- data.frame()
par_50 <- data.frame()

# 
# count500 = 0 
# count50 = 0 

for (file in list.files(pattern = '.E.out')){
  
  if(grepl('1500_500_[a-z]+_[[:graph:]]+.E.out',file)){
    upload <- fread(file, col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                        'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      numb <-  strsplit(file,'.E.out',fixed = T)[[1]][1] %>% strsplit(split = '_B_a_results_1500_500_',fixed = T)
      numb <- numb[[1]][2] %>% strsplit('_',fixed = T)
      if(length(numb[[1]]) == 3){
        param <- paste0(numb[[1]][1],numb[[1]][2])
        par_500 <- rbind(par_500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 500, Steps = 1500 ))

      }else{
        param <- numb[[1]][1]
        par_500 <- rbind(par_500,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 500, Steps = 1500 ))

      }
    }
  }else if(grepl('1500_50_[a-z]+_[[:graph:]]+.E.out',file)){
    upload <- fread(file, col.names =  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                         'E_2','Chp_a','Chp_i','Pho','S','null'), data.table = F,sep = '\t',fill = T)
    if (any(as.numeric(upload$TR_p) >= 150)){
      numb <-  strsplit(file,'.E.out',fixed = T)[[1]][1] %>% strsplit(split = '_B_a_results_1500_50_',fixed = T)
      numb <- numb[[1]][2] %>% strsplit('_',fixed = T)
      if(length(numb[[1]]) == 3){
        param <- paste0(numb[[1]][1],numb[[1]][2])
        par_50 <- rbind(par_50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 50, Steps = 1500 ))

      }else{
        param <- numb[[1]][1]
        par_50 <- rbind(par_50,tibble(Time = upload$time[which(upload$TR_p >= 150)][1], Quant_par = as.numeric(numb[[1]] %>% last()), Parameter= param,TF_a = 50, Steps = 1500 ))

         }
       }
  }else if(grepl('[[:alpha:]]_50_[a-z]+_[[:graph:]]+.E.out',file)) {
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



# plotting parameters  #####

#times
ggplot(data=par_500 %>% group_by(Parameter, Quant_par) %>% summarise(Mean = mean(Time)), aes(Mean, Parameter, fill = Parameter)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal500$Time), colour = 'red', lwd =1) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_par)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Time') + ylab('Species')

ggplot(data=par_50 %>% group_by(Parameter, Quant_par) %>% summarise(Mean = mean(Time)), aes(Mean, Parameter, fill = Parameter)) +
  geom_boxplot() + geom_vline(xintercept = mean(normal50$Time), colour = 'red', lwd =1) + 
  geom_point(position=position_dodge(),aes(group = Mean, colour = Quant_par)) + 
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Time') + ylab('Species')

# percentage 

moment1 <- par_500 %>% group_by(Parameter, Quant_par) %>% count(Quant_par) %>% mutate(perc = n/2)
ggplot(moment1, aes(perc,Parameter, fill = Parameter)) + geom_boxplot() + geom_vline(xintercept = nrow(normal500)/2, colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_par)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Percentage %') + ylab('Species')

moment2 <- par_50 %>% group_by(Parameter, Quant_par) %>% count(Quant_par) %>% mutate(perc = n/2)
ggplot(moment2, aes(perc,Parameter, fill = Parameter)) + geom_boxplot() + geom_vline(xintercept = nrow(normal50)/2, colour = 'red', lwd= 0.2) + 
  geom_point(position = position_dodge(),aes(group = perc , colour = Quant_par)) +
  scale_colour_gradient(low = '#605b8f',high = '#de425b') + xlab('Percentage %') + ylab('Species')








