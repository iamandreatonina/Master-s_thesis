library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

counter <- 0

listina <- list()
check_times = 0
times <- list()

for (i in 1:200){
  
  system('../../../sim64 ../I_a_Cell-cycle_500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=try_results')
  
  file <- read.delim('try_results.E.out', col.names = 
                    c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                    'E_2','Chp_a','Chp_i','Pho','S','useless'))
  
  listina[[length(listina)+1]] = any(file$TR_p >= 150)
  # listina <- any(file$TR_p >= 150)
  
  check_times = check_times + 1
  print(check_times)
  
  
  if (any(file$TR_p >= 150)){
    times[[length(times)+1]] = file$time[which(file$TR_p >= 150)][1]
    counter = counter + 1
    print('switch')
  }

}

print(any(listina))
print(counter)

times2 <- t(as.data.frame(times))
boxplot(times2)
summary(times2)
