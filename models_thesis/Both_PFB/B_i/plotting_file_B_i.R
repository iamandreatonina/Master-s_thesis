#  File for plotting 

library(ggplot2)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

###500 TF_i####
setwd('TF_i_500')

# 500 steps 
system('../../../sim64 Cell-cycle_500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=B_i_results_500')

file_B_i_500 <- read.delim('B_i_results_500.E.out', col.names = c('time','TR','TR_p','activator','activator_p',
                                                                           'inhibitor','inhibitor_p','TF_a','TF_i','E_1','E_2',
                                                                           'Chp_a','Chp_i','Pho','S','useless'))
file_B_i_500$useless <- NULL

ggplot(file_B_i_500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('B_i - TF on inhibitor 500 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(filename = 'B_i_go_500.png', plot = last_plot())


# 1500 steps 

bef  <- readLines("B_i_Cell-cycle_500.prog")
bef2  <- gsub(pattern = "steps = 1000", replace = "steps = 3000", x = bef)
writeLines(bef2, con="B_i_Cell-cycle_500_1500.prog")


system('../../../sim64 Cell-cycle_500_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=B_i_results_500_1500')

file_B_i_500_1500 <- read.delim('B_i_results_500_1500.E.out', col.names = 
                                   c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                     'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_B_i_500_1500$useless <- NULL

ggplot(file_B_i_500_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle('B_i - TF on inhibitor 500 - GO control - 1500 steps') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'B_i_go_500_1500.png', plot = last_plot())



###50 TF_i####
setwd('../TF_i_50')

# 500 steps 
system('../../../sim64 Cell-cycle_50.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=B_i_results_50')


file_B_i_50 <- read.delim('B_i_results_50.E.out', col.names = c('time','TR','TR_p','activator','activator_p','inhibitor',
                                                                        'inhibitor_p','TF_a','TF_i','E_1','E_2','Chp_a','Chp_i',
                                                                        'Pho','S','useless'))
file_B_i_50$useless <- NULL

ggplot(file_B_i_50) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('B_i - TF on inhibitor 50 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(filename = 'B_i_go_50.png', plot = last_plot())

# 1500 steps 

bef  <- readLines("B_i_Cell-cycle_50.prog")
bef2  <- gsub(pattern = "steps = 1000", replace = "steps = 3000", x = bef)
writeLines(bef2, con="B_i_Cell-cycle_50_1500.prog")


system('../../../sim64 Cell-cycle_50_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=B_i_results_50_1500')

file_B_i_50_1500 <- read.delim('B_i_results_50_1500.E.out', col.names = 
                                  c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                    'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_B_i_50_1500$useless <- NULL

ggplot(file_B_i_50_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle('B_i - TF on inhibitor 50 - GO control - 1500 steps') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'B_i_go_50_1500.png', plot = last_plot())
