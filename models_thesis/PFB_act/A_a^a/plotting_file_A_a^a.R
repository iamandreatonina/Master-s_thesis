#  File for plotting 

library(ggplot2)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

###500 TF_a####
setwd('TF_a_500')

# 500 steps 

system('../../../sim64 A_a^a_Cell-cycle_500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=A_a^a_results_500')

file_A_aa_500 <- read.delim('A_a^a_results_500.E.out', col.names = 
                             c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                               'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_A_aa_500$useless <- NULL

ggplot(file_A_aa_500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'A_a^a - TF on activator 500 - STOP control') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'A_a^a_stop_500.png', plot = last_plot())

# 1500 steps 

bef  <- readLines("A_a^a_Cell-cycle_500.prog")
bef2  <- gsub(pattern = "steps = 1000", replace = "steps = 3000", x = bef)
writeLines(bef2, con="A_a^a_Cell-cycle_500_1500.prog")


system('../../../sim64 A_a^a_Cell-cycle_500_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=A_a^a_results_500_1500')

file_A_aa_500_1500 <- read.delim('A_a^a_results_500_1500.E.out', col.names = 
                              c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_A_aa_500_1500$useless <- NULL

ggplot(file_A_aa_500_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'A_a^a - TF on activator 500 - STOP control - 1500 steps') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'A_a^a_stop_500_1500.png', plot = last_plot())


#TF_a 50 ####

setwd('../TF_a_50')


#500 steps 

system('../../../sim64 A_a^a_Cell-cycle_50.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=A_a^a_results_50')

file_A_aa_50 <- read.delim('A_a^a_results_50.E.out', 
                          col.names = c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p'
                                        ,'TF_a','TF_i','E_1','E_2','Chp_a','Chp_i','Pho','S','useless'))
file_A_aa_50$useless <- NULL

ggplot(file_A_aa_50) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'A_a^a - TF on activator 50 - STOP control') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'A_a^a_stop_50.png', plot = last_plot())

# 1500 steps 

bef  <- readLines("A_a^a_Cell-cycle_50.prog")
bef2  <- gsub(pattern = "steps = 1000", replace = "steps = 3000", x = bef)
writeLines(bef2, con="A_a^a_Cell-cycle_50_1500.prog")


system('../../../sim64 A_a^a_Cell-cycle_50_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=A_a^a_results_50_1500')

file_A_aa_50_1500 <- read.delim('A_a^a_results_50_1500.E.out', col.names = 
                                   c('time','TR','TR_p','activator','activator_p','inhibitor','inhibitor_p','TF_a','TF_i','E_1',
                                     'E_2','Chp_a','Chp_i','Pho','S','useless'))
file_A_aa_50_1500$useless <- NULL

ggplot(file_A_aa_50_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red')) +
  ggtitle( 'A_a^a - TF on activator 50 - STOP control - 1500 steps') +
  xlab( 'Time') + 
  ylab( 'Molecule number')


ggsave(filename = 'A_a^a_stop_50_1500.png', plot = last_plot())
