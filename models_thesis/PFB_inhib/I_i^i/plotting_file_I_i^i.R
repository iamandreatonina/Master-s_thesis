#  File for plotting 
library(ggplot2)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#TF_i_500
setwd('TF_i_500')

system('../../../sim64 I_i^i_Cell-cycle_500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=I_i^i_results_500')

file_I_ii_500 <- read.delim('I_i^i_results_500.E.out', col.names = c('time','TR','TR_p','activator','activator_p',
                                                                           'inhibitor','inhibitor_p','TF_a','TF_i','E_1','E_2',
                                                                           'Chp_a','Chp_i','Pho','S','useless'))
file_I_ii_500$useless <- NULL

ggplot(file_I_ii_500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i^i - TF on inhibitor 500 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(plot = last_plot(),filename = 'I_i^i_go_500.png')


system('../../../sim64 I_i^i_Cell-cycle_500_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=I_i^i_results_500_1500')

file_I_ii_500_1500 <- read.delim('I_i^i_results_500_1500.E.out', col.names = c('time','TR','TR_p','activator','activator_p',
                                                                  'inhibitor','inhibitor_p','TF_a','TF_i','E_1','E_2',
                                                                  'Chp_a','Chp_i','Pho','S','useless'))
file_I_ii_500_1500$useless <- NULL

ggplot(file_I_ii_500_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i^i - TF on inhibitor 500 - GO control - 1500 steps') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(plot = last_plot(),filename = 'I_i^i_go_500_1500.png')
# TF_i_50

setwd('../TF_i_50')

system('../../../sim64 I_i^i_Cell-cycle_50.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=I_i^i_results_50')

file_I_ii_50 <- read.delim('I_i^i_results_50.E.out', col.names = c('time','TR','TR_p','activator','activator_p','inhibitor',
                                                                        'inhibitor_p','TF_a','TF_i','E_1','E_2','Chp_a','Chp_i',
                                                                        'Pho','S','useless'))
file_I_ii_50$useless <- NULL

ggplot(file_I_ii_50) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i^i - TF on inhibitor 50 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(filename = 'I_i^i_go_50.png', plot = last_plot())

system('../../../sim64 I_i^i_Cell-cycle_50_1500.prog ../../../Cell_cycle.types ../../../Cell_cycle.fun -o=I_i^i_results_50_1500')

file_I_ii_50_1500 <- read.delim('I_i^i_results_50_1500.E.out', col.names = c('time','TR','TR_p','activator','activator_p','inhibitor',
                                                                'inhibitor_p','TF_a','TF_i','E_1','E_2','Chp_a','Chp_i',
                                                                'Pho','S','useless'))
file_I_ii_50_1500$useless <- NULL

ggplot(file_I_ii_50_1500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i^i - TF on inhibitor 50 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

ggsave(filename = 'I_i^i_go_50_1500.png', plot = last_plot())

