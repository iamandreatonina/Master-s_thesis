#  File for plotting 

library(ggplot2)

file_I_I_500 <- read.delim('TF_i_500/I_i_results_500.E.out', col.names = c('time','TR','TR_p','activator','activator_p',
                                                                           'inhibitor','inhibitor_p','TF_a','TF_i','E_1','E_2',
                                                                           'Chp_a','Chp_i','Pho','S','useless'))
file_I_I_500$useless <- NULL

ggplot(file_I_I_500) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i - TF on inhibitor 500 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

setwd('TF_i_500')
ggsave(plot = last_plot(),filename = 'I_i_go_500.png')

setwd('../')

file_I_I_50 <- read.delim('TF_i_50/I_i_results_50.E.out', col.names = c('time','TR','TR_p','activator','activator_p','inhibitor',
                                                                        'inhibitor_p','TF_a','TF_i','E_1','E_2','Chp_a','Chp_i',
                                                                        'Pho','S','useless'))
file_I_I_50$useless <- NULL

ggplot(file_I_I_50) + 
  geom_line(aes(x=time,y=TR_p, color = 'TR_p')) + 
  geom_hline(yintercept = 150, linetype = 'dashed', color = 'gray') +
  geom_line(aes(x=time, y= activator_p, color = 'activator_p')) + 
  geom_line(aes(x=time, y = inhibitor_p, color = 'inhibitor_p')) +
  scale_color_manual( name = 'legend', values = c( 'TR_p' = 'black', 'activator_p' = 'green', 'inhibitor_p' = 'red'))+
  ggtitle('I_i - TF on inhibitor 50 - GO control') + 
  ylab( 'Molecule number') + 
  xlab( 'Time')

setwd('TF_i_50')
ggsave(filename = 'I_i_go_50.png', plot = last_plot())
