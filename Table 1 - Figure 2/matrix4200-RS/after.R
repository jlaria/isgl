# Process the results
setwd('~/simulations/script-3node/')

load('summary.RData')

nrun = 30
expert_num_groups = 200
pj = 21

for(i in 1:3){
  latex.tab = paste0('&\\multicolumn{4}{c}{$',i,'$ generating groups ($',i*5,'$ non-zero coefficients)} \\\\ \n',
                     '& $\\#\\lambda$ & \\EV & \\ET &  \\comptime(sec.)\\\\ \\midrule\n',
                     '\\iSgL & 2 & ',round(mean(results.table$isgl.validation_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$isgl.validation_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun), 2), ')& ',
                     round(mean(results.table$isgl.test_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$isgl.test_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')& ',
                     round(mean(results.table$isgl.time[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$isgl.time[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')\\\\ \n',
                     '\\iSgL &',expert_num_groups+2,' & ',
                     round(mean(results.table$uisgl.validation_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$uisgl.validation_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun), 2), ')& ',
                     round(mean(results.table$uisgl.test_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$uisgl.test_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')& ',
                     round(mean(results.table$uisgl.time[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$uisgl.time[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')\\\\ \n',
                     'GD &',2,' & ',
                     round(mean(results.table$hc0.validation_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc0.validation_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun), 2), ')& ',
                     round(mean(results.table$hc0.test_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc0.test_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')& ',
                     round(mean(results.table$hc0.time[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc0.time[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')\\\\ \n',
                     'GD &', expert_num_groups + 1,' & ',
                     round(mean(results.table$hc.validation_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc.validation_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun), 2), ')& ',
                     round(mean(results.table$hc.test_error[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc.test_error[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')& ',
                     round(mean(results.table$hc.time[((i-1)*nrun+1):(i*nrun)]),2),
                     ' (',round(sd(results.table$hc.time[((i-1)*nrun+1):(i*nrun)])/sqrt(nrun),2), ')\\\\ \n',
                     '\\cmidrule{2-5}\n')
  cat(latex.tab)
}

library(ggplot2)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



Method = rep(c('iSGL2','iSGL', 'GD2', 'GD'), each = nrun)
dd <- data.frame(Method = Method, x = rep(1:nrun, 4), y = c(results.table$isgl.test_error[31:60], 
                                                            results.table$uisgl.test_error[31:60],
                                                            results.table$hc0.test_error[31:60],
                                                            results.table$hc.test_error[31:60]) )

p1 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method))+
  scale_y_continuous("Test Error") + scale_x_continuous('Simulation number') + 
  scale_linetype_manual(values=c("twodash","dotted","longdash","solid")) + 
  scale_color_manual(values=c("red", "darkgreen", "blue", "orange"))

Method = rep(c('iSGL2','iSGL', 'GD2', 'GD'), each = nrun)
dd <- data.frame(Method = Method, x = rep(1:nrun, 4), y = c(results.table$isgl.validation_error[31:60], 
                                                            results.table$uisgl.validation_error[31:60],
                                                            results.table$hc0.validation_error[31:60],
                                                            results.table$hc.validation_error[31:60]) )

p2 =ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method))+
  scale_y_continuous("Validation Error") + scale_x_continuous('Simulation number') + 
  scale_linetype_manual(values=c("twodash","dotted","longdash","solid")) + 
  scale_color_manual(values=c("red", "darkgreen", "blue", "orange"))

multiplot(p2, p1, cols = 1)
