setwd('/home/juank/Documents/Doctorado/Articulo-iSgL/R')
library(ggplot2)
library(SGL)
load('colitis.RData')

set.seed(0)

training.ind = sample(1:nrow(X), 80)
train.data <- list(x = X[training.ind,], y = y[training.ind])
test.data <- list(x = X[-training.ind,], y = y[-training.ind])

validation.idx.1 = sample(which(train.data$y==T), floor(0.4*sum(train.data$y==T)) )
validation.idx.0 = sample(which(train.data$y==F), floor(0.4*sum(train.data$y==F)) )
validation.idx = c(validation.idx.0, validation.idx.1)
validate.data = list(x = train.data$x[validation.idx,], y = train.data$y[validation.idx])

train.data = list( x = train.data$x[-validation.idx,], y = train.data$y[-validation.idx] )

# sgl ===============================================================================
fitSGL <- SGL(train.data, membership.index, type = "logit", verbose = TRUE, nlam = 100, 
              alpha = 0.05, standardize = T) #0.01?

validate.pred.SGL <- matrix(NA, ncol = length(fitSGL$lambdas), nrow = length(validate.data$y))
for(i in 1:length(fitSGL$lambdas)){
  validate.pred.SGL[,i] <- predictSGL(fitSGL,validate.data$x,i)
}

correct.class.SGL <- (validate.pred.SGL > 0.5) * validate.data$y + (validate.pred.SGL < 0.5)* (1-validate.data$y)
c.sgl <- apply(correct.class.SGL,2,mean)
nonzero.sgl  = apply(fitSGL$beta!=0, 2, sum)

# lasso ===========================================================================
fitL <- SGL(train.data, membership.index, type = "logit", verbose = TRUE, nlam = 100, 
              alpha = 1, standardize = T) #0.01?

validate.pred.L <- matrix(NA, ncol = length(fitL$lambdas), nrow = length(validate.data$y))
for(i in 1:length(fitL$lambdas)){
  validate.pred.L[,i] <- predictSGL(fitL,validate.data$x,i)
}

correct.class.L <- (validate.pred.L > 0.5) * validate.data$y + (validate.pred.L < 0.5)* (1-validate.data$y)
c.l <- apply(correct.class.L,2,mean)
nonzero.l  = apply(fitL$beta!=0, 2, sum)

# gl ===============================================================================
fitGL <- SGL(train.data, membership.index, type = "logit", verbose = TRUE, nlam = 100, 
              alpha = 0, standardize = T) #0.01?

validate.pred.GL <- matrix(NA, ncol = length(fitGL$lambdas), nrow = length(validate.data$y))
for(i in 1:length(fitGL$lambdas)){
  validate.pred.GL[,i] <- predictSGL(fitGL,validate.data$x,i)
}

correct.class.GL <- (validate.pred.GL > 0.5) * validate.data$y + (validate.pred.GL < 0.5)* (1-validate.data$y)
c.gl <- apply(correct.class.GL,2,mean)
nonzero.gl  = apply(fitGL$beta!=0, 2, sum)

# isgl =============================================================================
isgl = sglfast::isgl_simple(data.train = train.data,
                            data.validate = validate.data,
                            index = membership.index, type = "logit", standardize = T)
validate.pred.isgl = sglfast::predict.isgl(isgl, validate.data$x)
correct.class.isgl = (validate.pred.isgl > 0.5) * validate.data$y + (validate.pred.isgl < 0.5)* (1-validate.data$y)
c.isgl<- apply(correct.class.isgl,2,mean)
nonzero.isgl = sum(isgl$beta!=0)

# plot ======================================================
c.class <- c(c.gl,c.l,c.sgl, c.isgl)
Method <- c(rep("GL",100),rep("Lasso", 100),rep("SGL",100), rep("iSgL",1))
nonzero <- c(nonzero.gl, nonzero.l, nonzero.sgl, nonzero.isgl)

cutoff <- data.frame(yintercept=c.isgl)

dd <- data.frame(Method = Method, x = nonzero, y = c.class)
p1 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method, size=Method))+
  scale_y_continuous("CCR (Validation sample)") + scale_x_continuous(expression(num.variables)) + 
  scale_linetype_manual(values=c("twodash","longdash","dotted","solid")) + 
  scale_color_manual(values=c("red", "darkgreen", "blue", "orange"))+
  scale_size_manual(values = c(1,3,1,1)) +
  geom_hline(aes(yintercept=yintercept, linetype="iSgL", color = "iSgL"), data=cutoff, show.legend = F)


# sgl =====================================================
test.pred.SGL <- matrix(NA, ncol = length(fitSGL$lambdas), nrow = length(test.data$y))
for(i in 1:length(fitSGL$lambdas)){
  test.pred.SGL[,i] <- predictSGL(fitSGL,test.data$x,i)
}

correct.class.SGL <- (test.pred.SGL > 0.5) * test.data$y + (test.pred.SGL < 0.5)* (1-test.data$y)
c.sgl <- apply(correct.class.SGL,2,mean)
nonzero.sgl  = apply(fitSGL$beta!=0, 2, sum)
# lasso ===================================================
test.pred.L <- matrix(NA, ncol = length(fitL$lambdas), nrow = length(test.data$y))
for(i in 1:length(fitL$lambdas)){
  test.pred.L[,i] <- predictSGL(fitL,test.data$x,i)
}

correct.class.L <- (test.pred.L > 0.5) * test.data$y + (test.pred.L < 0.5)* (1-test.data$y)
c.l <- apply(correct.class.L,2,mean)
nonzero.l  = apply(fitL$beta!=0, 2, sum)
# gl ======================================================
test.pred.GL <- matrix(NA, ncol = length(fitGL$lambdas), nrow = length(test.data$y))
for(i in 1:length(fitGL$lambdas)){
  test.pred.GL[,i] <- predictSGL(fitGL,test.data$x,i)
}

correct.class.GL <- (test.pred.GL > 0.5) * test.data$y + (test.pred.GL < 0.5)* (1-test.data$y)
c.gl <- apply(correct.class.GL,2,mean)
nonzero.gl  = apply(fitGL$beta!=0, 2, sum)
# isgl ====================================================
test.pred.isgl = sglfast::predict.isgl(isgl, test.data$x)
correct.class.isgl = (test.pred.isgl > 0.5) * test.data$y + (test.pred.isgl < 0.5)* (1-test.data$y)
c.isgl<- apply(correct.class.isgl,2,mean)
nonzero.isgl = sum(isgl$beta!=0)

# plot ======================================================
c.class <- c(c.gl,c.l,c.sgl, c.isgl)
Method <- c(rep("GL",100),rep("Lasso", 100),rep("SGL",100), rep("iSgL",1))
nonzero <- c(nonzero.gl, nonzero.l, nonzero.sgl, nonzero.isgl)

cutoff <- data.frame(yintercept=c.isgl)

dd <- data.frame(Method = Method, x = nonzero, y = c.class)
p2 = ggplot(data = dd, aes(x = x, y = y, group = Method, shape = Method)) + geom_line(aes(linetype=Method,col=Method)) +
  geom_point(aes(col=Method, size=Method))+
  scale_y_continuous("CCR (Test sample)") + scale_x_continuous(expression(num.variables)) + 
  scale_linetype_manual(values=c("twodash","longdash","dotted","solid")) + 
  scale_color_manual(values=c("red", "darkgreen", "blue", "orange"))+
  scale_size_manual(values = c(1,3,1,1)) +
  geom_hline(aes(yintercept=yintercept, linetype="iSgL", color = "iSgL"), data=cutoff, show.legend = F)


#save.image(file = 'section4-colitis3.RData')

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

multiplot(p1, p2, cols = 1)

