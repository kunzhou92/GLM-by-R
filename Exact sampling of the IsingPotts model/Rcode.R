library(Rcpp)
library(ggplot2)
sourceCpp("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project2\\project2.cpp")
beta_0.5 = ChainState(0.5)
beta_0.65 = ChainState(0.65)
beta_0.75 = ChainState(0.75)
beta_0.83 = ChainState(0.83)
beta_0.84 = ChainState(0.84)
beta_0.85 = ChainState(0.85)
beta_0.9 = ChainState(0.9)
beta_1 = ChainState(1)

#beta_0.5
min_0.5 = min(which(beta_0.5$X1 == beta_0.5$X2))
A = subset(beta_0.5[1:30,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.5[1:30,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.5 = rbind(A, B)
ggplot(gg_0.5, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.5, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.5)) + 
  annotate("text", x=min_0.5, y=0, label=paste("step=", min_0.5), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())

#beta_0.65
min_0.65 = min(which(beta_0.65$X1 == beta_0.65$X2))
A = subset(beta_0.65[1:80,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.65[1:80,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.65 = rbind(A, B)
ggplot(gg_0.65, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.65, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.65)) +
  annotate("text", x=min_0.65, y=0, label=paste("step=", min_0.65), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())


#beta_0.75
min_0.75 = min(which(beta_0.75$X1 == beta_0.75$X2))
A = subset(beta_0.75[1:220,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.75[1:220,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.75 = rbind(A, B)
ggplot(gg_0.75, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.75, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.75)) +
  annotate("text", x=min_0.75, y=0, label=paste("step=", min_0.75), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())


#beta_0.83
min_0.83 = min(which(beta_0.83$X1 == beta_0.83$X2))
A = subset(beta_0.83[1:2000,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.83[1:2000,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.83 = rbind(A, B)
ggplot(gg_0.83, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.83, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.83)) +
  annotate("text", x=min_0.83, y=0, label=paste("step=", min_0.83), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())


#beta_0.84
min_0.84 = min(which(beta_0.84$X1 == beta_0.84$X2))
A = subset(beta_0.84[1:4000,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.84[1:4000,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.84 = rbind(A, B)
ggplot(gg_0.84, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.84, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.84)) +
  annotate("text", x=min_0.84, y=0, label=paste("step=", min_0.84), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())


#beta_0.85
min_0.85 = min(which(beta_0.85$X1 == beta_0.85$X2))
A = subset(beta_0.85[1:6000,], select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.85[1:6000,], select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.85 = rbind(A, B)
ggplot(gg_0.85, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_vline(xintercept=min_0.85, linetype="dashed", color="black", alpha=.5) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.85)) +
  annotate("text", x=min_0.85, y=0, label=paste("step=", min_0.85), hjust=-.5, size=10) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())



#beta_0.9
A = subset(beta_0.9, select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_0.9, select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_0.9 = rbind(A, B)
ggplot(gg_0.9, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==0.9)) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,0.6), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())


#beta_1
A = subset(beta_1, select=c(step, X1))
colnames(A)[2] = "sum"
A["label"] = "upper bound"
B = subset(beta_1, select=c(step, X2))
colnames(B)[2] = "sum"
B["label"] = "lower bound"
gg_1 = rbind(A, B)
ggplot(gg_1, aes(x=step, y=sum, color=label, linetype=label)) + 
  geom_line(size=rel(1)) + 
  ggtitle(expression(beta==1)) +
  xlab("Step") + ylab("Sum of\nSites") + theme(axis.title.y=element_text(angle=0)) +
  theme(legend.position=c(1,.6), legend.justification=c(1,1), legend.background=element_blank(), legend.key=element_blank())

data = BetaTao(seq(0.5, 0.89, by=0.01))

ggplot(data, aes(beta, tao)) + geom_line() + ylab(expression(tau)) + xlab(expression(beta)) +
  theme(axis.text = element_text(size=rel(1.5))) + 
  theme(axis.title=element_text(size=rel(1.5))) + theme(axis.title.y=element_text(angle=0)) 

ggplot(data, aes(beta, tao)) + geom_line() + ylab(expression(log(tau))) + xlab(expression(beta)) +
  theme(axis.text = element_text(size=rel(1.5))) + 
  theme(axis.title=element_text(size=rel(1.5))) + theme(axis.title.y=element_text(angle=0)) + scale_y_log10()
  

