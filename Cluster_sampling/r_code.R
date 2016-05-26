library(Rcpp)
library(ggplot2)
sourceCpp("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project2\\project2.cpp")


#beta = 0.65  sweep 54; h 0.3033
sweep_0.65 = c()
for(i in 1:100)
  sweep_0.65 = rbind(sweep_0.65, findSweep(0.65, i, 200))
colMeans(sweep_0.65) 

#beta = 0.75  sweep 158; h 0.2535
sweep_0.75 = c()
for(i in 1:500)
  sweep_0.75 = rbind(sweep_0.75, findSweep(0.75, i, 500))
colMeans(sweep_0.75) 

#beta = 0.85  sweep 1740; h 0.1820
sweep_0.85 = c()
for(i in 1:50)
  sweep_0.85 = rbind(sweep_0.85, findSweep(0.85, i))
colMeans(sweep_0.85) 

#epsilon 0.001


###############################################################
check_v1_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v1_65.txt")
ggplot(check_v1_65, aes(V1, V2)) + geom_hline(yintercept = 0.3033, color="blue") + geom_vline(xintercept=7, linetype=2) + 
  geom_line(color="red") + annotate("text", x=7, y=0, label="t=7", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 1, ", beta==0.65))) + theme(plot.title=element_text(size=rel(1.5)))
###
check_v1_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v1_75.txt")
ggplot(check_v1_75, aes(V1, V2)) + geom_hline(yintercept = 0.2535, color="blue") + geom_vline(xintercept=23, linetype=2) + 
  geom_line(color="red") + annotate("text", x=23, y=0, label="t=23", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 1, ", beta==0.75))) + theme(plot.title=element_text(size=rel(1.5)))
###
check_v1_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v1_85.txt")
ggplot(check_v1_85, aes(V1, V2)) + geom_hline(yintercept = 0.1820, color="blue") + geom_vline(xintercept=25, linetype=2) + 
  geom_line(color="red") + annotate("text", x=25, y=0, label="t=25", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 1, ", beta==0.85))) + theme(plot.title=element_text(size=rel(1.5)))
###
check_v2_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v2_65.txt")
ggplot(check_v2_65, aes(V1, V2)) + geom_hline(yintercept = 0.3033, color="blue") + geom_vline(xintercept=8.85, linetype=2) + 
  geom_line(color="red") + annotate("text", x=8.85, y=0, label="t=8.85", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 2, ", beta==0.65))) + theme(plot.title=element_text(size=rel(1.5)))
###
check_v2_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v2_75.txt")
ggplot(check_v2_75, aes(V1, V2)) + geom_hline(yintercept = 0.2535, color="blue") + geom_vline(xintercept=16.17, linetype=2) + 
  geom_line(color="red") + annotate("text", x=16.17, y=0, label="t=16.17", size=8, hjust=-0.5) +  ylim(0, 0.7) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 2, ", beta==0.75))) + theme(plot.title=element_text(size=rel(1.5)))
###
check_v2_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v2_85.txt")
ggplot(check_v2_85, aes(V1, V2)) + geom_hline(yintercept = 0.1820, color="blue") + geom_vline(xintercept=25.54, linetype=2) + 
  geom_line(color="red") + annotate("text", x=25.54, y=0, label="t=25.54", size=8, hjust=-0.5) +  ylim(0, 0.7) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("checkerboard image, version 2, ", beta==0.85))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v1_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v1_65.txt")
ggplot(const_v1_65, aes(V1, V2)) + geom_hline(yintercept = 0.3033, color="blue") + geom_vline(xintercept=6, linetype=2) + 
  geom_line(color="red") + annotate("text", x=6, y=0, label="t=6", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 1, ", beta==0.65))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v1_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v1_75.txt")
ggplot(const_v1_75, aes(V1, V2)) + geom_hline(yintercept = 0.2535, color="blue") + geom_vline(xintercept=12, linetype=2) + 
  geom_line(color="red") + annotate("text", x=12, y=0, label="t=12", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 1, ", beta==0.75))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v1_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v1_85.txt")
ggplot(const_v1_85, aes(V1, V2)) + geom_hline(yintercept = 0.1820, color="blue") + geom_vline(xintercept=32, linetype=2) + 
  geom_line(color="red") + annotate("text", x=32, y=0, label="t=32", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 1, ", beta==0.85))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v2_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v2_65.txt")
ggplot(const_v2_65, aes(V1, V2)) + geom_hline(yintercept = 0.3033, color="blue") + geom_vline(xintercept=5.01, linetype=2) + 
  geom_line(color="red") + annotate("text", x=5.01, y=0, label="t=5.01", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 2, ", beta==0.65))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v2_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v2_75.txt")
ggplot(const_v2_75, aes(V1, V2)) + geom_hline(yintercept = 0.2535, color="blue") + geom_vline(xintercept=9.52, linetype=2) + 
  geom_line(color="red") + annotate("text", x=9.52, y=0, label="t=9.52", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 2, ", beta==0.75))) + theme(plot.title=element_text(size=rel(1.5)))
###
const_v2_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v2_85.txt")
ggplot(const_v2_85, aes(V1, V2)) + geom_hline(yintercept = 0.1820, color="blue") + geom_vline(xintercept=17.12, linetype=2) + 
  geom_line(color="red") + annotate("text", x=17.12, y=0, label="t=17.12", size=8, hjust=-0.5) +  ylim(0, 0.6) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + xlab("t") + ylab("h") +
  ggtitle(expression(paste("constant image, version 2, ", beta==0.85))) + theme(plot.title=element_text(size=rel(1.5)))
###########
size_check_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_check_65.txt")
size_const_65 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_const_65.txt")
size_check_65["label"] = "check"
size_const_65["label"] = "const"
size_check_65["beta"] = "0.65"
size_const_65["beta"] = "0.65"
size_check_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_check_75.txt")
size_const_75 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_const_75.txt")
size_check_75["label"] = "check"
size_const_75["label"] = "const"
size_check_75["beta"] = "0.75"
size_const_75["beta"] = "0.75"
size_check_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_check_85.txt")
size_const_85 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\size_const_85.txt")
size_check_85["label"] = "check"
size_const_85["label"] = "const"
size_check_85["beta"] = "0.85"
size_const_85["beta"] = "0.85"
size = rbind(size_check_65, size_const_65, size_check_75, size_const_75, size_check_85, size_const_85)
ggplot(size, aes(V1, V2, color=beta)) + geom_line(aes(linetype=label), size=rel(1.2)) + xlab("t (sweep)") + ylab("average size") +
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + 
  ggtitle("averge size aganist sweep") + theme(plot.title=element_text(size=rel(1.5))) +
  theme(legend.position=c(1,1), legend.justification=c(1, 1)) + scale_linetype_discrete(labels=c("checkerboard", "constant"))

###########
const_v1_1 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v1_1.txt")
check_v1_1 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v1_1.txt")
const_v1_1["label"] = "MC1";
check_v1_1["label"] = "MC2";
v1_1 = rbind(const_v1_1, check_v1_1)
ggplot(v1_1, aes(V1, V2, color=label)) + geom_vline(xintercept = 52, linetype = 2) + xlab("sweep") + ylab("h") + 
  geom_line(size=rel(1.2)) + annotate("text", x=52, y=0, label="t=52", size=8, hjust=1.1) + xlim(0, 100) +
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + 
  ggtitle(expression(paste("version 1, ", beta==1))) + theme(plot.title=element_text(size=rel(1.5))) +
  theme(legend.position=c(1,1), legend.justification=c(1, 1)) + scale_color_discrete(labels=c("constant", "checkerboard"))

###
const_v2_1 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\const_v2_1.txt")
check_v2_1 = read.table("C:\\Study\\Statistics\\202C Monte Carlo Methods for Optimization\\project3\\data\\check_v2_1.txt")
const_v2_1["label"] = "MC1";
check_v2_1["label"] = "MC2";
v2_1 = rbind(const_v2_1, check_v2_1)
ggplot(v2_1, aes(V1, V2, color=label)) + geom_vline(xintercept = 55, linetype = 2) + xlab("sweep") + ylab("h") + 
  geom_line(size=rel(1.2)) + annotate("text", x=55, y=0, label="t=55", size=8, hjust=1.1) + xlim(0, 100) + 
  theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3))) + 
  ggtitle(expression(paste("version 2, ", beta==1))) + theme(plot.title=element_text(size=rel(1.5))) +
  theme(legend.position=c(1,1), legend.justification=c(1, 1)) + scale_color_discrete(labels=c("constant", "checkerboard"))







