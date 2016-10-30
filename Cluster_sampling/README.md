# STATS 202C Project 3
## Cluster sampling of the Ising/Potts model

**X** is the image (or state) defined on the lattice and the variable **X**s at each site s takes value in {0, +1}. The distribution of X is given. The convergence rate to static distribtuion from initial distribution was studied. A detailed description of question is in file **Project3_Cluster_sampling.pdf**. 

C++ code is available in **/Cluster_sampling/ConsoleApplication1/ConsoleApplication1** for simulation. THe final report is conducted by R code in file **r_code.R**. Report is at **/report/project3.pdf**. 

- Ran two Markov chains using Cluster sampling based on three different parameters of distribution and two versions of algorithm. 
- Plotted a sufficient statistics. 
- Marked the convergence time t1, t2, t3 (sweeps). 
- Plotted the average sizes of the CPs (number of pixels that are flipped together at each step).
