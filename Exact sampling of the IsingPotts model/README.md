# STATS 202C Project2
## Exact sampling of the Ising/Potts model with coupled Markov Chains

X is the image (or state) defined on the lattice and the variable Xs at each site s takes value in {0, +1}. The distribution of X is given. The convergence rate to static distribtuion from initial distribution was studied. A detailed description of question is in file **Project3_Cluster_sampling.pdf**. At each step, the Gibbs sampler is applied. 'coalesce', which means two chains will stay in the same state forever as they are driven by the same random number at each step, is studied. A detailed description of question is in file **Project2_Exact_sampling.pdf**.

C++ code is available in file **project2.cpp**. It is integrated into R file **project2.R** by Rcpp. The final report is in file **project2.pdf**.

- Proved that 𝑋_s^1≥𝑋_s^2, ∀𝑠 in any time. X_s^1 and 𝑋_s^2 are states of two Mrkov Chains specified in **Project2_Exact_sampling.pdf**.
- Plotted the two chain states over the sweeps with distribution of different parameters.
- Plotted the curve of coalesce time versus parameters.
