# Bayesian Networks

In Bayesian networks, either all of the nodes must be continuous variables or all must be discrete variables; continuous and discrete variables cannot be mixed.
This is to facilitate calculating the joint probability distribution which is only possible given certain assumptions about the network's nodes.

## Parameter Learning

When the variables that make up the nodes take on continuous values, the probability distribution of each node is a normal distribution and the resulting network that is formed is termed a Gaussian Bayesian network.
In the case of continuous variables, the parameters are just regression coefficients.
Because the nodes of a Bayesian network are linked, multivariate regression is performed to predict the distribution arising at each node in the network, providing regression coefficients for each pairwise interaction between a node and its  connections.
Parameter learning is generally done through a maximum likelihood approach (whereby the best fit parameters are estimated) or a Bayesian approach (whereby the posterior distribution of the parameters for a discrete distribution is estimated).
