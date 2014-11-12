library(bnlearn)
library(lattice)

# Randomly generating several possible BNs?
# random.graph(LETTERS[1:6], num = 2, method = "ic-dag")
# ...Where `method` specifies how the space of possible graphs is sampled

data(gaussian.test)
pdag = iamb(gaussian.test) # Partially directed acyclic graph

# One edge is undirected; this is not allowed for a BN.
# Set the topological ordering of the nodes
dag = pdag2dag(pdag, ordering = c("A", "B", "C", "D", "E", "F" ,"G"))

# We can use MLE to fit parameters of the local probability distributions
fit = bn.fit(dag, gaussian.test)