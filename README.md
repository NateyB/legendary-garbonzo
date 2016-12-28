# legendary-garbonzo
CS 7313: Advanced Artificial Intelligence

All of the projects from my Advanced AI class at the University of Tulsa,
wonderfully preserved (except the broken ones) in the hopes that one day they
will fly on their own. Presently, I plan to update them periodically as I
become a better programmer and gain more knowledge (particularly in the
languages that I do not know very well).

## Project 1: MDPs
Project 1 dealt with optimal policies for MDPs (Markov Decision Problems).

### Project 1a: Value & Policy Iteration
Project 1a consisted of approximate algorithms for generating optimal
policies for MDPs, provided the transition model. This project was
completed in Haskell.

Recently, I reworked this project so that the MDP state space could be
n-dimensional, and reformulated the solving utilities and definition so
that, to solve an MDP, the user need only specify the state space, the
state → action mapping, the state → transition → (state, probability)
mapping, and the state → reward mapping—as it should be. Previously,
the user needed to specify many more utilities.

#### Value Iteration
The value iteration algorithm constantly updates the utilities of each
square until the largest change in utility is less than or equal to some
user-specified threshold. Then, a policy is generated from the resultant
utilities.

#### Policy Iteration
The policy iteration algorithm chooses a policy and keeps tweaking it until
it stabilizes. In our textbook, the algorithm said that when the policy didn't
change from one timestep to another, then it had converged. I did not, however,
find this to be true. Frequently, it took many subsequent iterations to
converge. Ultimately, I defined convergence as an unchanging policy after
5000 iterations (a large overshoot).


### Project 1b: Q-Learning
Project 1b consisted of using Tile Encoding and Radial Basis Function
algorithms to approximate the optimal policy for a continuous-space MDP,
with no provided transition model. I tried to implement this many times
in Lua/Lisp before finally giving up and coding it in Java—hence the
mounds of dead code. I hope to debug eventually these broken programs
because I still do not understand why they don't work.

In both cases, a function, theta(x), was learned that used weighted
inuts to determine the optimal policy.

#### Tile Encoding
Tile encoding consists of making a several sets of tiles (known as a tiling)
that each capture the entirety of the continuous input space. Then,
a weighted function, theta(x), based off of the corresponding tiles
(using eligibility traces; think backproopgation) predicts the next action.

#### Radial Basis
The difference between radial basis and tile encoding is that, rather
than tilings which have a set of binary features, radial basis uses some
function (in this case, Gaussian decay) of the distance between the feature
and the point in the input space to generate the values for the
variables in the weighted function, theta(x).


## Project 2: Inference in Bayesian Networks
Project 2 dealt with inference in Bayesian Networks; that is,
given hidden variables, evidence, and a query variable, determine the
probability distribution over the query variable.

### Exact Inference: Enumeration Ask
The Enumration Ask algorithm determined the exact distribution for the
query value by iterating over the entire network, which made it intractable
for large networks.

### Approximate Inference: Likelihood Weighting
Likelihood weighting uses the parents of each node to approximate the
value of the query node, then weight that value according to the
probability of the node given its parents.

### Approximate Inference: Gibbs Sampling (Monte-Carlo Markov Chain implementation)
Gibbs Sampling assigned random values (proportional to probability) to all
of the hidden variables in the network, then would change the value and
see how likely the new value is, given the Markov blanket, repeating this
procedure.

## Project 3: Dynamic Bayesian Networks & Language
Project 3 deals with inference in Dynamic Bayesian Networks and language
analysis.
