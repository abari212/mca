###########################################################
# Reinforcement Learning - RL
# Reference: Nicolas Pröllochs & Stefan Feuerriegel (2017)
#######################

# Reinforcement Learning - RL
# Under RL algorithm, the machine is exposed to an environment (env_sa) where it trains itself   based on using trial and error.. RL learns by taking actions (a) under continuous changing conditions or states (s). It is trained thus to learn from past experience and tries to capture the best possible knowledge to make accurate decisions. 

# Example of RL algorithms is Markov Decision Process (MDP). There is a package for applying MDP. Other algorithms and packages are also under development such as  the “ReinforcementLearning” package, which is intended to partially close this gap and offers the ability to perform model-free reinforcement learning in a highly customizable framework .

# Installation or RL related platforms 
# Using the devtools package, one can easily install the latest development version of ReinforcementLearning as follows.
# install.packages("devtools")
# Option 1: download and install latest version from GitHub
# devtools::install_github("nproellochs/ReinforcementLearning")

# Package loading
library(ReinforcementLearning)
# Usage
# The following sections present the usage and main functionality of the ReinforcementLearning package.

# Data format
# The ReinforcementLearning package uses experience replay to learn an optimal policy based on past experience in the form of sample sequences consisting of states, actions and rewards. Here each training example consists of a state transition tuple (s,a,r,s_new), as described below.

# s The current environment state.
# a The selected action in the current state.
# r The immediate reward received after transitioning from the current state to the next state.
# s_new The next environment state.

# The input data must be a dataframe in which each row represents a state transition tuple (s,a,r,s_new).
# Experience sampling using an environment function
# The ReinforcementLearning package consist of built-in RL capabilities such as sampling experience from a function that defines the dynamics of the environment. 
# These dynamics of the environment may be known in advance (a priori), in this case arbitrary complex environment function can be created in R and sample state transition tuples. 
This function can thus be manually implemented with defined state (s) and an action (a) as input. The output or return value needs also to contain the name of the next state and the reward (r). 
This procedure of experience sampling allows to validate the performance of RL, by applying the learned policy (pi) to newly generated dataset.

environment <- function(state, action) {
  ...
  return(list("NextState" = newState,
              "Reward" = reward))
}
# To illustrate this process using an environment function, below an agent that navigates from a random starting position clock-wise to a goal position on a simulated 2x2 grid.
# The illustration is a grid with 4 states where the  process moves from s1 to S4 clock-wise: 

# |----|----|
# | s4 | s1 |
# |----|----|
# | s3 | s2 |
# |----|----|

# Each cell on the grid represents one state, which yields a total of 4 states. It is assumed that the agent cannot move off the grid. In addition, the agent faces an obstruction and can’t move directly from s1 to s4. 
At each state, the agent randomly chooses one out of four possible actions, i. e. to move up, down, left, or right. 
To consider the obstruction the agent encounters the following reward structure. When crossing each square on the grid leads to a reward of -1. However, if the agent reaches the goal position, it will earn a reward of 10.

# To create the above environment for this envisaged grid with obstruction: 

environment_sa <- function(state, action) {
  ...
  return(list("NextState" = newState,
              "Reward" = reward))
}

# Define state and action sets
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")

# Create environment function of state-action (envt_sa)

envt_sa <- function (state, action) 
{
    next_state <- state
    if (state == state("s1") && action == "down") 
        next_state <- state("s2")
    if (state == state("s2") && action == "left") 
        next_state <- state("s3")
    if (state == state("s3") && action == "up") 
        next_state <- state("s4")
    if (next_state == state("s4") && state != state("s4")) {
        reward <- 10
    }
    else {
        reward <- -1
    }
    out <- list(NextState = next_state, Reward = reward)
    return(out)
}
print(envt_sa)

# The reward value or function determines the extent of how difficult the problem is.
# A reward can also be specified for a state such as in this case  
# R(s4)=10 (above) 
# with R(s1),R(s2) and R(s3)= -1
# install.packages('data.table'), if required
library(data.table)
# Sample N = 1000 random sequences from the environment
sa_data <- sampleExperience(N = 1000, env = envt_sa, states = states, actions = actions)
head(sa_data)

# Performing reinforcement learning
# The following example shows how to teach a reinforcement learning agent using input data(s,a), in the form of sample sequences consisting of states, actions and rewards. The ‘data’ argument must be a dataframe object (sa_data) in which each row represents a state transition tuple (s,a,r,s_new). 
# It is required to specify the column names of the individual tuple elements in ‘sa_data’.

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(sa_data, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", control = control)

# Print result
print(model)
# The result of the learning process is a state-action table and an optimal policy that defines the best possible action in each state.

# Print policy
policy(model)

# Updating an existing policy
# Specifying an environment function to model the dynamics of the environment helps to validate the performance of the agent. The validation procedure involves as per previous models to apply the learned policy to newly generated dataset. 

The ReinforcementLearning package has additional predefined action selection mode, namely ‘epsilon-greedy’ to carry out the validation where the agent explores the environment by selecting an action at random with probability.

# The following example shows how to sample new experience from an existing policy using ‘epsilon-greedy’ action selection. The result is new state transition samples (‘data_new’) with significantly higher rewards compared to the original sample (‘sa_data’).
# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Sample N = 1000 sequences from the environment using epsilon-greedy action selection
sa_data_new <- sampleExperience(N = 1000, env = envt_sa, states = states, actions = actions, 
                             model = model, actionSelection = "epsilon-greedy", 
                             control = control)
head(sa_data_new)
# Update the existing policy using new training data
model_new <- ReinforcementLearning(sa_data_new, s = "State", a = "Action", r = "Reward", 
                                   s_new = "NextState", control = control, model = model)

# Print result
print(model_new)
summary(model_new)
# Plot reinforcement learning curve
plot(model_new)
# Print optimal policy
policy(model)
names(model)
  
# Reference to Nicolas Pröllochs & Stefan Feuerriegel (2017)

#######################

# Markov Decision Processes in R
# Load R library MDPtoolbox
library(MDPtoolbox)
# Create transition matrix for two states and two actions
T <- array(0, c(2, 2, 2))
T[,,1] <- matrix(c(0, 1, 0.8, 0.2), nrow=2, ncol=2, byrow=TRUE)
T[,,2] <- matrix(c(0.5, 0.5, 0.1, 0.9), nrow=2, ncol=2, byrow=TRUE)
# ? Dimensions are #states × #states × #actions
# Create reward matrix (of dimensions #states × #actions)
R <- matrix(c(10, 10, 1, -5), nrow=2, ncol=2, byrow=TRUE)
#  Check whether the given T and R represent a well-defined MDP
mdp_check(T, R)

# Policy Iteration
# Learning an agent traveling through a 2×2 grid (i. e. 4 states)
# s3 (Goal) s0, s1,  s2
# Wall (red line) prevents direct moves from s0 to s3
# Reward favors shorter routes
# Visiting each square/state gives a reward of -1
# Reaching the goal gives a reward of 10
# Actions: move left, right, up or down
# Transition probabilities are < 1
# ? i. e. allows erroneous moves

# Design an MDP that finds the optimal policy to that problem
# Create individual matrices with pre-specified (random) transition
# probabilities for each action
up <- matrix(c( 1, 0, 0, 0,
0.7, 0.2, 0.1, 0,
0, 0.1, 0.2, 0.7,
0, 0, 0, 1),
nrow=4, ncol=4, byrow=TRUE)
left <- matrix(c(0.9, 0.1, 0, 0,
0.1, 0.9, 0, 0,
0, 0.7, 0.2, 0.1,
0, 0, 0.1, 0.9),
nrow=4, ncol=4, byrow=TRUE)


# Second chunk of matrices
down <- matrix(c(0.3, 0.7, 0, 0,
0, 0.9, 0.1, 0,
0, 0.1, 0.9, 0,
0, 0, 0.7, 0.3),
nrow=4, ncol=4, byrow=TRUE)
right <- matrix(c(0.9, 0.1, 0, 0,
0.1, 0.2, 0.7, 0,
0, 0, 0.9, 0.1,
0, 0, 0.1, 0.9),
nrow=4, ncol=4, byrow=TRUE)

# Aggregate previous matrices to create transition probabilities in T
T <- list(up=up, left=left, down=down, right=right)

# Create matrix with rewards
R <- matrix(c(-1, -1, -1, -1,
-1, -1, -1, -1,
-1, -1, -1, -1,
10, 10, 10, 10),
nrow=4, ncol=4, byrow=TRUE)
# Check if this provides a well-defined MDP
mdp_check(T, R) # empty string => ok

# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE)
R <- matrix(c(5, 10, -1, 2), 2, 2, byrow=TRUE)
# Not run
# mdp_Q_learning(P, R, 0.9)
# With a sparse matrix
P <- list()
P[[1]] <- Matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE, sparse=TRUE)
# Not run
# mdp_Q_learning(P, R, 0.9)

##########"
set.seed(0)
mdp_example_rand(2, 2)
mdp_example_rand(2, 2, FALSE)
mdp_example_rand(2, 2, TRUE)
mdp_example_rand(2, 2, FALSE, matrix(c(1,0,1,1),2,2))
# Generates a MDP for a simple forest management problem
MDP <- mdp_example_forest()
# Find an optimal policy
results <- mdp_policy_iteration(MDP$P, MDP$R, 0.9)
# Visualise the policy
results$policy 
# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE)
R <- matrix(c(5, 10, -1, 2), 2, 2, byrow=TRUE)
mdp_bellman_operator(P, R, 0.9, c(0,0))
# With a sparse matrix
P <- list()
P[[1]] <- Matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE, sparse=TRUE)
mdp_bellman_operator(P, R, 0.9, c(0,0))

# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE)
R <- matrix(c(5, 10, -1, 2), 2, 2, byrow=TRUE)
mdp_check(P, R)
# With a sparse matrix
P <- list()
P[[1]] <- Matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE, sparse=TRUE)
mdp_check(P, R) 

M <- matrix(c(0.6116, 0.3884, 0, 1.0000), 2, 2, byrow=TRUE)
mdp_check_square_stochastic(M)


# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.6116, 0.3884, 0, 1.0000), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0.6674, 0.3326, 0, 1.0000), 2, 2, byrow=TRUE)
R <- array(0, c(2,2,2))
R[,,1] <- matrix(c(-0.2433, 0.7073, 0, 0.1871), 2, 2, byrow=TRUE)
R[,,2] <- matrix(c(-0.0069, 0.6433, 0, 0.2898), 2, 2, byrow=TRUE)
policy <- c(2,2)
mdp_computePpolicyPRpolicy(P, R, policy)
# With a sparse matrix (P)
P <- list()
P[[1]] <- Matrix(c(0.6116, 0.3884, 0, 1.0000), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0.6674, 0.3326, 0, 1.0000), 2, 2, byrow=TRUE, sparse=TRUE)
mdp_computePpolicyPRpolicy(P, R, policy)


# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.6116, 0.3884, 0, 1.0000), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0.6674, 0.3326, 0, 1.0000), 2, 2, byrow=TRUE)
R <- array(0, c(2,2,2))
R[,,1] <- matrix(c(-0.2433, 0.7073, 0, 0.1871), 2, 2, byrow=TRUE)
R[,,2] <- matrix(c(-0.0069, 0.6433, 0, 0.2898), 2, 2, byrow=TRUE)
mdp_computePR(P, R)
# With a sparse matrix (P)
P <- list()
P[[1]] <- Matrix(c(0.6116, 0.3884, 0, 1.0000), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0.6674, 0.3326, 0, 1.0000), 2, 2, byrow=TRUE, sparse=TRUE)
mdp_computePR(P, R)


# With a non-sparse matrix
P <- array(0, c(2,2,2))
P[,,1] <- matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE)
P[,,2] <- matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE)
R <- matrix(c(5, 10, -1, 2), 2, 2, byrow=TRUE)


policy <- c(2,1)
mdp_eval_policy_iterative(P, R, 0.8, policy)
# With a sparse matrix
P <- list()
P[[1]] <- Matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow=TRUE, sparse=TRUE)
P[[2]] <- Matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow=TRUE, sparse=TRUE)
mdp_eval_policy_iterative(P, R, 0.8, policy)


