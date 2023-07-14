This is the code for running a Spectral Meta Learner. The SML combines multiple predictions targeted at the same continuous outcome and merges them together, weighting more accurate predictions higher and ignoring bad predictions. This can be performed in 2 ways with very slightly different results, with SVD or the eigenvalues of the covariance matrix between the predictors. A plotting function is also included
