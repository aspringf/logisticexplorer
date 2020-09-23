# Logistic regression results explorer

This is a shiny app that displays the results
of a logistic regression fit in R, conditional
on values of the covariates.

Users can upload a RDS file containing the glm
fit object, as well as a file that contains the
mean and standard deviation of the covariates in
the original data. The standardization information
is then used to display results on the original
scale in case covariate transformations were 
applied prior to fitting the regression.

By default, covariate values are set to their
means or the reference value in case of factors.


