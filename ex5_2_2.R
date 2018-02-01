# exercise 5.2.2

# get regression data generated in exercise 5.2.1
source(file.path("Scripts", "ex5_2_1.R"))
dev.off() # close the figure from exercise 5.2.1

# Estimate model parameters
w_est = lm(y ~ X);

# Plot the predictions of the model
plot(X, y, main='Linear regression', xlab="X", ylab="y");
y_est = w_est$coef[1] +w_est$coef[2]*X;
lines(X, y_est, col='red');
y_true = w0+w1*X;
lines(X, y_true, col='green');
legend("topleft", legend=c("Data", "Fitted model", "True model"), fill=c("black", "red", "green"))
