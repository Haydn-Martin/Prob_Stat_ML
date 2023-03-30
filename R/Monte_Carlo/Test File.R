input_var <- 1
est_var <- -10
output <- 1

acc <- 0.01

error <- 1



func.test <-  function(est_var, input_var){exp(est_var + 0.5*input_var^2)}

test <- func.test(est_var, input_var)

while (abs(error) > acc) {
  
  est_var <- est_var + 0.1
  test <- func.test(est_var, input_var)
  
  error <- output - test
  
}  

est_var
