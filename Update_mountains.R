library(fastDummies)

mountains2 <- dummy_cols(mountains, select_columns = "Education_Level")
mountains2 <- dummy_cols(mountains2, select_columns = "Preferred_Activities")

set.seed(123)
         
mountains2 <- mountains2 |>
  mutate(customer = 0.25 +
           Age * 0.0005 +
                    Income * 0.000004 +
           Education_Level_bachelor * 0.08 +
           Education_Level_master * 0.18 + 
  Education_Level_doctorate * 0.24 +
    Preferred_Activities_hiking * 0.4 + 
    Environmental_Concerns * 0.24 +
    rnorm(n(), -0.75, 0.75))
    

table(mountains2$customer)
sd(mountains2$customer)

mountains2 <- mountains2 |>
  mutate(customer2 = if_else(customer >= 0.75, 1, 0))

table(mountains2$customer2)

mountains$buyer <- mountains2$customer2

log_model <- glm(formula = buyer ~ Age + Income + Preferred_Activities + Environmental_Concerns + Education_Level,
                 data = mountains, 
                 family = binomial) 

summary(log_model)

write.csv(mountains,"C:/Users/go56rez/OneDrive - TUM/Desktop/Teaching/SDP/WS_2024_2025/mountains.csv", row.names = FALSE)
