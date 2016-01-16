### Find sensitivity, specificity, accuracy and etc...
total <- 98901 + 999 + 99 + 1
tp <- 99
fp <- 999
fn <- 1
tn <- 98901

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
accuracy <- (tp + tn) / (tp + tn + fp + fn)
ppv <- tp / (tp + fp)
npv <- tn / (tn + fn)
