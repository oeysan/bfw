require(truncnorm)

# Reward = Food and Dance = Yes/No
food <- data.frame(
  Reward = rep("Food",380) ,
  Dance = c( rep("Yes",280) ,
             rep("No",100) )
)

# Reward = Affection and Dance = Yes/No
affection <- data.frame(
  Reward = rep("Affection",1620) ,
  Dance = c( rep("Yes",480) ,
             rep("No",1140) )
)

# Combine Rewards and Dance into data frame
data <- rbind(food,affection)
# Create frequency table
freq.table <- table(data)

# Create alignement data
set.seed(666)
food.dance.alignment <- stats::rbinom( freq.table["Food", "Yes"] , 1 , 0.3 ) # 30% of the cats were Good
food.no.dance.alignment <- stats::rbinom( freq.table["Food", "No"] , 1 , 0.7 ) # 70% of the cats were Good
aff.dance.alignment <- stats::rbinom( freq.table["Affection", "Yes"] , 1 , 0.9 ) # 90% of the cats were Good
aff.no.dance.alignment <- stats::rbinom( freq.table["Affection", "No"] , 1 , 0.1 ) # 10% of the cats were Good

# Create data frame for alignment
alignment <- data.frame(Alignment = c(food.dance.alignment,
                                      food.no.dance.alignment,
                                      aff.dance.alignment,
                                      aff.no.dance.alignment))

# Convert binary to character
alignment[alignment == 1] <- "Good"
alignment[alignment == 0] <- "Evil"
# Convert to factor
alignment$Alignment <- as.factor(alignment$Alignment)

# Combine Rewards, Dance and Alignment into data frame
data <- cbind(data,alignment)
freq.table <- data.frame(table(data))

# order data frames
freq.table <- freq.table[order(freq.table$Alignment ,
                               freq.table$Dance,
                               freq.table$Reward),]
data <- data[order(data$Alignment ,
                   data$Dance ,
                   data$Reward),]

# Create ratings data (Cats rate their owners)
## Average of several Likert-type items, measured on a scale from 1 (Hate) to 7 (Love)
set.seed(777)

# Evil cats that don't dance for food
food.no.evil <- truncnorm::rtruncnorm( freq.table[1,"Freq"] , a=1, b=7, mean = 5, sd = 1)
# Evil cats that don't dance for affection
affection.no.evil <- truncnorm::rtruncnorm( freq.table[2,"Freq"] , a=1, b=7, mean = 1, sd = 1)
# Evil cats that dance for food
food.yes.evil <- truncnorm::rtruncnorm( freq.table[3,"Freq"] , a=1, b=7, mean = 5, sd = 1)
# Evil cats that dance for affection
affection.yes.evil <- truncnorm::rtruncnorm( freq.table[4,"Freq"] , a=1, b=7, mean = 1, sd = 1)

# Good cats that don't dance for food
food.no.good <- truncnorm::rtruncnorm( freq.table[5,"Freq"] , a=1, b=7, mean = 4, sd = 1)
# Good cats that don't dance for affection
affection.no.good <- truncnorm::rtruncnorm( freq.table[6,"Freq"] , a=1, b=7, mean = 6, sd = 1)
# Good cats that dance for food
food.yes.good <- truncnorm::rtruncnorm( freq.table[7,"Freq"] , a=1, b=7, mean = 4, sd = 1)
# Good cats that dance for affection
affection.yes.good <- truncnorm::rtruncnorm( freq.table[8,"Freq"] , a=1, b=7, mean = 6, sd = 1)

# Create vector for Ratings
Ratings <- c(food.no.evil,
             affection.no.evil,
             food.yes.evil,
             affection.yes.evil,
             food.no.good,
             affection.no.good,
             food.yes.good,
             affection.yes.good
)

# Combine Reward, Dance, Alignmenet and Ratings
Cats <- cbind( data  , Ratings )

devtools::use_data(Cats)
