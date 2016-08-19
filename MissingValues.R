titanic <- read.csv("titanic_original.csv", stringsAsFactors = FALSE)
dim(titanic)
head(titanic)
tail(titanic)

titanic <- titanic %>% 
  mutate(embarked = replace(embarked, embarked == '' & name != '', "S"))

titanic <- titanic %>% 
  mutate(age = replace(age, is.na(age), mean(age[!is.na(age)])))

titanic <- titanic %>% 
  mutate(boat = replace(boat, boat == "", NA))

titanic <- titanic %>% 
  mutate(has_cabin_number = as.numeric(cabin != ""))

write.csv(titanic,"titanic_clean.csv")
