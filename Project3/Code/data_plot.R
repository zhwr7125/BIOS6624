va %<>% mutate(miss_albumin = is.na(albumin),
               my_bmi = (va$weight*0.453592)/(va$height*0.0254)^2,
               bmi_discrep = bmi - my_bmi) 


#### NOTES #### 

# Hospital 30 is missing ALL time 39 height, weight, bmi
# proced = 2 ??
# Bilogically possible range of BMI, 2 above 60, 1 at 2.558
# The bmi at sixmonth 39 seems very off

# Missing tons of Albumin measurements about half from everyone
# from 265-330


#############################
## MISSINGNESS BY HOSPITAL ##
#############################

sum.na <- function(x) sum(is.na(x))

missing.tab <- va %>% 
  group_by(hospcode) %>% 
  summarise_all(.funs = sum.na)

names(missing.tab) <- c(names(missing.tab)[1], 
                        paste(names(missing.tab)[-1], "missing", sep = "_"))

par(mfrow = c(2,4))
for(i in 2:ncol(missing.tab)){
  .f <- paste(names(missing.tab)[i],"~ hospcode")
  
  plot(as.formula(.f), data = missing.tab)
}
par(mfrow = c(1,1))

#########
###BMI###
#########

# 3 weird points
plot(va$bmi)
boxplot(va$bmi)

plot(bmi_discrep ~ hospcode, data = va)

## Hospcods <= 16 are wierd
plot(bmi_discrep ~ hospcode, data = va %>% filter(hospcode < 20))

## Timepoint 39 is all fucky
plot(bmi_discrep ~ sixmonth, data = va)

va %>% filter(hospcode == 30) %>% 
  ggplot(aes(x = as.factor(sixmonth), y = weight)) + 
  geom_boxplot() +
  theme_bw()



#############
###albumin###
#############

va %>% 
  group_by(hospcode) %>% 
  summarise(n = n()) #%>% View


va %>% 
  ggplot(aes(x = sixmonth, y = albumin, group = hospcode)) +
  geom_line() +
  theme_bw() 

va %>% 
  ggplot(aes(x = as.factor(hospcode), y = albumin)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap( ~ sixmonth)

with(va, table(miss_albumin, hospcode))
with(va, table(miss_albumin, sixmonth))
with(va, table(miss_albumin, asa))
with(va, table(miss_albumin, proced))
with(va, table(miss_albumin, death30))


plot(albumin ~ weight, data = va)

va %>% ggplot(aes(x = miss_albumin, y = weight)) + geom_boxplot() + theme_bw() +
  facet_wrap( ~ sixmonth)

va %>% ggplot(aes(x = miss_albumin, y = height)) + geom_boxplot() + theme_bw() +
  facet_wrap( ~ sixmonth)

va %>% ggplot(aes(x = miss_albumin, y = my_bmi)) + geom_boxplot() + theme_bw() +
  facet_wrap( ~ sixmonth)


va %>% ggplot(aes(x = hospcode, y = miss_albumin)) + geom_point() + theme_bw() +
  facet_wrap( ~ sixmonth)