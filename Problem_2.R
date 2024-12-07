#-----------#
### Setup ###
#-----------#

### Load packages
library(tidyverse)
library(ggplot2)
library(Rfast)
library(ggtext)
library(dplyr)
library(gt)
library(scales)
library(car)

### Set working directory (change before running code)
setwd("/Users/eli/Desktop/ST425/Project")

### Set seed
set.seed(1205)

#---------------#
### Load data ###
#---------------#

### Load assignment dataset
dat = read.csv("p2_dat.csv")

### Seperate temperature vectors for men and women
men = subset(dat, gender == "Men")$temp
women = subset(dat, gender == "Women")$temp

### Dataset without two outliers
dat_out = dat %>%
  filter(temp < 38) 

men_out = subset(dat_out, gender == "Men")$temp
women_out = subset(dat_out, gender == "Women")$temp

#----------------------------#
### Descriptive statistics ###
#----------------------------#

### Table 1 (descriptive statistics)
all = dat %>%
  summarise(
    Group = "All",
    Mean = mean(temp, na.rm = TRUE),
    Variance = var(temp, na.rm = TRUE),
    Max = max(temp),
    Min = min(temp),
    n = n()) 

gender = dat %>%
  rename(Group = gender) %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(temp, na.rm = TRUE),
    Variance = var(temp, na.rm = TRUE),
    Max = max(temp),
    Min = min(temp),
    n = n())    

rbind(all, gender)

### Figure 1a (histogram for entire dataset)
dat %>%
  ggplot(aes(x=temp)) +
  geom_histogram(binwidth=0.05,
                 colour = "black",
                 fill = "#31CAA8",
                 lwd = 0.5,
                 linetype = 1,
                 position = "identity") + 
  theme_classic() +
  xlab("Temperature (°C)") + ylab("Count") +
  scale_x_continuous(limits = c(35.5, 38.5),
                     breaks = c(35.5, 36, 36.5, 37, 37.5, 38, 38.5)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) 

### Figure 1b (histogram for men and women)
dat_mean <- data.frame(
  label = c("Mean: 36.9", "Mean: 36.6"),
  gender = c("Men", "Women"),
  x     = c(37.5, 37.5),
  y     = c(8, 8))

dat_var <- data.frame(
  label = c("Variance: 0.25", "Variance: 0.10"),
  gender = c("Men", "Women"),
  x     = c(37.5, 37.5),
  y     = c(7, 7))

dat %>%
  ggplot(aes(x=temp, fill = gender)) +
  facet_grid(rows = vars(gender)) + 
  geom_histogram(binwidth=0.05,
                 colour = "black",
                 lwd = 0.5,
                 linetype = 1,
                 position = "dodge") + 
  theme_classic() +
  scale_fill_manual(values=c("#ff412c", "#9F29FF")) +
  xlab("Temperature (°C)") + ylab("Count") +
  scale_x_continuous(limits = c(35.5, 38.5),
                     breaks = c(35.5, 36, 36.5, 37, 37.5, 38, 38.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 14))

### Descriptive statistics without outliers
dat_out %>%
  group_by(gender) %>%
  summarise(mean = mean(temp),
            median = summary(temp)[3],
            var = var(temp))

#------------#
### T-test ###
#------------#

### Testing nornality for entire sample
shapiro.test(dat$temp)

### Testing nornality for men and women seperately
shapiro.test(men)
shapiro.test(women)

### Testing equal variance
leveneTest(temp ~ gender, data = dat)

### T-test equal variance
t_equal = t.test(men, women, 
                 alternative = "two.sided", 
                 var.equal = TRUE)

### T-test unequal variance
t_unequal = t.test(men, women, 
                   alternative = "two.sided", 
                   var.equal = FALSE)

### T-test no outliers
t_out = t.test(men_out, women_out,
               alternative = "two.sided", 
               var.equal = TRUE)

### Results (table 2)
t_unequal$statistic; t_unequal$p.value; t_unequal$conf.int

t_equal$statistic; t_equal$p.value; t_equal$conf.int

t_out$statistic; t_out$p.value; t_out$conf.int

#------------------------#
### Randomization test ###
#------------------------#

### Lengths of vectors for randomization
len_m = length(men)
len_w = length(women)
len = len_m + len_w
m1 = len_m + 1

### Values for permutation loop
B = 10000
t = abs(mean(men) - mean(women))
z = c(men, women)
k = 0
t_sim = c()

### Permutation loop
for(i in 1:B){
  
  zp = sample(z, len)
  
  t_perm = abs(mean(zp[1:len_m]) - mean(zp[m1:len])) # permuting sample
  
  if(t_perm > t) k = k + 1 # checking condition
  
  t_sim[i] = t_perm # saving test statistic
  
}

### P-value
cat("p-value", k/B, "\n")

### Figure 2 (permutation test results)
data.frame(t_sim) %>%
  ggplot(aes(x = t_sim)) +
  geom_histogram(bins = 15,
                 colour = "black",
                 lwd = 0.5,
                 linetype = 1,
                 fill = "#ff412c",
                 position = "dodge") +
  theme_classic() + 
  geom_vline(aes(xintercept = t),
             linetype = "dashed") +
  xlab("Absoulte Difference in Means") + ylab("Count") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(labels = label_comma()) +
  annotate("text", x = 0.44, y = 2000, label = "Observed Difference: 0.35", size = 2.5, color = "black") +
  annotate("text", x = 0.44, y = 1900, label = "P = 0.0036", size = 2.5, color = "black")

### Test without outliers
len_m = length(men_out)
len_w = length(women_out)
len = len_m + len_w
m1 = len_m + 1

B = 10000
t = abs(mean(men_out) - mean(women_out))
z = c(men_out, women_out)
k = 0
t_sim = c()

for(i in 1:B){
  
  zp = sample(z, len)
  
  t_perm = abs(mean(zp[1:len_m]) - mean(zp[m1:len])) # permuting sample
  
  if(t_perm > t) k = k + 1 # checking condition
  
  t_sim[i] = t_perm # saving test statistic
  
}

### P-value
cat("p-value", k/B, "\n")

#--------------------#
### Bootstrap test ###
#--------------------#

### Hypothesis test with package
boot_package = boot.ttest2(men, 
                           women, 
                           B = 10000)

### Test results
boot_package

### Manual bootstrap
diff = abs(mean(men) - mean(women))
B=10000
mean_diff = numeric(B)

for(i in 1:B){
  
  m_tmp = mean(sample(men,replace=T))
  w_tmp = mean(sample(women,replace=T))
  
  mean_diff[i]= abs(m_tmp - w_tmp)
  
}

### Figure 3 (Bootstrap results)
data.frame(mean_diff) %>%
  ggplot(aes(x = mean_diff)) +
  geom_histogram(bins = 25,
                 colour = "black",
                 lwd = 0.5,
                 linetype = 1,
                 fill = "#9F29FF",
                 position = "dodge") +
  theme_classic() +
  xlab("Absoulte Difference in Means") + ylab("Count") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(labels = label_comma(),
                     breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750)) +
  geom_vline(aes(xintercept = median(mean_diff)),
             linetype = "dashed") +
  annotate("rect", 
           xmin = quantile(mean_diff, p = 0.025), 
           xmax = quantile(mean_diff, p = 0.975), 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "grey50", alpha = 0.2) +
  annotate("text", x = 0.77, y = 1300, label = "Median Difference: 0.34", size = 2.5, color = "black") +
  annotate("text", x = 0.77, y = 1250, label = "P = 0.01829817", size = 2.5, color = "black") +
  ggsave("Slides/fig4.png") 


### Test with no outliers
 boot.ttest2(men_out,
             women_out,
             B = 10000)


