#Heatmap by year and 5 year age group
library(tidyverse)
###########################################
## Generate synthetic data

# Define years and age groups
years <- 2000:2024
age_groups <- c(paste(seq(0, 80, 5), seq(4, 84, 5), sep = "-"), "85+")

# Create all combinations of age group and year
df <- expand.grid(AgeGroup = age_groups, Year = years)

# Set seed for reproducibility
set.seed(42)

# Age effect: older groups have higher mean
age_effect <- seq(5, 50, length.out = length(age_groups))

# Year effect: increasing trend over time
year_effect <- seq(1, 2, length.out = length(years))

# Expected mean counts (outer product, then flatten)
mean_matrix <- as.vector(outer(age_effect, year_effect))

# Simulate Poisson counts
df$RR <- exp(rnorm(n = length(mean_matrix), 0,1))

df$Year= factor(df$Year)
df$AgeGroup= factor(df$AgeGroup)

df <- df %>%
  mutate(RR_grp = if_else(RR<0.5,1,
                      if_else(RR>=0.5 & RR<1 , 2, 
                        if_else(RR>=1.0 & RR<1.25  ,3 , 
                      
                        if_else(RR>=1.25 & RR<1.5  ,4 , 
                         if_else(RR>=1.5 & RR<1.75  ,5 , 
                          if_else(RR>=1.75 & RR<2.0  ,6 ,
                            if_else(RR>=2.0 & RR<2.5  ,7 ,
                                          
                           if_else(RR>=2.5 & RR<3.0  ,8 , 
                                   if_else(RR>=3.0 & RR<3.5  ,9 , 
                                           
                            if_else(RR>=3.5 & RR<4.0  ,10 ,
                             if_else(RR>=4.0 & RR<6.0  ,11 , 
                                    
                             if_else(RR>=6.0 & RR<8.0  ,12 , 
                               if_else(RR>=8 & RR<10.0  ,13 , 
                                 if_else(RR>=10   , 14, NA_real_
                                 )))))))))))))),
         RR_grp = factor(RR_grp, levels=1:14, labels=c('0.5','1.0','1.25','1.5','1.75',
                                                       '2.0','2.5','3.0','3.5',
                                                       '4.0','6.0','8.0','10.0','>10.0')
                         )
  )
        
head(df, 10)

######################



  ggplot(data = df, 
         aes(x = Year, 
             y =  AgeGroup )) +
    geom_raster(aes(fill = RR_grp), interpolate = F) +
    scale_fill_manual(values = c(">10.0"="#5c0900", "10.0"="#850d00",
                                 "8.0"="#a31000", "6.0"="#c21300",
                                 "4.0"="#eb1800", "3.5"="#ff3e29",
                                 "3.0"="#ff7014", "2.5"="#ff9049",
                                 "2.0"="#ffaf35", "1.75"="#ffd230",
                                 "1.5"="#a3a3cc", "1.25"="#b7b7d7",
                                 "1.0"="#cacae2", "0.5"="#dbdbeb")) +
    xlab("Time") +
    labs(fill = "O/E Ratio") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7, vjust = 1, hjust = 0, angle = 90))
