# code from https://ourcodingclub.github.io/tutorials/mixed-models/

load("/Users/andrewenfield/work/CC-Linear-mixed-models/dragons.RData")
head(dragons)

hist(dragons$testScore)

# standardize explanatory vars (mean zero and std of 1), so all the 
# coefs are on the same scale and effect sizes can be compared apples-to-apples
dragons$bodyLengthStand <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

basic.lm <- lm(testScore ~ bodyLengthStand, data = dragons)
summary(basic.lm)

library(tidyverse)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore))
  + 
    geom_point() +
    geom_smooth(method = "lm"))

# residual line isn't flat
plot(basic.lm, which = 1)

# points at ends of qq plot aren't on the line
plot(basic.lm, which = 2)

# obs from each mtn range are _not_ independent
boxplot(testScore ~ mountainRange, data = dragons)
(color_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, color = mountainRange)) 
  +
    geom_point(size = 2) +
    theme_classic() + 
    theme(legend.position = "none"))

# one option would be to do a separate regression for each mtn range
(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons)
  +
    geom_point() + 
    facet_wrap(~ mountainRange) + 
    xlab("length") + 
    ylab("test score"))

# but we also have sites within range and they're not independent either
# we have three sites for each of eight ranges, so we'd have to fit 24 
# models, each with only 20 observations; in addition, this many models 
# increases the risk of overall type I errors (falsely rejecting the null) because
# of the multiple comparison problem

# so, first cut is just to add in independent variables for mtn range (and
# also site, but I'll just do mtn range here to keep things simple)
mountain.lm <- lm(testScore ~ bodyLengthStand + mountainRange, data = dragons)
summary(mountain.lm)

# the result above shows that bodyLength isn't significant and also that we're
# estimating the difference in test scores for each mountain range. however, we
# don't care about the effect of the mountain range - "we just want to know whether
# body length affects test scores and we simply want to _control for the variation_
# coming from mountain ranges" - so, 'control' but not 'quantify'? then it says
# that controlling is something we can do with "random factors" and if we add in 
# random factors to what we already/normally have - "fixed" factors" - then we 
# get a "mixed effect model"

# there's a lot of good text explanation in the tutorial around this code

# one thing I'll note down here to remember is that ultimately w/ the mixed effect
# model we're doing below, we're trying to "explain part of the variation in test
# score through fitting body length as a fixed effect" but that also since "the
# response variable has some residual variation (i.e., unexplained variation)
# associated with mountain ranges" we also want to use random effects to 

library(lme4)
