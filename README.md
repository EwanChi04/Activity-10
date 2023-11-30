# Activity-10
---
title: "First RMD File"
author: "Ewan G. Chi"
date: "2023-11-01"
output: pdf_document
---

### loading packages for the code in this project
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, dpi = 300)
library(groundhog)
groundhog.library("ggplot2", "2023-11-01")
groundhog.library("dplyr", "2023-11-01")
groundhog.library("kableExtra", "2023-11-01")
groundhog.library("knitr", "2023-11-01")
```

## Collatz Conjecture

The Collatz Conjecture is where if the number is even, divide by two, and if the number is odd, multiply the number by three and add 1, then repeat that process until the number is one. By calculating the stop coutner of the first 10,000 digits (stop counter meaning how many changes it takes a number until it becomes one), we see that the data has a median of about 50 and is skewed with a tail to the right.

### this code runs the Collatz Conjecture and makes a visualization to show it
```{r CollatzConjecture}
collatzConjecture <- function(num, counter = 0) {
  if(num == 1)
    return(counter)
  else if(num %% 2 == 0)
    return(collatzConjecture(num / 2, counter + 1))
  else if(num %% 2 == 1)
    return(collatzConjecture(3 * num + 1, counter + 1))
}
outputs <- sapply(seq(1, 10000), collatzConjecture)
hist(outputs)
```

## Diamonds

From the visualization below, we can see how the clarity and carat of a diamond effects the price of it. There is a decent positive correlation between the carat and clarity to the cost of the diamond.

### this code makes a visualization for the diamonds data set
```{r Diamonds}
ggplot(diamonds) +
  aes(x = carat, y = price, colour = clarity) +
  geom_point(shape = "circle") +
  scale_color_hue(direction = 1) +
  labs(
    x = "Carat",
    y = "Prices",
    title = "Diamonds: Carat vs. Prices",
    color = "clarity"
  ) +
  theme_classic()
diamondsZstats <- diamonds %>%
  group_by (cut) %>%
  select(cut, price) %>%
  summarize(
    across(
      .cols = where(is.numeric),
      .fns = list(
        min = ~min(price, na.rm = TRUE),
        Q1 = ~quantile(price, probs = 0.25, na.rm = TRUE),
        median = ~median(price, na.rm = TRUE),
        Q3 = ~quantile(price, probs = 0.75, na.rm = TRUE),
        max = ~max(price, na.rm = TRUE),
        sam = ~mean(price, na.rm = TRUE),
        sasd = ~sd(price, na.rm = TRUE)
      )
    ),
    count = n()
  )
diamondsZstats %>%
  kable() %>%
  kableExtra::kable_classic()
```

Although there are still plenty of things I struggle with in this class, I have learned a lot. Everything I am able to do now, I have learned. I never took a statistics class or a coding class before and never did my own research on the topics, so this class has been very eye opening. I enjoy coding. Although it is difficult because I am not familiar with the language, it's like a puzzle. Once I figure it out I'm very proud of myself. I've learned a lot of how functions work and how to code things and the R language.

\newpage
# Code Appendix
```{r codeAppendix}
#| ref.label = knitr::all_labels(),
#| echo = TRUE,
#| eval = FALSE
```
