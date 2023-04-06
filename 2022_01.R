# Author: Francis Abreu
# Date: 24 March 2023

# Part 1:
# The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of Calories each Elf is carrying (your puzzle input). The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line. In case the Elves get hungry and need extra snacks, they need to know which Elf to ask: they'd like to know how many Calories are being carried by the Elf carrying the most Calories. Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

# Part 2:
# By the time you calculate the answer to the Elves' question, they've already realized that the Elf carrying the most Calories of food might eventually run out of snacks. To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups. Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?

# Libraries and functions

library(tidyverse)
library(tidyr)


# Read data 
data_2022_01 <- read.table(file = "./data/2022_01_input.txt", blank.lines.skip = FALSE)


df = data_2022_01 %>%
	mutate(label_na = ifelse(!is.na(V1),0,1),
		  elf = cumsum(label_na)+1
	) %>%
	filter(!is.na(V1)) %>%
	group_by(elf) %>%
	summarise(sum_cals = sum(V1)) 

results_part1 = filter(df,sum_cals == max(sum_cals))
results_part1

df %>%
	arrange(desc(sum_cals)) %>%
	filter(row_number()<=3) %>%
	summarise(sum_top3 = sum(sum_cals))