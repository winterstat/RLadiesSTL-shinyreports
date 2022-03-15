# Shiny App: Fixed Objects

# Characteristic names
char_names <- c("Bill length" = 'bill_length_mm',
                   "Bill depth" = 'bill_depth_mm',
                   "Flipper length" = 'flipper_length_mm',
                   "Body mass" = 'body_mass_g')

# Grouping variable names
group_names <- c("Island" = 'island', "Species" = 'species',
                 "Sex" = 'sex')
# Quiz answer key
key <- list("q1" = 'Biscoe', "q2" = 168, "q3" = 'Gentoo')

# Quiz questions
q1_text <- "1. On what island were most penguins observed?" 
q2_text <- "2. How many male penguins were observed?"
q3_text <- "3. What penguin species has the highest average body mass?"  

# Report quiz fixed response components
answer_p1 <- "Your response was "
answer_p2 <- ", which is "
answer_p3 <- "."
answer_p4 <- " The correct answer is "