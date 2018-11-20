filepath <- "C:/Users/angel/OneDrive/Projects/Pokemon"

pokedex <- read.table(paste(filepath,"pokedex.txt",sep="/"), sep="\t", check.names = FALSE)
pokedex$Name <- as.character(pokedex$Name)
pokedex$`Special Evolution` <- as.factor(pokedex$`Special Evolution`)
pokedex$Legendary <- as.factor(pokedex$Legendary)
pokedex$Generation <- as.factor(pokedex$Generation)

types <- read.table(paste(filepath,"types.txt",sep="/"), sep="\t", check.names = FALSE)
gamegen <- read.table(paste(filepath,"gamegen.txt",sep="/"), sep="\t", check.names = FALSE)


# Exploratory plots
library(tidyverse)

# Total by game

# Total by generation
ggplot(pokedex, aes(x = Generation, y = Total, col = Generation)) + 
  geom_jitter(alpha = 0.5, width = 0.3)

# Total types by generation
ggplot(pokedex, aes(x = Generation)) +
  geom_bar(aes(fill = Type1), position = "fill")

# Counts of types by generation
test <- pokedex %>%
  group_by(Generation, Type1) %>%
  summarize(count = n())
ggplot(test, aes(x = Generation, y = count, group = Type1)) + geom_line() + facet_wrap(~Type1)

test <- pokedex %>%
  filter(!is.na(Type2)) %>%
  group_by(Generation, Type2) %>%
  summarize(count = n())
ggplot(test, aes(x = Generation, y = count, group = Type2)) + geom_line() + facet_wrap(~Type2)

# Total by game and type
type_by_gen <- as.data.frame(table(pokedex$Type1, by = pokedex$Generation))
ggplot(type_by_gen, aes(by, Freq, group = Var1, fill = Var1)) + geom_area(position = "fill")
type_by_gen <- as.data.frame(table(pokedex$Type2, by = pokedex$Generation))
ggplot(type_by_gen, aes(by, Freq, group = Var1, fill = Var1)) + geom_area(position = "fill")


# Score based method to find which pokemon is best all around
# Based on the ranking of how well the pokemon would do against 
# each other pokemon using only base stats
# Do this by game
# And by generation
# for damage calculation refer to https://bulbapedia.bulbagarden.net/wiki/Damage#Damage_calculation

# Best pokemon from Red
red_pokedex <- pokedex[pokedex$Red == 1,1:13]

# most damage against bulbasaur is from Mewtwo according to below methodology
simple_damage <- function(attack=NULL, defense=NULL, Type=NULL){
  damage = (attack/defense)*Type
  damage
}
type_fun <- function(attck_type1=NULL, attck_type2=NULL, 
                     def_type1=NULL, def_type2=NULL, types_matrix){
  attacker_types <- c(which(attck_type1 == colnames(types_matrix)),
                      which(attck_type2 == colnames(types_matrix)))
  defender_types <- c(def_type1,def_type2)
  types_adjust_vec <- NULL
  for (i in attacker_types){
    for (j in defender_types){
      type_adjust <- types_matrix[i,j]
      types_adjust_vec <- rbind(types_adjust_vec, type_adjust)
    }
  }
  types_adjust_vec
}
adjusted_damage <- function(attack_pokemon=NULL, defend_pokemon=NULL, pokedex=NULL,
                            types_matrix=NULL){
  poknum1 <- which(pokedex$Name == attack_pokemon)
  poknum2 <- which(pokedex$Name == defend_pokemon)
  attck_type1 <- as.character(pokedex$Type1[poknum1])
  attck_type2 <- as.character(pokedex$Type2[poknum1])
  def_type1 <- as.character(pokedex$Type1[poknum2])
  def_type2 <- as.character(pokedex$Type2[poknum2])
  type_multiplier <- type_fun(attck_type1,attck_type2,def_type1,def_type2,types_matrix)
  physical_damage <- simple_damage(pokedex$Attack[poknum1],pokedex$Defense[poknum2],type_multiplier)
  sp_damage <- simple_damage(pokedex$`Sp. Atk`[poknum1],pokedex$`Sp. Def`[poknum2],type_multiplier)
  adj_damage <- mean(c(physical_damage, sp_damage))
  adj_damage
}

# rows are attackers
# columns are defenders
red_adj_damage_matrix <- sapply(red_pokedex$Name, function(x){
  sapply(red_pokedex$Name,function(y){adjusted_damage(x,y,red_pokedex,types)})})

all_best_fun <- function(damage_matrix=NULL, pokedex=NULL){
  rank_matrix <- NULL
  for (i in 1:ncol(damage_matrix)){
    rank_row <- t(nrow(damage_matrix) + 1 - order(damage_matrix[i,]))
    rank_matrix <- rbind(rank_matrix, rank_row)
  }
  rank_vector <- t(sapply(1:ncol(rank_matrix), function(x){
    sum(rank_matrix[,x])
  }))
  best_pokemon <- pokedex$Name[which.min(rank_vector)]
  best_pokemon
}

highest_score_fun <- function(damage_matrix=NULL, pokedex=NULL){
  colsum_vec <- colSums(damage_matrix)
  strong_pokemon <- pokedex$Name[which.max(colsum_vec)]
  strong_pokemon
}


# See mostly what kind of pokemon it is:
# Physical sweeper: attack + speed
# Special sweeper: sp. attack + speed
# Wall: HP + defense + sp. defense
# Physical tank: attack + defense
# Special tank: sp. attack + sp. defense


# See which starter pokemon is best based on gym battles