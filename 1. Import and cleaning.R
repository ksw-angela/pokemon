######################## INPUT ########################
#               CHANGE TO YOUR FILE PATH              #

filepath <- "C:/Users/angel/OneDrive/Projects/Pokemon"

######################## INPUT ########################





########################################################
##                                                    ##
##                                                    ##
##                   Importing data                   ##
##                                                    ##
##                                                    ##
########################################################

library(rvest)

# Main stats - Initial data frame
main_url <- "https://pokemondb.net/pokedex/all"
main_table <- html_nodes(read_html(main_url), "table")
main_pokedex <- html_table(main_table)[[1]]

# Legendary information
legend_names <- c("Articuno","Zapdos","Moltres","Mew","Mewtwo",
                  "Raikou","Entei","Suicune","Ho-oh","Lugia","Celebi",
                  "Regice","Registeel","Regirock","Latias","Latios","Groudon","Kyogre","Rayquaza","Jirachi","Deoxys",
                  "Uxie","Mesprit","Azelf","Dialga","Palkia","Heatran","Regigigas","Giratina","Cresselia","Phione","Manaphy","Darkrai","Shaymin","Arceus",
                  "Victini","Cobalion","Terrakion","Virizion","Tornadus","Thundurus","Reshiram","Zekrom","Landorus","Kyurem","Keldeo","Meloetta","Genesect",
                  "Xerneas","Yveltal","Zygarde","Diancie","Hoopa","Volcanion",
                  "Cosmog","Cosmoem","Solgaleo","Lunala","Necrozma","Magearna","Marshadow")

# Generation information
gen_url <- "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number"
gen_table <- html_nodes(read_html(gen_url), "table")
gen_pokedex <- html_table(gen_table, fill=TRUE)[2:8]

# Availability information
game_url <- "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_availability"
game_table <- html_nodes(read_html(game_url), "table")
availability <- html_table(game_table,fill=TRUE)[2:8]
game_names <- c("Red","English Blue","Japanese Blue","Yellow",
                "Gold","Silver","Crystal",
                "Ruby","Sapphire","FireRed","LeafGreen","Emerald","Colosseum","XD",
                "Diamond","Pearl","Platinum","HeartGold","SoulSilver","Pokewalker",
                "Black","White","Black 2","White 2","Dream World",
                "X","Y","Omega Ruby","Alpha Sapphire","Friend Safari",
                "Sun","Moon")

# Mega evolution information
mega_url <- "https://bulbapedia.bulbagarden.net/wiki/Mega_Evolution"
mega_table <- html_nodes(read_html(mega_url), "table")
mega <- html_table(mega_table, fill=TRUE)[1:2]

# Effectiveness of Types Chart
type_url <- "https://pokemondb.net/type"
type_table <- html_nodes(read_html(type_url), "table")
type_chart <- html_table(type_table,fill=TRUE)[[1]]





########################################################
##                                                    ##
##                                                    ##
##                   Cleaning data                    ##
##                                                    ##
##                                                    ##
########################################################

library(tidyverse)

# Cleaning main stats #
# 1. Separate Type into two columns
# 2. Create separate column for special evolutions (908 - 802 = 106 pokemon)

type_new <- paste(main_pokedex$Type, " ") %>%
  gsub("([a-z])([A-Z])", "\\1\\U \\2", ., perl = TRUE) %>%
  strsplit(" ")

main_pokedex$Type1 <- map_chr(type_new, 1) %>% factor(ordered = FALSE)
main_pokedex$Type2 <- map_chr(type_new, 2) %>% factor(ordered = FALSE)
main_pokedex$Type2[main_pokedex$Type2 == ""] <- NA
main_pokedex$Type <- NULL
rm(type_new)

special <- duplicated(main_pokedex$`#`)
main_pokedex$`Special Evolution` <- 0
main_pokedex$`Special Evolution`[special == 1] <- 1
main_pokedex$`Special Evolution` <- factor(main_pokedex$`Special Evolution`)
rm(special)



# Add flag for legendary status #

legendary_index <- unlist(sapply(legend_names, function(x) grep(x, main_pokedex$Name)))
main_pokedex$Legendary <- 0
main_pokedex$Legendary[legendary_index] <- 1
main_pokedex$Legendary <- factor(main_pokedex$Legendary)
rm(legendary_index)



# Cleaning generation information #
# 1. Bind rows of all data frames from gen_pokedex with argument Generation - there
#    some duplicated numbers... let's take those out.
# 2. Include the generation column to the main data frame

generation <- bind_rows(gen_pokedex, .id = "Generation") %>% 
  select(Generation, Ndex) %>%
  mutate(`#` = as.integer(gsub("#", "", Ndex))) %>%
  select(`#`, Generation)
generation[duplicated(generation$`#`),] <- NA

main_pokedex <- semi_join(main_pokedex, generation, by = "#")

rm(generation)



# Cleaning availability information #
# 1. Rename each of the columns to refer to game titles
# 2. Join all data frames in availability
# 3. Only include flags that indicate they are obtainable in game
# 4. Merge final availability data frame with main pokedex by Ndex

in_game_flag <- c("C","S","D","R","E","B","EV")

rename_cols <- function(df, game_list = game_names){
  colnames(df)[4:ncol(df)] <- tail(game_names, ncol(df)-3)
  df <- tail(df, nrow(df) - 1)
}

is_in_game <- function(df, in_game_vec = in_game_flag){
  df %in% in_game_vec
}

pokemon_by_game <- map(availability, ~rename_cols(., game_names)) %>%
  bind_rows() %>%
  select(-`#`, -Name, -Icon) %>%
  map_df(~is_in_game(., in_game_flag)) %>%
  mutate(`#` = 1:nrow(.))

main_pokedex <- left_join(main_pokedex, pokemon_by_game, by = "#")

rm(pokemon_by_game)



# Cleaning mega evolution information #
# 1. First data frame from mega is for which pokemon are found in X and Y (26)
#    Second data frame is for which pokemon are found in Omega Ruby and Alpha Sapphire (20)
# 2. If names are in the above and they are of special evolution status, make them only
#    available in those respective games
# 3. Pokemon with "Alolan" in the name are from Sun and Moon
x_y <- mega[[1]]$Pokémon[-1] %>% unique()
or_as <- mega[[2]]$Pokémon[-1] %>% unique()

x_y_index <- unlist(sapply(x_y, 
                           function(x) grep(paste(x,"Mega",sep=""), main_pokedex$Name)))
or_as_index <- unlist(sapply(or_as, 
                             function(x) grep(paste(x,"Mega",sep=""), main_pokedex$Name)))

main_pokedex[x_y_index,15:39] <- FALSE
main_pokedex[or_as_index,15:41] <- FALSE

alo_index <- grep("Alolan", main_pokedex$Name)
main_pokedex[alo_index,15:44] <- FALSE



# Cleaning effectiveness of type chart #
# 1. Rename columns and remove the first column
# 2. "" or NAs -> 1;  "1/2" -> 0.5
# 3. Change values to numeric

colnames(type_chart) <- c("",type_chart[,1])
type_chart <- type_chart[-1]

type_chart[type_chart == "½"] <- 0.5
type_chart[type_chart == "" | is.na(type_chart)] <- 1
type_chart <- walk(type_chart, as.numeric)





########################################################
##                                                    ##
##                                                    ##
##                    Saving data                     ##
##                                                    ##
##                                                    ##
########################################################

# Main pokedex #
# `#`: Ndex
# Name: Name of pokemon
# Total: sum of all stats (from HP to Speed) - general guide of how strong pokemon is
# HP: hit points, or health, defines how much damage a pokemon can withstand before fainting 
# Attack: the base modifier for normal attacks (eg. Scratch, Punch)
# Defense: the base damage resistance against normal attacks
# Sp. Atk: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)
# Sp. Def: the base damage resistance against special attacks
# Speed: determines which pokemon attacks first each round
# Type1: Each pokemon has a type, this determines weakness/resistance to attacks
# Type2: Some pokemon are dual type and have 2
# Special Evolution: Boolean whether it's a special evolution (e.g. Mega)
# Legendary: Boolean
# Generation: Factor from 1 to 7
# Rest are Boolean if they can be caught in the game
write.table(main_pokedex, paste(filepath, "pokedex.txt", sep="/"), sep="\t")

# Effectiveness of types #
# Weakness or resistance to attacks by type
write.table(type_chart, paste(filepath, "types.txt", sep="/"), sep="\t")