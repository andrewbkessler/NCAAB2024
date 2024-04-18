# Load the packages we installed
library(tidyverse)
library(hoopR)
library(plyr)
library(dplyr)
library(gt)
library(gtExtras)

### Load March Madness Schedule
mm_schedule <- load_mbb_schedule(seasons = 2024) |>
  filter(tournament_id == "22") |>
  mutate(event_name = paste(away_display_name, home_display_name, sep="_"))
### Load Team Info
team_info <- espn_mbb_teams(year = 2024)
### Load Championship Odds (pulled via API 3/21/2024)
championship_odds <- read_csv("Championship Odds.csv") |>
  filter(bookmaker == "FanDuel")
### Load Milestone Odds (Championship + manually pulled from FD 3/21/2024)
milestone_odds <- read_csv("Milestone Odds.csv") |>
  left_join(select(championship_odds, label, price), by = c('team' = 'label'))
milestone_odds$team <- str_replace_all(milestone_odds$team, c('Mississippi St Bulldogs' = 'Mississippi State Bulldogs',
                                                                'Long Beach St 49ers' = 'Long Beach State Beach',
                                                                'Michigan St Spartans' = 'Michigan State Spartans',
                                                                'Grand Canyon Antelopes' = 'Grand Canyon Lopes',
                                                                'South Dakota St Jackrabbits' = 'South Dakota State Jackrabbits',
                                                                'Colorado St Rams' = 'Colorado State Rams',
                                                                'Morehead St Eagles' = 'Morehead State Eagles',
                                                                'San Diego St Aztecs' = 'San Diego State Aztecs',
                                                                'Washington St Cougars' = 'Washington State Cougars',
                                                                'Iowa State Cyclones' = 'Iowa State Cyclones',
                                                                'Grambling St Tigers' = 'Grambling Tigers'))

### Pull odds listed as of today, manually manipulate problem names, and join with schedule to filter on MM games
today_odds <- read_csv("CBB Odds - Sheet1.csv") |>
  filter(bookmaker == "FanDuel")
today_odds$home_team <- str_replace_all(today_odds$home_team, c('Mississippi St Bulldogs' = 'Mississippi State Bulldogs',
                                                                'Long Beach St 49ers' = 'Long Beach State Beach',
                                                                'Michigan St Spartans' = 'Michigan State Spartans',
                                                                'Grand Canyon Antelopes' = 'Grand Canyon Lopes',
                                                                'South Dakota St Jackrabbits' = 'South Dakota State Jackrabbits',
                                                                'Colorado St Rams' = 'Colorado State Rams',
                                                                'Morehead St Eagles' = 'Morehead State Eagles',
                                                                'San Diego St Aztecs' = 'San Diego State Aztecs',
                                                                'Washington St Cougars' = 'Washington State Cougars',
                                                                'Iowa State Cyclones' = 'Iowa State Cyclones',
                                                                'Grambling St Tigers' = 'Grambling Tigers'))
today_odds$away_team <- str_replace_all(today_odds$away_team, c('Mississippi St Bulldogs' = 'Mississippi State Bulldogs',
                                                                'Long Beach St 49ers' = 'Long Beach State Beach',
                                                                'Michigan St Spartans' = 'Michigan State Spartans',
                                                                'Grand Canyon Antelopes' = 'Grand Canyon Lopes',
                                                                'South Dakota St Jackrabbits' = 'South Dakota State Jackrabbits',
                                                                'Colorado St Rams' = 'Colorado State Rams',
                                                                'Morehead St Eagles' = 'Morehead State Eagles',
                                                                'San Diego St Aztecs' = 'San Diego State Aztecs',
                                                                'Washington St Cougars' = 'Washington State Cougars',
                                                                'Iowa State Cyclones' = 'Iowa State Cyclones',
                                                                'Grambling St Tigers' = 'Grambling Tigers'))
today_odds <- today_odds |>
  mutate(event_name = paste(away_team, home_team, sep="_")) |>
  left_join(select(mm_schedule, event_name, id), by='event_name') |>
  filter(!is.na(id))

### Archive all odds and remove duplicates
archive_odds <- read.csv("Odds Archive.csv", header = TRUE)
archive_odds <- archive_odds |>
  rbind.fill(archive_odds, today_odds)
archive_odds <- distinct(archive_odds, game_id, bookmaker, .keep_all = TRUE)
write.csv(archive_odds, file = "Odds Archive.csv", row.names = FALSE)

### Split schedules into home/away, combine, only include known matchups
### May need to modify "winner" to binary
away_schedule <- mm_schedule |>
  select(id, date, notes_headline, event_name, away_display_name, away_winner) |>
  left_join(select(archive_odds, event_name, odd_2), by='event_name') |>
  dplyr::rename(team = away_display_name, win = away_winner, odds = odd_2)
home_schedule <- mm_schedule |>
  select(id, date, notes_headline, event_name, home_display_name, home_winner) |>
  left_join(select(archive_odds, event_name, odd_1), by='event_name') |>
  dplyr::rename(team = home_display_name, win = home_winner, odds = odd_1)
expanded_schedule <- rbind.fill(away_schedule, home_schedule) |>
  mutate(round = str_sub(notes_headline, start = 33, end = length(notes_headline))) #|>
rm(away_schedule, home_schedule)

### Split out schedule by round, pulling wagers from previous winnings
### First Round
first_round <- expanded_schedule |>
  filter(grepl('1st Round', round)) |>
  mutate(wager = 10,
         payout = ifelse(odds < 0, as.numeric(win)*(wager/(-1*(odds/100))+wager),
                         ifelse(odds > 0, as.numeric(win)*((wager*odds)/100+wager),"")))
### Second Round
second_round <- expanded_schedule |>
  filter(grepl('2nd Round', round)) |>
  left_join(select(first_round, team, payout), by='team') |>
  filter(team != "TBD") |>
  dplyr::rename(wager = payout) |>
  mutate(payout = ifelse(odds < 0, as.numeric(win)*(as.numeric(wager)/(-1*(odds/100))+as.numeric(wager)),
                         ifelse(odds > 0, as.numeric(win)*((as.numeric(wager)*odds)/100+as.numeric(wager)),"")))
# ### Sweet Sixteen
sweet_sixteen <- expanded_schedule |>
  filter(grepl('Sweet 16', round)) |>
  left_join(select(second_round, team, payout), by='team') |>
  dplyr::rename(wager = payout) |>
  mutate(payout = ifelse(odds < 0, as.numeric(win)*(as.numeric(wager)/(-1*(odds/100))+as.numeric(wager)),
                         ifelse(odds > 0, as.numeric(win)*((as.numeric(wager)*odds)/100+as.numeric(wager)),"")))
### Elite Eight
elite_eight <- expanded_schedule |>
  filter(grepl('Elite 8', round)) |>
  left_join(select(sweet_sixteen, team, payout), by='team') |>
  dplyr::rename(wager = payout) |>
  mutate(payout = ifelse(odds < 0, as.numeric(win)*(as.numeric(wager)/(-1*(odds/100))+as.numeric(wager)),
                         ifelse(odds > 0, as.numeric(win)*((as.numeric(wager)*odds)/100+as.numeric(wager)),"")))
### Final Four
final_four <- expanded_schedule |>
  filter(grepl('Final Four', round)) |>
  left_join(select(elite_eight, team, payout), by='team') |>
  dplyr::rename(wager = payout) |>
  mutate(payout = ifelse(odds < 0, as.numeric(win)*(as.numeric(wager)/(-1*(odds/100))+as.numeric(wager)),
                         ifelse(odds > 0, as.numeric(win)*((as.numeric(wager)*odds)/100+as.numeric(wager)),"")))
### National Championship
national_champ <- expanded_schedule |>
  filter(grepl('National Championship', round)) |>
  left_join(select(final_four, team, payout), by='team') |>
  dplyr::rename(wager = payout) |>
  mutate(payout = ifelse(odds < 0, as.numeric(win)*(as.numeric(wager)/(-1*(odds/100))+as.numeric(wager)),
                         ifelse(odds > 0, as.numeric(win)*((as.numeric(wager)*odds)/100+as.numeric(wager)),"")))
## Final Payouts
final_payouts <- rbind.fill(first_round, second_round,
                            sweet_sixteen, elite_eight, final_four, national_champ)

### Create a DF for team's journey
team_journey <- first_round |>
  select(team, odds, payout) |>
  dplyr::rename(rd1_odds = odds, rd1_payout = payout) |>
  left_join(select(second_round, team, odds, payout), by='team') |>
  dplyr::rename(rd2_odds = odds, rd2_payout = payout) |>
  left_join(select(sweet_sixteen, team, odds, payout), by='team') |>
  dplyr::rename(rd3_odds = odds, rd3_payout = payout) |>
  left_join(select(elite_eight, team, odds, payout), by='team') |>
  dplyr::rename(rd4_odds = odds, rd4_payout = payout) |>
  left_join(select(final_four, team, odds, payout), by='team') |>
  dplyr::rename(rd5_odds = odds, rd5_payout = payout) |>
  left_join(select(national_champ, team, odds, payout), by='team') |>
  dplyr::rename(rd6_odds = odds, rd6_payout = payout)
team_journey$rd1_payout <- as.numeric(team_journey$rd1_payout)
team_journey$rd2_payout <- as.numeric(team_journey$rd2_payout)
team_journey$rd3_payout <- as.numeric(team_journey$rd3_payout)
team_journey$rd4_payout <- as.numeric(team_journey$rd4_payout)
team_journey$rd5_payout <- as.numeric(team_journey$rd5_payout)
team_journey$rd6_payout <- as.numeric(team_journey$rd6_payout)

### Sweet Sixteen calculations
sweet_sixteen_payoffs <- team_journey |>
  select(team, rd1_odds, rd1_payout, rd2_odds, rd2_payout) |>
  left_join(select(milestone_odds, team, sweet_16), by='team') |>
  arrange(sweet_16) |>
  mutate(wager = 10, make_16 = ifelse((!is.na(rd2_payout) & rd2_payout != 0), 1, 0),
  sweet_16_payout = ifelse(sweet_16 < 0, as.numeric(make_16)*(as.numeric(wager)/(-1*(sweet_16/100))+as.numeric(wager)),
                           ifelse(sweet_16 > 0, as.numeric(make_16)*((as.numeric(wager)*sweet_16)/100+as.numeric(wager)),"")),
  net_payout = as.numeric(sweet_16_payout) - as.numeric(rd2_payout)) |>
  filter(make_16 == 1) |>
  relocate(team, sweet_16, wager)
sweet_sixteen_payoffs$sweet_16_payout <- as.numeric(sweet_sixteen_payoffs$sweet_16_payout)
### Sweet Sixteen hypothetical calculations
hypo_sweet_sixteen_payoffs <- team_journey |>
  select(team, rd1_odds, rd1_payout, rd2_odds) |>
  left_join(select(milestone_odds, team, sweet_16), by='team') |>
  filter(!is.na(rd2_odds)) |>
  arrange(sweet_16) |>
  mutate(rd_2_fake_payout = ifelse(rd2_odds < 0, (as.numeric(rd1_payout)/(-1*(rd2_odds/100))+as.numeric(rd1_payout)),
                                   ifelse(rd2_odds > 0, ((as.numeric(rd1_payout)*rd2_odds)/100+as.numeric(rd1_payout)),"")),
         wager = 10, make_16 = ifelse((rd1_payout > 0), 1, 0),
         sweet_16_payout = ifelse(sweet_16 < 0, as.numeric(make_16)*(as.numeric(wager)/(-1*(sweet_16/100))+as.numeric(wager)),
                           ifelse(sweet_16 > 0, as.numeric(make_16)*((as.numeric(wager)*sweet_16)/100+as.numeric(wager)),"")),
         net_payout = as.numeric(sweet_16_payout) - as.numeric(rd_2_fake_payout)) |>
  relocate(team, sweet_16, wager)
hypo_sweet_sixteen_payoffs$rd_2_fake_payout <- as.numeric(hypo_sweet_sixteen_payoffs$rd_2_fake_payout)
hypo_sweet_sixteen_payoffs$sweet_16_payout <- as.numeric(hypo_sweet_sixteen_payoffs$sweet_16_payout)
### Sweet 16 table
sweet_sixteen_tbl <- sweet_sixteen_payoffs |>
  left_join(select(team_info, display_name, logo), by = c('team' = 'display_name')) |>
  select(logo, sweet_16, rd2_payout, sweet_16_payout, net_payout) |>
  ungroup() |> gt() |>
  fmt_currency(columns = c(rd2_payout, sweet_16_payout, net_payout), decimals = 1) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  data_color(columns = net_payout, method = "bin", palette = c("red", "yellow", "green"), bins = c(-1000, -1, 1, 1000)) |>
  cols_label(logo = "Team", sweet_16 = "Sweet 16 Odds", rd2_payout = "Rollover Payout", sweet_16_payout = "Sweet 16 Payout",
             net_payout = "Payout Discrepancy") |> 
  opt_row_striping() |>
  tab_header(title = "Sweet 16 Payouts",
             subtitle = "Odds: FanDuel, using $10 starting wager") |>
  gt_theme_538()
sweet_sixteen_tbl
gtsave(sweet_sixteen_tbl, "Outputs/Sweet 16 Payouts.png")
### Hypothetical Sweet 16 table
hypo_sweet_sixteen_tbl <- hypo_sweet_sixteen_payoffs |>
  left_join(select(team_info, display_name, logo), by = c('team' = 'display_name')) |>
  select(logo, sweet_16, rd_2_fake_payout, sweet_16_payout, net_payout) |>
  ungroup() |> gt() |>
  fmt_currency(columns = c(rd_2_fake_payout, sweet_16_payout, net_payout), decimals = 1) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  data_color(columns = net_payout, method = "bin", palette = c("red", "yellow", "green"), bins = c(-1000, -1, 1, 1000)) |>
  cols_label(logo = "Team", sweet_16 = "Sweet 16 Odds", rd_2_fake_payout = "Rollover Payout", sweet_16_payout = "Sweet 16 Payout",
             net_payout = "Payout Discrepancy") |> 
  opt_row_striping() |>
  tab_header(title = "Hypothetical Sweet 16 Payouts",
             subtitle = "Odds: FanDuel, using $10 starting wager") |>
  gt_theme_538()
hypo_sweet_sixteen_tbl
gtsave(hypo_sweet_sixteen_tbl, "Outputs/Hypo Sweet 16 Payouts.png")

### Elite Eight calculations
elite_eight_payoffs <- team_journey |>
  select(team, rd1_odds, rd1_payout, rd2_odds, rd2_payout, rd3_odds, rd3_payout) |>
  left_join(select(milestone_odds, team, elite_eight), by='team') |>
  arrange(elite_eight) |>
  mutate(wager = 10, make_8 = ifelse((!is.na(rd3_payout) & rd3_payout != 0), 1, 0),
         elite_8_payout = ifelse(elite_eight < 0, as.numeric(make_8)*(as.numeric(wager)/(-1*(elite_eight/100))+as.numeric(wager)),
                                  ifelse(elite_eight > 0, as.numeric(make_8)*((as.numeric(wager)*elite_eight)/100+as.numeric(wager)),"")),
         net_payout = as.numeric(elite_8_payout) - as.numeric(rd3_payout)) |>
  filter(make_8 == 1) |>
  relocate(team, elite_eight, wager)
elite_eight_payoffs$elite_8_payout <- as.numeric(elite_eight_payoffs$elite_8_payout)
### Elite Eight hypothetical calculations
hypo_elite_eight_payoffs <- team_journey |>
  select(team, rd1_odds, rd1_payout, rd2_odds, rd2_payout, rd3_odds) |>
  left_join(select(milestone_odds, team, elite_eight), by='team') |>
  filter(!is.na(rd3_odds)) |>
  arrange(elite_eight) |>
  mutate(rd_3_fake_payout = ifelse(rd3_odds < 0, (as.numeric(rd2_payout)/(-1*(rd3_odds/100))+as.numeric(rd2_payout)),
                                   ifelse(rd3_odds > 0, ((as.numeric(rd2_payout)*rd3_odds)/100+as.numeric(rd2_payout)),"")),
         wager = 10, make_8 = ifelse((rd2_payout > 0), 1, 0),
         elite_8_payout = ifelse(elite_eight < 0, as.numeric(make_8)*(as.numeric(wager)/(-1*(elite_eight/100))+as.numeric(wager)),
                                  ifelse(elite_eight > 0, as.numeric(make_8)*((as.numeric(wager)*elite_eight)/100+as.numeric(wager)),"")),
         net_payout = as.numeric(elite_8_payout) - as.numeric(rd_3_fake_payout)) |>
  relocate(team, elite_eight, wager)
hypo_elite_eight_payoffs$rd_3_fake_payout <- as.numeric(hypo_elite_eight_payoffs$rd_3_fake_payout)
hypo_elite_eight_payoffs$elite_8_payout <- as.numeric(hypo_elite_eight_payoffs$elite_8_payout)
### Elite 8 table
elite_eight_tbl <- elite_eight_payoffs |>
  left_join(select(team_info, display_name, logo), by = c('team' = 'display_name')) |>
  select(logo, elite_eight, rd3_payout, elite_8_payout, net_payout) |>
  ungroup() |> gt() |>
  fmt_currency(columns = c(rd3_payout, elite_8_payout, net_payout), decimals = 1) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  data_color(columns = net_payout, method = "bin", palette = c("red", "yellow", "green"), bins = c(-1000, -1, 1, 1000)) |>
  cols_label(logo = "Team", elite_eight = "Elite 8 Odds", rd3_payout = "Rollover Payout", elite_8_payout = "Elite 8 Payout",
             net_payout = "Payout Discrepancy") |> 
  opt_row_striping() |>
  tab_header(title = "Elite 8 Payouts",
             subtitle = "Odds: FanDuel, using $10 starting wager") |>
  gt_theme_538()
elite_eight_tbl
gtsave(elite_eight_tbl, "Outputs/Elite 8 Payouts.png")
### Hypothetical Elite 8 table
hypo_elite_eight_tbl <- hypo_elite_eight_payoffs |>
  left_join(select(team_info, display_name, logo), by = c('team' = 'display_name')) |>
  select(logo, elite_eight, rd_3_fake_payout, elite_8_payout, net_payout) |>
  ungroup() |> gt() |>
  fmt_currency(columns = c(rd_3_fake_payout, elite_8_payout, net_payout), decimals = 1) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  data_color(columns = net_payout, method = "bin", palette = c("red", "yellow", "green"), bins = c(-1000, -1, 1, 1000)) |>
  cols_label(logo = "Team", elite_eight = "Elite 8 Odds", rd_3_fake_payout = "Rollover Payout", elite_8_payout = "Elite 8 Payout",
             net_payout = "Payout Discrepancy") |> 
  opt_row_striping() |>
  tab_header(title = "Hypothetical Elite 8 Payouts",
             subtitle = "Odds: FanDuel, using $10 starting wager") |>
  gt_theme_538()
hypo_elite_eight_tbl
gtsave(hypo_elite_eight_tbl, "Outputs/Hypo Elite 8 Payouts.png")

### Championship hypothetical calculations
hypo_champ_payoffs <- team_journey |>
  select(team, rd1_odds, rd1_payout, rd2_odds, rd2_payout, rd3_odds,
         rd4_odds, rd4_payout, rd5_odds, rd5_payout, rd6_odds, rd6_payout) |>
  left_join(select(milestone_odds, team, price), by='team') |>
  filter(!is.na(rd6_odds)) |>
  arrange(price) |>
  mutate(rd_6_fake_payout = ifelse(rd6_odds < 0, (as.numeric(rd5_payout)/(-1*(rd6_odds/100))+as.numeric(rd5_payout)),
                                   ifelse(rd6_odds > 0, ((as.numeric(rd5_payout)*rd6_odds)/100+as.numeric(rd5_payout)),"")),
         wager = 10, make_champ = ifelse((rd5_payout > 0), 1, 0),
         champ_payout = ifelse(price < 0, as.numeric(make_champ)*(as.numeric(wager)/(-1*(price/100))+as.numeric(wager)),
                                 ifelse(price > 0, as.numeric(make_champ)*((as.numeric(wager)*price)/100+as.numeric(wager)),"")),
         net_payout = as.numeric(champ_payout) - as.numeric(rd_6_fake_payout)) |>
  relocate(team, price, wager)
hypo_champ_payoffs$rd_6_fake_payout <- as.numeric(hypo_champ_payoffs$rd_6_fake_payout)
hypo_champ_payoffs$champ_payout <- as.numeric(hypo_champ_payoffs$champ_payout)
### Hypothetical Elite 8 table
hypo_champ_tbl <- hypo_champ_payoffs |>
  left_join(select(team_info, display_name, logo), by = c('team' = 'display_name')) |>
  select(logo, price, rd_6_fake_payout, champ_payout, net_payout) |>
  ungroup() |> gt() |>
  fmt_currency(columns = c(rd_6_fake_payout, champ_payout, net_payout), decimals = 1) |>
  gt_img_rows(logo) |>
  opt_align_table_header("center") |> cols_align("center") |>
  tab_source_note("Table: Andrew Kessler | data: nflreadr | @DuvalAndrew904") |>
  data_color(columns = net_payout, method = "bin", palette = c("red", "yellow", "green"), bins = c(-1000, -1, 1, 1000)) |>
  cols_label(logo = "Team", price = "Championship Odds", rd_6_fake_payout = "Rollover Payout", champ_payout = "Championship Payout",
             net_payout = "Payout Discrepancy") |> 
  opt_row_striping() |>
  tab_header(title = "Hypothetical Championship Payouts",
             subtitle = "Odds: FanDuel, using $10 starting wager") |>
  gt_theme_538()
hypo_champ_tbl
gtsave(hypo_champ_tbl, "Outputs/Hypo Champ Payouts.png")
