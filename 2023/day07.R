# Data
data <- readLines("2023/inputs/day07_input.txt") |>
  strsplit(split = " ")
cards <- vapply(data, "[[", character(1), 1)
bids <- lapply(data, "[[", 2) |>
  as.integer()

# Part 1
rank_cards <- function(cards, bids) {
  cards <- cards |>
    gsub(pattern = "T", replacement = "a") |>
    gsub(pattern = "J", replacement = "b") |>
    gsub(pattern = "Q", replacement = "c") |>
    gsub(pattern = "K", replacement = "d") |>
    gsub(pattern = "A", replacement = "e")
  cards_hex <- as.hexmode(cards)
  card_mat <- cards |>
    strsplit(split = "") |>
    do.call(what = "rbind")
  tabs <- apply(card_mat, 1, table)
  z <- logical(1)
  types <- list(
    `high_card` = vapply(tabs, function(x) length(x) == 5, z),
    `one_pair` = vapply(tabs, function(x) length(x) == 4, z),
    `two_pair` = vapply(tabs, function(x) length(x) == 3 && 2 %in% x, z),
    `three_kind` = vapply(tabs, function(x) length(x) == 3 && 3 %in% x, z),
    `full_house` = vapply(tabs, function(x) length(x) == 2 && 3 %in% x, z),
    `four_kind` = vapply(tabs, function(x) length(x) == 2 && 4 %in% x, z),
    `five_kind` = vapply(tabs, function(x) length(x) == 1, z)
  )
  card_order <- c()
  for (type in types) {
    idx <- which(type)
    ord <- order(cards_hex[idx])
    card_order <- c(card_order, idx[ord])
  }
  sum(seq_along(cards) * bids[card_order])
}

result1 <- rank_cards(cards, bids)
result1

# Part 2
is_high_card <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  len == 5 && !"1" %in% nm
}

is_one_pair <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  (len == 4 && !"1" %in% nm) || (len == 5 && "1" %in% nm)
}

is_two_pair <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  len == 3 && 2 %in% tab && !"1" %in% nm
}

is_three_kind <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  (len == 3 && 3 %in% tab && !"1" %in% nm) || (len == 4 && "1" %in% nm)
}

is_full_house <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  (len == 2 && 3 %in% tab && !"1" %in% nm) || 
    (len == 3 && 2 %in% tab && "1" %in% nm && tab["1"] == 1)
}

is_four_kind <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  (len == 2 && 4 %in% tab && !"1" %in% nm) || 
    (len == 3 && 3 %in% tab && "1" %in% nm && tab["1"] %in% c(1, 3)) ||
    (len == 3 && 2 %in% tab && "1" %in% nm && tab["1"] == 2)
}

is_five_kind <- function(tab) {
  len <- length(tab)
  nm <- names(tab)
  (len == 1) || (len == 2 && "1" %in% nm)
}

rank_cards_joker <- function(cards, bids) {
  cards <- cards |>
    gsub(pattern = "T", replacement = "a") |>
    gsub(pattern = "J", replacement = "1") |>
    gsub(pattern = "Q", replacement = "c") |>
    gsub(pattern = "K", replacement = "d") |>
    gsub(pattern = "A", replacement = "e")
  cards_hex <- as.hexmode(cards)
  card_mat <- cards |>
    strsplit(split = "") |>
    do.call(what = "rbind")
  tabs <- apply(card_mat, 1, table)
  z <- logical(1)
  types <- list(
    `high_card` = vapply(tabs, is_high_card, z),
    `one_pair` = vapply(tabs, is_one_pair, z),
    `two_pair` = vapply(tabs, is_two_pair, z),
    `three_kind` = vapply(tabs, is_three_kind, z),
    `full_house` = vapply(tabs, is_full_house, z),
    `four_kind` = vapply(tabs, is_four_kind, z),
    `five_kind` = vapply(tabs, is_five_kind, z)
  )
  card_order <- c()
  for (type in types) {
    idx <- which(type)
    ord <- order(cards_hex[idx])
    card_order <- c(card_order, idx[ord])
  }
  sum(seq_along(cards) * bids[card_order])
}

result2 <- rank_cards_joker(cards, bids)
result2
