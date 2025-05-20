# ---- initialization ----
initialize_simulation_new <- function(initial_max_stakes   = NULL,
                                      initial_total_unbond = NULL,
                                      era_number           = 0,
                                      window_size          = 28,
                                      min_unbond_delay     = 2) {
  # sanity checks
  stopifnot(is.numeric(window_size), window_size >= 1)
  stopifnot(is.numeric(min_unbond_delay),
            min_unbond_delay >= 0,
            min_unbond_delay < window_size)
  # store parameters
  assign("window_size",      as.integer(window_size),  envir = .GlobalEnv)
  assign("min_unbond_delay", as.integer(min_unbond_delay), envir = .GlobalEnv)

  # optionally seed the per-era vectors
  if (!is.null(initial_max_stakes)) {
    stopifnot(length(initial_max_stakes) == window_size)
    assign("max_stake_per_era", initial_max_stakes, envir = .GlobalEnv)
  }
  if (!is.null(initial_total_unbond)) {
    stopifnot(length(initial_total_unbond) == window_size)
    assign("total_unbond_in_era", initial_total_unbond, envir = .GlobalEnv)
  }

  # reset records and current era
  assign("unbonding_records", data.frame(
    amount              = numeric(0),
    start_era           = integer(0),
    prev_unbonded_stake = numeric(0),
    stringsAsFactors    = FALSE
  ), envir = .GlobalEnv)
  assign("current_era", as.integer(era_number), envir = .GlobalEnv)
}

# ---- withdraw check ----
withdraw_check <- function(rec, era_check) {
  # not yet past the min delay?
  if (era_check < rec$start_era + min_unbond_delay) {
    return(FALSE)
  }

  # if the record is entirely before our sliding window, it's safe
  window_start <- era_check - (window_size - 1)
  if (rec$start_era < window_start) {
    return(TRUE)
  }

  total_unbond <- 0
  # scan backwards from start_era down to window_start
  for (e in seq(rec$start_era, window_start, by = -1)) {
    slot <- (e %% window_size) + 1

    if (e == rec$start_era) {
      total_unbond <- min(
        total_unbond_in_era[slot],
        rec$prev_unbonded_stake + rec$amount
      )
    } else {
      total_unbond <- total_unbond + total_unbond_in_era[slot]
    }

    if (total_unbond >= max_stake_per_era[slot]) {
      return(FALSE)
    }
  }

  TRUE
}

# ---- estimate withdraw era ----
estimate_unbonding_era <- function(rec) {
  earliest <- rec$start_era + min_unbond_delay
  latest   <- rec$start_era + window_size
  for (era_try in seq(earliest, latest)) {
    if (withdraw_check(rec, era_try)) {
      return(era_try)
    }
  }
  # if we never pass, it's exactly at the edge of the full window
  rec$start_era + window_size
}

fill_zeroes <- function(from_era, to_era) {
  for (e in seq(from_era + 1L, to_era - 1L)) {
    slot <- (e %% window_size) + 1L
    # record a zero‐amount “unbond” so that
    # total_unbond_in_era[slot] is explicitly zeroed
    unbonding_records <<- rbind(
      unbonding_records,
      data.frame(amount = 0,
                 start_era = e,
                 prev_unbonded_stake = 0,
                 stringsAsFactors = FALSE)
    )
    total_unbond_in_era[slot] <<- 0
    # optionally reset max_stake_per_era[slot] to your default
  }
}

# ---- unbond w/ dynamic window & delay ----
unbond <- function(amount, era_index, max_stake) {
  # make sure we fill the history with zeros if we jumped into the future.
  fill_zeroes(current_era, era_index)
  # 1) jump to the requested era
  current_era <<- era_index

  # 2) measure queue fullness across the configured window
  queue_fullness <- sum(total_unbond_in_era)

  # 3) find which slot in [1..window_size] this era maps to
  slot <- (current_era %% window_size) + 1

  # 4) how much has already unbonded this era?
  if (nrow(unbonding_records) == 0) {
    prev_slot_total <- 0
  } else {
    prev_slot_total <-
      sum(unbonding_records$amount[
        unbonding_records$start_era == current_era
      ])
  }

  # 5) update threshold & record this unbond
  max_stake_per_era[slot] <<- max_stake
  unbonding_records <<- rbind(
    unbonding_records,
    data.frame(
      amount              = amount,
      start_era           = current_era,
      prev_unbonded_stake = prev_slot_total,
      stringsAsFactors    = FALSE
    )
  )

  # 6) add to the global tally
  total_unbond_in_era[slot] <<- total_unbond_in_era[slot] + amount

  # 7) estimate withdrawal era
  rec     <- tail(unbonding_records, 1)
  est_era <- estimate_unbonding_era(rec)
  est_delta <- est_era - current_era

  message(sprintf(
    "Unbonded %s at era %d; queue_fullness=%.0f, max_stake=%s\n → withdraw at era %d (in %d eras)",
    amount, current_era, queue_fullness, max_stake,
    est_era, est_delta
  ))
}

# Simulate historic unbonds to start the queue.
hist_unbond <- rep(0,window_size)
#hist_unbond <- rnorm(28, mean = 35, sd = 5)
hist_max   <- rep(10, window_size)
initialize_simulation_new(hist_max, hist_unbond, 0)

unbond(1, era_index=0, max_stake=10)
unbond(2, era_index=1, max_stake=10)
unbond(7, era_index=2, max_stake=10)
unbond(8, era_index=3, max_stake=10)
unbond(10, era_index=4, max_stake=10)
unbond(1, era_index=5, max_stake=10)
unbond(2, era_index=6, max_stake=10)
unbond(3, era_index=7, max_stake=10)
unbond(5, era_index=8, max_stake=10)
unbond(6, era_index=9, max_stake=10)
unbond(4, era_index=10, max_stake=10)
unbond(5, era_index=11, max_stake=10)
unbond(2, era_index=12, max_stake=10)
unbond(3, era_index=13, max_stake=10)
unbond(1, era_index=14, max_stake=10)
unbond(7, era_index=15, max_stake=10)
unbond(8, era_index=16, max_stake=10)
unbond(1, era_index=17, max_stake=10)
unbond(2, era_index=18, max_stake=10)
unbond(4, era_index=19, max_stake=10)
