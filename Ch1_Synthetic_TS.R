require(tidyverse)
require(dplyr)
require(timetk)

options(warn = 0)

##############################################
### **** Noise in Data generation Process ****
##############################################

time <- 0:199
# Sample 200 random values from a standard normal distribution
# and scale by 100
white_noise <- rnorm(200) * 100

time_series_wn <- data.frame(time = time,vals = white_noise)

p <- time_series_wn %>%
  plot_time_series(time,vals,.interactive = TRUE)

# print(p)

## RED NOISE
# x_j+1 = r*x_j + sqrt(1-r^2) * w

# Correlation coefficient
r <- 0.6

# create zeros of same length as white noise
values <- numeric(200)

for (i in seq_along(white_noise)) {
  if (i == 1) {
    values[i] <- white_noise[i]
  } else {
    values[i] <- r * values[i - 1] +
      sqrt(1 - r^2) * white_noise[i]
  }
}

time_series_rn <- data.frame(time=time,red_noise=values)

time_series_rn %>%
  plot_time_series(
    .date_var = time,
    .value = red_noise,
    .smooth = FALSE
  )

ts_white_red <- time_series_wn %>%
  left_join(time_series_rn,by="time")%>%
  rename(white_noise=vals)

ts_long <- ts_white_red %>%
  pivot_longer(
    cols = -time,
    names_to = "Noise",
    values_to = "value"
  )

ts_long %>%
  plot_time_series(
    .date_var = time,
    .value = value,
    .color_var = Noise,
    .smooth = FALSE,
    .title = "Synthethic Time Series with White and Red Noise",
    .interactive = TRUE,
    .line_size = 0.8
  )
  # scale_color_manual(values = c(
  #   "white_noise" = "midnightblue",
  #   "red_noise" = "red3"
  # ))+
  # scale_x_continuous(
  #   breaks = scales::breaks_pretty(n = 20),
  #   expand = c(0, 0)) +
  # theme(
  #   panel.border = element_blank(),
  #   panel.grid.major = element_line(color = alpha("darkslateblue", 0.5), linewidth = 0.1),
  #   panel.grid.minor = element_line(color = alpha("darkslateblue", 0.3), linewidth = 0.1)
  # )


##########################################
### **** Cyclical or seasonal signals ****
##########################################

# Function to create different time points
generate_time_samples <- function(stop_time = 20, num_points = 100) {
  seq(0, stop_time, length.out = num_points)
}

#### sinusoidal function to create cyclicity
sinusoidal_signal <- function(time, amplitude, frequency) {
  amplitude * sin(2 * pi * frequency * time)
}

# Generate noise (white)
gen_noise <- function(n, sd = 0.2) {
  rnorm(n, mean = 0, sd = sd)
}

# Function to create a time series based on specific signal function
generate_timeseries <- function(time, signal_func, noise_sd = NULL) {
  
  # true signal
  signals <- signal_func(time)
  
  # noise
  if (!is.null(noise_sd)) {
    errors <- gen_noise(length(time), sd = noise_sd)
  } else {
    errors <- rep(0, length(time))
  }
  
  # observed samples
  samples <- signals + errors
  result <- data.frame(
    time = time,
    samples = samples,
    signal = signals,
    error = errors
  )
  
  return(result)
}


time <- generate_time_samples(stop_time = 20, num_points = 200)

signal_1_func <- function(t) {
  sinusoidal_signal(t, amplitude = 1.5, frequency = 0.25)
}
result_1 <- generate_timeseries(time, signal_1_func,noise_sd = 0) %>%
  rename(samples_1 = samples,signal_1 = signal,error_1 = error)

signal_2_func <- function(t) {
  sinusoidal_signal(t, amplitude = 1, frequency = 0.5)
}
result_2 <- generate_timeseries(time, signal_2_func,noise_sd = 0) %>%
  rename(samples_2 = samples,signal_2 = signal,error_2 = error)

combined_signals <- result_1 %>%
  left_join(result_2, by="time") %>%
  pivot_longer(-time, names_to = "type", values_to = "value") %>%
  filter(type=="samples_1"|type=="samples_2")

p <- combined_signals %>%
  plot_time_series(
    .date_var = time,
    .value = value,
    .color_var = type,
    .interactive = TRUE,
    .line_size = 1.2,
    .smooth = FALSE,
    .line_type = NULL
  )

combined_signals %>%
  plot_time_series(
    .date_var = time,
    .value = value,
    .color_var = type,
    .interactive = FALSE,
    .line_size = 1.2,
    .smooth = FALSE,
    .title = "Synthetic Sinusoidal Signals"
  ) +
  scale_color_manual(
    name = "Series",
    values = c(
      samples_1 = "blue",
      samples_2 = "firebrick"
    ),
    labels = c(
      samples_1 = "Amplitude = 1.5 | Frequency = 0.25",
      samples_2 = "Amplitude = 1 | Frequency = 0.5"
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.87,1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    panel.border = element_blank()
  )

##### pseudoperiodic function to create cyclicity
PseudoPeriodic_signal <- function(time, amplitude, frequency) {
  # Smooth amplitude variation
  amp <- 1 + 0.15 * sin(0.8 * pi * 0.2 * time)
  # Smooth phase variation
  phase <- cumsum(rnorm(length(time), sd = 1))
  return(amplitude * amp * sin(2 * pi * frequency * time + phase))
}

PseudoP_signal <- function(t){
  PseudoPeriodic_signal(t,amplitude = 1,frequency = 0.25)
}
PseudoP_result <- generate_timeseries(time = time,
                                      signal_func = PseudoP_signal,
                                      noise_sd = NULL)
PseudoP_result %>%
  plot_time_series(
    .date_var = time,
    .value = samples,
    .line_color = "blue",
    .smooth = FALSE,
    .title = "Pseudo-Periodic Time Series"
  )

##### Autoregressive signal
## We are generating a ts using 2nd order autoregressive function
n <- 200

phi1 <- 1.5
phi2 <- -0.75

## AR(2) : x(t) = 1.5 * x(t-1) - 0.75 * x(t-2) where t = current time step

ar_signal <- numeric(n) # empty list for AR values
ar_noise <- rnorm(n, mean = 0, sd = 1) # white noise with std normal dist

###### Homework : Try for 3rd order AR #####
# Initial values
ar_signal[1] <- ar_noise[1]
ar_signal[2] <- phi1 * ar_signal[1] + ar_noise[2]

# AR(2) recursion
autoreg_func <- function(t){
  for (t in 3:n) {
    ar_signal[t] <- phi1 * ar_signal[t - 1] +  phi2 * ar_signal[t - 2] +  ar_noise[t]
  }
  return(ar_signal)
}

ar_result <- generate_timeseries(time, autoreg_func,noise_sd = 0)

ar_result %>%
  plot_time_series(
    .date_var = time,
    .value = samples,
    .line_color = "blue",
    .smooth = FALSE
  )


pseudo_samples <- generate_timeseries(
  generate_time_samples(stop_time = 50, num_points = 300),
  PseudoP_signal,0.3
)

ar_signal <- numeric(300)
ar_noise <- rnorm(300, mean = 0, sd = 1)

# Initial values
ar_signal[1] <- ar_noise[1]
ar_signal[2] <- phi1 * ar_signal[1] + ar_noise[2]
ar_samples <- generate_timeseries(generate_time_samples(stop_time = 50, num_points = 300), autoreg_func)

ts <- pseudo_samples*2+ar_samples
ts$time <- generate_time_samples(stop_time = 50, num_points = 300)

ts %>%
  plot_time_series(
    .date_var = time,
    .value = samples,
    .line_color = "blue",
    .smooth = FALSE
  )

## generate a time series with trend and seasonality
regular_time_samples <- generate_time_samples(
  stop_time = 20, num_points = 200
)

sinusoidal_signals <- function(t){
  sinusoidal_signal(t,amplitude = 1,frequency = 0.25)
}
sinusoidal_samples <- generate_timeseries(
  regular_time_samples,sinusoidal_signals,noise_sd = 0.3)

trend <- regular_time_samples * 0.4
sin_ts <- sinusoidal_samples + trend
sin_ts$time <- regular_time_samples

sin_ts %>%
  plot_time_series(
    .date_var = time,
    .value = samples,
    .line_color = "blue",
    .line_size = 0.8,
    .smooth = FALSE,
    .interactive = FALSE
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n=20),
    expand = c(0,0)
  )

# Increasing variance over time
trend <- 5+regular_time_samples * 0.2
sigma <- 0.2 + 0.2 * trend
noise_var <- rnorm(200, sd = sigma)

sin_ts_var <-sinusoidal_samples + trend + noise_var
sin_ts_var$time <- regular_time_samples

sin_ts_var %>%
  plot_time_series(
    .date_var = time,
    .value = samples,
    .line_color = "blue",
    .line_size = 0.8,
    .smooth = FALSE,
    .interactive = FALSE
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n=20),
    expand = c(0,0)
  )
