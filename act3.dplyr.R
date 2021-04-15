## activity 3:dplyr
#install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
data("flights")
?flights
names(flights)
filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, carrier %in% c("UA", "AA", "DL"))
filter(flights, month %in% c(7,8,9))
# filter(flights, month >= 7, month <= 9)

filter(flights, dep_time <= 600 | dep_time == 2400)
filter(flights, dep_time == 2400 | dep_time <= 600)
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
table(flights$dest)
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")
sum(is.na(flights$dep_delay))
table(is.na(flights$dep_delay))

# Activity 3 - Part 2
flights
flights_sml <- select (flights, dep_time, sched_dep_time)
mutate(flights_sml,
        dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
        sched_dep_time_mins = ((sched_dep_time %/% 100)*60) + sched_dep_time %% 100
      )

flights_sml <- select(flights, air_time, arr_time, dep_time)
mutate (flights_sml,
        arr_time_mins = ((arr_time %/% 100)*60) + arr_time %% 100,
        dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
        diff = arr_time_mins - dep_time_mins
        )

flights_sml <- select(flights, dep_time, sched_dep_time, dep_delay)
mutate(flights_sml,
       dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
       sched_dep_time_mins = ((sched_dep_time %/% 100)*60) + sched_dep_time %% 100,
       diff = dep_time_mins - sched_dep_time_mins
      )

## practice from book ##
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# summarise() is not terribly useful unless we pair it with group_by()
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))  #gives average delay per date

#  explore the relationship between the distance and average delay for each location
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# There is another way to tackle the same problem with the pipe, %>%
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))





# If missing values represent cancelled flights, we could also tackle the problem by first removing the cancelled flights

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# exercise

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n = n())

not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(sum = sum(distance))

# which carrier has worst delays
flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))

flights %>%
  group_by(carrier)%>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE))%>%
  arrange(desc(arr_delay))

# which plane (tailnum) has the worst on-time record?
flights %>%
  filter(arr_delay > 0) %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
  
flights %>%
  filter(dep_delay > 0) %>%
  group_by(tailnum) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_delay))
  
  
  
## act 3 part 2
flights_sml <- select (flights, dep_time, sched_dep_time)
mutate(flights_sml,
       dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
       sched_dep_time_mins = ((sched_dep_time %/% 100)*60) + sched_dep_time %% 100
)

flights_sml <- select(flights, air_time, arr_time, dep_time)
mutate (flights_sml,
        arr_time_mins = ((arr_time %/% 100)*60) + arr_time %% 100,
        dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
        diff = arr_time_mins - dep_time_mins
)

flights_sml <- select(flights, dep_time, sched_dep_time, dep_delay)
mutate(flights_sml,
       dep_time_mins = ((dep_time %/% 100)*60) + dep_time %% 100,
       sched_dep_time_mins = ((sched_dep_time %/% 100)*60) + sched_dep_time %% 100,
       diff = dep_time_mins - sched_dep_time_mins
)


not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n = n())


not_cancelled %>% count(tailnum, wt = distance) 

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(sum = sum(distance))






  
