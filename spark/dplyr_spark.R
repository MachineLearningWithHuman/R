library(sparklyr)
library(dplyr)
library(nycflights13)
library(ggplot2)

#spark_install()
sc <- spark_connect(master="local")
flights <- copy_to(sc, flights, "flights")
airlines <- copy_to(sc, airlines, "airlines")
src_tbls(sc)

#selecting
head(flights)

select(flights, year:day, arr_delay, dep_delay)

#filter data
filter(flights , dep_delay >1000)

#arrane
arrange(flights, desc(dep_delay))

#summaerise
summarise(flights, mean_dep_delay = mean(dep_delay))

#mutate
mutate(flights, speed = distance / air_time * 60)

#lazyness
c1 <- filter(flights, day == 17, month == 5, carrier %in% c('UA', 'WN', 'AA', 'DL'))
c2 <- select(c1, year, month, day, carrier, dep_delay, air_time, distance)
c3 <- arrange(c2, year, month, day, carrier)
c4 <- mutate(c3, air_time_hours = air_time / 60)
c4

#piping
c4 <- flights %>%
  filter(month == 5, day == 17, carrier %in% c('UA', 'WN', 'AA', 'DL')) %>%
  select(carrier, dep_delay, air_time, distance) %>%
  arrange(carrier) %>%
  mutate(air_time_hours = air_time / 60)
c4 %>%
  group_by(carrier) %>%
  summarize(count = n(), mean_dep_delay = mean(dep_delay))

#copy data to local
carrierhours <- collect(c4)

# Test the significance of pairwise differences and plot the results
with(carrierhours, pairwise.t.test(air_time, carrier))

ggplot(c4, aes(carrier, air_time_hours)) + geom_boxplot()
ggplot(carrierhours, aes(carrier, air_time_hours)) + geom_boxplot()


#sql
# Find the most and least delayed flight each day
bestworst <- flights %>%
  group_by(year, month, day) %>%
  select(dep_delay) %>% 
  filter(dep_delay == min(dep_delay) || dep_delay == max(dep_delay))
dbplyr::sql_render(bestworst)
bestworst

# Rank each flight within a daily
ranked <- flights %>%
  group_by(year, month, day) %>%
  select(dep_delay) %>% 
  mutate(rank = rank(desc(dep_delay)))
dbplyr::sql_render(ranked)

#join
flights %>% left_join(airlines)
flights %>% left_join(airlines, by = "carrier")

#sampling
sample_n(flights, 10)
sample_frac(flights, 0.01)

#writing
spark_write_parquet(tbl, "hdfs://hdfs.company.org:9000/hdfs-path/data")
tbl <- spark_read_parquet(sc, "data", "hdfs://hdfs.company.org:9000/hdfs-path/data")

#hive function
flights %>% 
  mutate(flight_date = paste(year,month,day,sep="-"),
         days_since = datediff(current_date(), flight_date)) %>%
  group_by(flight_date,days_since) %>%
  tally() %>%
  arrange(-days_since)
