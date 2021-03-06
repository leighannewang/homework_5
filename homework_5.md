Homework 5
================

## Problem 1

**Read in the homicide dataset**

``` r
homicide_df = 
  read_csv("./data/homicide-data.csv") %>%
  mutate(
    city_state = str_c(city, state, sep = "_"),
    resolved = case_when(
      disposition == "Closed without arrest" ~ "unsolved",
      disposition == "Open/No arrest"        ~ "unsolved",
      disposition == "Closed by arrest"      ~ "solved",
    )
  ) %>% 
  select(city_state, resolved) %>% 
  filter(city_state != "Tulsa_AL")
```

The raw dataset shows us information on homicides in 50 U.S. cities; it
contains variables such as id, city/state and latitude and longitude for
the location of the murder, date of reported crime, and information
about the victim such as name, race, age, and sex.

We created a city\_state variable and are looking at each case based on
whether or not they are solved/unsolved.

**Looking at the dataset**

``` r
aggregate_df = 
  homicide_df %>% 
  group_by(city_state) %>% 
  summarize(
    hom_total = n(),
    hom_unsolved = sum(resolved == "unsolved")
  )
```

In this step we are summarizing to obtain number of unsolved and total
homicides in each city\_state.

**Proportion test for Baltimore, MD:**

``` r
prop.test(
  aggregate_df %>% filter(city_state == "Baltimore_MD") %>% pull(hom_unsolved), 
  aggregate_df %>% filter(city_state == "Baltimore_MD") %>% pull(hom_total)) %>% 
  broom::tidy()
## # A tibble: 1 x 8
##   estimate statistic  p.value parameter conf.low conf.high method    alternative
##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample~ two.sided
```

**Iterate the proportion test for each city:**

``` r
results_df = 
  aggregate_df %>% 
  mutate(
    prop_tests = map2(.x = hom_unsolved, .y = hom_total, ~prop.test(x = .x, n = .y)),
    tidy_tests = map(.x = prop_tests, ~broom::tidy(.x))
  ) %>% 
  select(-prop_tests) %>% 
  unnest(tidy_tests) %>% 
  select(city_state, estimate, conf.low, conf.high)
```

**Plot:**

``` r
results_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="homework_5_files/figure-gfm/results_df plot-1.png" width="90%" />

## Problem 2

**Read in all datasets and create tidy dataframe**

``` r
path_df = 
  tibble(
    path = list.files("data/study")
  ) %>% 
  mutate(
    path = str_c("data/study/", path),
    data = map(.x = path, ~read_csv(.x)) # reading in datasets
  ) %>% 
  unnest(data) %>% 
  mutate(
    path = str_replace(path, "data/study/", "")
  ) %>% 
  separate(path, c("arm", "id"), sep = "_") %>% 
  mutate(
    id = str_replace(id, ".csv", "")
    ) %>% 
# pivot longer to tidy data
  pivot_longer(
    week_1:week_8,
    names_to = "week",
    values_to = "observation",
    names_prefix = "week_"
  ) %>% 
  mutate(
    week = as.numeric(week)
  )
```

**Create spaghetti plot and comment on group differences**

``` r
path_df %>% 
  ggplot(aes(x = week, y = observation, group = id, color = arm)) +
  geom_line() +
  facet_grid(. ~ arm)
```

<img src="homework_5_files/figure-gfm/path_df plot-1.png" width="90%" />

From the plot we can see that the control observation values are lower
than the experimental group’s observations; while the control group’s
values seem to be stable over the 8 weeks, the experimental group’s
values seem to be increasing.

## Problem 3

**Function that will run a t.test to be used later and tibble with
normal distribution with sample size of 30, standard deviation of 5**

``` r
sim_test = function(n = 30, mu, sigma = 5) {
  
  sim_data = 
    tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
    t.test() %>% 
    broom::tidy()
  
}
```

**Running the simulation for mu = 0**

``` r
sim_results = 
  rerun(5000, sim_test(30, 0, 5)) %>% 
  bind_rows()
```

**Running simulation for mu = {0, 1, 2, 3, 4, 5, 6}**

``` r
sim_results = 
  tibble(mu = c(0, 1, 2, 3, 4, 5, 6)) %>% 
  mutate(
    output_lists = map(.x = mu, ~rerun(5000, sim_test(mu = .x))),
    estimate_dfs = map(output_lists, bind_rows)) %>% 
  select(-output_lists) %>% 
  unnest(estimate_dfs) %>% 
  mutate(
    mu_hat = estimate
  ) %>% 
  select(-estimate) %>% 
  relocate(mu, mu_hat)
```

**Plot showing proportion of times null was rejected on y-axis and true
value of mu on x-axis**

``` r
sim_results %>% 
  mutate(
    rejected_null = case_when(
     p.value <= 0.05 ~ "yes",
     p.value > 0.05 ~ "no" 
    )
  ) %>% 
  group_by(mu, rejected_null) %>% 
  filter(rejected_null == "yes") %>% 
  count(rejected_null) %>% 
  summarize(
    prop_rejected = sum(n)/5000
  ) %>% 
  ggplot(aes(x = mu, y = prop_rejected)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "true mu",
    y = "proportion of times null rejected"
  )
```

<img src="homework_5_files/figure-gfm/plot_prop-1.png" width="90%" />

We can see that as the true value of mu increases, so does the
likelihood that the null hypothesis will be rejected, meaning that
higher effect size means more power. The plot shows that as the true
value of mean increases so does the proportion of times false null
hypothesis was rejected until about mu = 3 where it plateaus.

**Plot showing average estimate of mu\_hat on y-axis an true mu on
x-axis and plot of average estimate of mu\_hat where null was rejected
on y-axis and true value of mu on x-axis**

``` r
sim_results %>% 
  group_by(mu) %>%
  mutate(
    mean_mu_hat = mean(mu_hat)
  ) %>% 
  ungroup %>%   
  filter(p.value <= 0.05) %>% 
  group_by(mu) %>% 
  mutate(
    mean_mu_hat_reject = mean(mu_hat)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = mu)) +
  geom_line(aes(y = mean_mu_hat, color = "mean_mu_hat"), size = 1) +
  geom_line(aes(y = mean_mu_hat_reject, color = "mean_mu_hat_reject"), size = 1) +
  labs(
    x = "true mu",
    y = "mean estimate of mu value"
  ) +
  scale_color_manual(values = c("green", "red"))
```

<img src="homework_5_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

The average estimate of mu\_hat when null is rejected is approximately
equal to the true value of mu when mu = 0, 4, 5, 6 because for most
samples the null hypothesis will be rejected so the average estimate is
very close to the true value of mu. When mu = 1, 2, 3, the average
estimate of mu\_hat for rejected null hypotheses is not approximately
equal because samples with rejected null hypothesis are on the upper
tail of the distribution so the average estimate is larger than true
value of mu.
