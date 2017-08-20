zip_code <- 5210

# Get price statistics ----
url_to_call <- paste0("https://bolighed.dk/api/v2/location/price-statistics?postcode=", zip_code)
my_data <- jsonlite::fromJSON(url_to_call)

zip_data <- tibble(
  all_m2 =  my_data$postcode$avg_sqm_value_all$value,
  for_sale_m2 = my_data$postcode$for_sale_external$avg_sqm_value$value,
  maybe_for_sale_m2 = my_data$postcode$maybe_for_sale$avg_sqm_value$value,
  area = zip_code
)

dk_data <- tibble(
  all_m2 =  my_data$denmark$avg_sqm_value_all$value,
  for_sale_m2 = my_data$denmark$for_sale_external$avg_sqm_value$value,
  maybe_for_sale_m2 = my_data$denmark$maybe_for_sale$avg_sqm_value$value,
  area = "dk"
)

price_stats <- rbind(zip_data, dk_data)


# Get home sales ----
url_to_call <- paste0("https://bolighed.dk/api/v2/location/home-sales?postcode=", zip_code)
my_data <- jsonlite::fromJSON(url_to_call)
home_sales <- map_df(1:length(my_data), function(i){
  my_data[[i]] %>%
    as_tibble() %>%
    mutate(date = lubridate::ymd(names(my_data)[i]))
})


# Get home counts ----

url_to_call <- paste0("https://bolighed.dk/api/v2/home/search?limit=0&postcode=", zip_code)
my_data <- jsonlite::fromJSON(url_to_call)

home_counts <- tibble(
  for_sale = my_data$for_sale_external_count$doc_count,
  maybe_for_sale = my_data$maybe_for_sale_count$doc_count,
  total_homes = my_data$total
)

# Get home stats ----
home_count <- home_counts$total_homes
off_sets <- (1:ceiling(home_count/1000)-1)*1000

home_data_list <- pbapply::pblapply(off_sets, function(off_set){
  url_to_call <- paste0("https://bolighed.dk/api/v2/home/search?limit=1000&offset=", off_set, "&postcode=", zip_code)
  my_data <- jsonlite::fromJSON(url_to_call)
  home_data <- my_data$homes
  return(home_data)
})

home_data_df <- bind_rows(home_data_list)

listviewer::jsonedit(home_data_list)


# Get listing period ----
url_to_call <- paste0("https://bolighed.dk/api/v2/listing-period?code=", zip_code)
my_data <- jsonlite::fromJSON(url_to_call)
listing_period <- tibble(
  quarter = names(my_data) %>% lubridate::ymd(),
  days = unlist(my_data)
)

plot(listing_period)


