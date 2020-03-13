### Data for baseball and US birthdays

pacman::p_load(tidyverse, glue, readxl, Lahman)
pacman::p_load_gh("byuidss/DataPushR")

set.seed(20200313)

# US Population data
dat_94_03 <- read_csv("https://github.com/fivethirtyeight/data/raw/master/births/US_births_1994-2003_CDC_NCHS.csv")
dat_00_14 <- read_csv("https://github.com/fivethirtyeight/data/raw/master/births/US_births_2000-2014_SSA.csv")

us_births <- dat_94_03 %>%
  filter(year < 2000) %>%
  bind_rows(dat_00_14) %>% 
  group_by(month, date_of_month) %>%
  summarise(births = sum(births)) %>%
  ungroup() %>%
  mutate(day_of_year = 1:n()) %>%
  left_join(tibble(month = 1:12, month_name = month.name)) %>%
  select(month_number = month, month_name, day_of_month = date_of_month, day_of_year, births)

data_description <- list(month = "The numeric month of the year", 
                        month_name ="The name of the month",
                        day_of_month = "The day number of the month 1-31",
                        day_of_year = "The day number of the year 1-366",
                        births = "The total count of births on that day over the period of the data")

# baseball data
# https://www.billjamesonline.com/article1191/
baseball_births <- Lahman::People %>%
  select(birthYear, birthMonth, birthDay, birthCountry, birthDate) %>%
  rename_all("str_to_lower") %>%
  rename_all(.funs = funs(str_remove_all(.,"birth"))) %>%
  filter(country == "USA", year > 1925, year < 2015) %>%
  count(month, day, name = "births") %>%
  mutate(day_of_year = 1:n()) %>%
  left_join(tibble(month = 1:12, month_name = month.name)) %>%
  select(month_number = month, month_name, day_of_month = day, day_of_year, births)

list_data <- list(baseball_births = baseball_births, us_births = us_births)

### Plots to check

baseball_births %>%
  ggplot(aes(x = month_number, y = births)) +
  geom_boxplot(aes(group = month_number)) +
  scale_x_continuous(breaks = 1:12)

us_births %>%
  ggplot(aes(x = month_number, y = births)) +
  geom_boxplot(aes(group = month_number)) +
  scale_x_continuous(breaks = 1:12) +
  coord_cartesian(ylim = c(200000, 260000))

## Build package

package_name_text <- "data4births"
base_folder <- glue("../")

github_info <- dpr_create_github(package_name_text, post_text = "/orgs/byuidatascience/repos")
# dpr_delete_github("byuidatascience", package_name_text)

package_path <- dpr_create_package(list_data = list_data, 
                                   package_name = package_name_text, 
                                   export_folder = base_folder, 
                                   git_remote = github_info$clone_url)

usethis::proj_set(package_path)

usethis::use_data(us_births, baseball_births)

dpr_document(us_births, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "us_births", title = "The count of births in the United States from 1994-2014",
             description = "Data obtained from the CDC and Census parsed by FiveThirtyEight ",
             source = "https://github.com/fivethirtyeight/data/tree/master/births",
             var_details = data_description)

dpr_document(baseball_births, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "baseball_births", 
             title = "The count of births in the United States from 1926-2014",
             description = "Data obtained from Lahman http://www.seanlahman.com/baseball-archive/statistics/",
             source = "https://github.com/cdalzell/Lahman",
             var_details = data_description)

dpr_readme(usethis::proj_get(), package_name_text, "byuidatascience")

devtools::document(pkg = usethis::proj_get())

dpr_write_script(usethis::proj_get(), r_read = "scripts/data_births.R")

dpr_push(folder_dir = usethis::proj_get(), message = "'Second Push from Hathaway'", repo_url = NULL)


