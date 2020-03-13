### Build LED data package for use with Math 119
pacman::p_load(tidyverse, glue, readxl)
pacman::p_load_gh("byuidss/DataPushR")

set.seed(20200313)

## Read in data

### Testing Data

sheet_names <- excel_sheets("../data_led_private/data/DataUsedForAnalysis.xlsx")

dat_list <- sheet_names[c(3,7)] %>% map(~read_xlsx("../data_led_private/data/DataUsedForAnalysis.xlsx", sheet = .x))

led_testing <- dat_list %>%
  map(~pivot_longer(.x, -Hours, names_to = "id", 
                    values_to = "percent_intensity")) %>%
  bind_rows() %>%
  mutate(company = ifelse(str_detect(id, "AAA"), "A", "B"),
         id = parse_number(id)) %>%
  select(id, hours = Hours, percent_intensity, company)
  
led_test_details <- list(id = "An id for each LED light measured",
                         hours = "The number of hours since the first measurement",
                         percent_intensity = "The percent light output based on the first measured intensity of the bulb",
                         company = "Either A or B to represent to different company products")


load("../data_led_private/data/LumenData.Rdata")
# dat object from the .Rdata file



### Study data ####

led_study <- dat %>%
  select(ID, Hours, Intensity, NI) %>%
  rename_all("str_to_lower") %>%
  rename(percent_intensity = ni) %>%
  mutate(intensity = intensity + rnorm(1, mean = 0, sd = .25),
         hours = round(hours) + rnorm(1, mean = 0, sd = .5)) %>%
  group_by(id) %>%
  mutate(percent_intensity = intensity / intensity[1],
         hours = ifelse(hours < 10, 0, hours)) %>%
  ungroup() %>%
  as_tibble()

led_study %>%
  ggplot(aes(x = hours, y = percent_intensity)) +
  geom_line(aes(group = id), color = "grey", alpha = .25) +
  geom_point() +
  theme_bw()
  
led_study %>%
  ggplot(aes(x = hours, y = intensity)) +
  geom_line(aes(group = id), color = "grey", alpha = .25) +
  geom_point() +
  theme_bw()


led_study_details <- list(id = "An id for each LED light measured",
                    hours = "The number of hours since the first measurement",
                    intensity = "The lumen output of the bulb. 800 lumens maps to a 60 watt incandescent bulb (https://www.lumens.com/how-tos-and-advice/light-bulb-facts.html)",
                    percent_intensity = "The percent light output based on the first measured intensity of the bulb")


list_data = list(led_study = led_study, led_testing = led_testing)

#################
#################
#################

#usethis::browse_github_token()

package_name_text <- "data4LED"
base_folder <- glue("../")

github_info <- dpr_create_github(package_name_text, post_text = "/orgs/byuidatascience/repos")
# dpr_delete_github("byuidatascience", package_name_text)

package_path <- dpr_create_package(list_data = list_data, 
                                   package_name = package_name_text, 
                                   export_folder = base_folder, 
                                   git_remote = github_info$clone_url)

usethis::proj_set(package_path)

usethis::use_data(led_study, led_testing)

dpr_document(led_study, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "led_study", title = "LED example bulbs of lumen output",
             description = "An example data set of LED bulbs based on actual data.",
             source = "data_led_private",
             var_details = led_study_details)

dpr_document(led_testing, extension = ".md.R", export_folder = usethis::proj_get(),
             object_name = "led_testing", 
             title = "LED example bulbs of lumen output for two products with standard procedure time point measurements",
             description = "An example data set of LED bulbs based on actual data.",
             source = "data_led_private",
             var_details = led_test_details)

dpr_readme(usethis::proj_get(), package_name_text, "byuidatascience")

devtools::document(pkg = usethis::proj_get())

dpr_write_script(usethis::proj_get(), r_read = "scripts/data_led.R")

dpr_push(folder_dir = usethis::proj_get(), message = "'Second Push from Hathaway'", repo_url = NULL)


