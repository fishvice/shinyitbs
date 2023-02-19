library(tidyverse)

# datadownload and tidy --------------------------------------------------------

if(fs::dir_exists("data-raw") & fs::file_exists("data-raw/nsibts-q3.rds")) {
  stop("This is not an error and you have what it takes")
}

fs::dir_create("data-raw")
library(icesDatras)
years <- 2000:2022
qs <- 3
surveys <- "NS-IBTS"

res <- list()
for(y in 1:length(years)) {
  print(years[y])
  res[[y]] <-
    getCPUELength(surveys, year = years[y], quarter = qs) |>
    as_tibble() |>
    rename_all(tolower) |>
    unite("id", survey, year, quarter, ship, gear, haulno, subarea, remove = FALSE) |>
    select(id, year, lon = shootlon, lat = shootlat, latin = species,
           length = lngtclas, n = cpue_number_per_hour) |>
    mutate(length = as.integer(floor(length / 10))) |>
    group_by(id, year, lon, lat, latin, length) |>
    summarise(n = sum(n),
              .groups = "drop")
}
# res |> write_rds("tmp.rds")
# res <- read_rds("tmp.rds")



# Split the data into station data and length measurement data -----------------
st <-
  res |>
  bind_rows() |>
  select(id, year, lon, lat) |>
  distinct() |>
  group_by(year) |>
  mutate(n.tows = n_distinct(id)) |>
  ungroup()
# Check if we get a unique id:
if(!st |> nrow() == st |> distinct(id, .keep_all = TRUE) |> nrow()) {
  stop("This is unexpected, check the code")
}
st |>
  group_by(id) |>
  mutate(n.id = n()) |>
  filter(n.id > 1) |>
  glimpse()
# This should really be reported to somebody
# remedy for now: drop one of the dual
st <-
  st |>
  group_by(id) |>
  slice(1) |>
  ungroup()

le <-
  res |>
  bind_rows() |>
  select(id, year, lon, lat, latin, length, n) |>
  # get rid of species where cpue is always zero
  group_by(latin) |>
  mutate(n.sum = sum(n)) |>
  ungroup() |>
  filter(n.sum > 0) |>
  select(-n.sum)

# (rbyl) results by year and length --------------------------------------------

## A. First the total caught in each length class each year --------------------
rbyl <-
  le |>
  group_by(year, latin, length) |>
  # the new kid on the block, here summarise returns a warning
  reframe(N = sum(n),                      # total number    by length caught in the year (per 60 minute haul)
          B = sum(n * 0.00001 * length^3)) #       mass [kg]
rbyl <-
  rbyl |>
  # fill in full cm lengths from min to max witin each species
  select(year, latin, length) |> # step not really needed, just added for clarity
  group_by(latin) |>
  expand(year = full_seq(year, 1),
         length = full_seq(length, 1)) |>
  # join back to get the N and B
  left_join(rbyl) |>
  mutate(N = replace_na(N, 0),
         B = replace_na(B, 0))

## B. Now the calculation of the mean per length per year ----------------------
rbyl <-
  rbyl |>
  left_join(st |> count(year, name = "n.tows")) |>
  mutate(n = N / n.tows,
         b = B / n.tows) |>
  select(-n.tows)

# average over the whole survey time period
# results by length, used in the length frequency plot ----------------------b--
rbl <-
  rbyl |>
  group_by(latin, length) |>
  reframe(N = mean(N),
          B = mean(B),
          n = mean(n),
          b = mean(b))

# rbsy --------------------------------------------------------------------------
rbys <-
  le |>
  group_by(id, year, lon, lat, latin) |>
  reframe(N = sum(n),
          B = sum(n * 0.00001 * length^3))
# add zero station
rbys <-
  rbys |>
  expand(nesting(id, year, lon, lat), latin) |>
  # get back N and B
  left_join(rbys) |>
  mutate(N = replace_na(N, 0),
         B = replace_na(B, 0))

my_boot = function(x, times = 100) {

  # Get column name from input object
  var = deparse(substitute(x))
  var = gsub("^\\.\\$","", var)

  # Bootstrap 95% CI
  cis = stats::quantile(replicate(times, mean(sample(x, replace=TRUE))), probs=c(0.025,0.975))

  # Return data frame of results
  data.frame(var, n=length(x), mean=mean(x), lower.ci=cis[1], upper.ci=cis[2])
}

print("Bootstrapping abundance:")

boot.N <-
  rbys |>
  dplyr::group_by(latin, year) %>%
  dplyr::do(my_boot(.$N)) %>%
  dplyr::mutate(variable = "N",
                var = as.character(variable))

print("Bootstrapping biomass:")

boot.B <-
  rbys %>%
  dplyr::group_by(latin, year) %>%
  dplyr::do(my_boot(.$B)) %>%
  dplyr::mutate(variable = "B",
                var = as.character(var))

boot <-
  bind_rows(boot.N,
            boot.B)


latin <- boot$latin |> unique() |> sort()
names(latin) <- latin

library(rnaturalearth)
library(sf)
bb <- st_bbox(c(xmin = -40, ymin = 27, xmax = 40, ymax = 70),
              crs = 4326)
cl <-
  rnaturalearth::ne_countries(scale = 50, continent = "europe", returnclass = "sf") |>
  st_make_valid() |>
  st_crop(bb) |>
  st_coordinates() |>
  as_tibble() |>
  mutate(group = paste(L1, L2, L3)) |>
  select(lon = X, lat = Y, group)

list(rbyl = rbyl, rbl = rbl, rbys = rbys, boot = boot, species = latin, cl = cl) |>
  write_rds("data-raw/nsibts-q3.rds")


