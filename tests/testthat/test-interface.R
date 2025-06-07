
test_that("database pool connections can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testpool(db))

  }
})

test_that("DB4Patches objects can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db))
  }
})

test_that("Tables can be created by DB4Patches object", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db))
    expect_no_condition(this_db$create_tables())
    expected_tables <- names(create_table_functions)
    created_tables <- DBI::dbListTables(this_db$con)
    for (table in expected_tables) {
      expect_true(table %in% created_tables)
    }
    expect_error(this_db$create_tables()) # tables have already been created
  }
})

test_that("track_exists is FALSE when track_id is not there", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    expect_false(this_db$track_exists("FJDK90w2fskldjfds"))
  }
})

test_that("add_track works, and then track_exists finds it", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    a_track <- kaaterskill$tracks[[1]]
    track_id <- expect_no_error(this_db$add_track(a_track))
    expect_true(this_db$track_exists(track_id))
  }
})

test_that("point_exists is FALSE when no point in range of given coordinates", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    expect_false(this_db$point_exists(-74.080, 42.162))
  }
})

test_that("add_point only requires lat and lon data", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    expect_no_condition(this_db$add_point(-74.080, 42.162))
  }
})

test_that("add_point works, and then point_exists finds it", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    peak_name <- peaks$peak_id[1]
    peak_coords <- sf::st_coordinates(peaks$geometry[1])
    peak_lat <- peak_coords[1,1]
    peak_lon <- peak_coords[1,2]
    peak_elev_m <- 1114
    peak_elev_ft <- 3655
    peak_id <- expect_no_error(this_db$add_point(peak_lat, peak_lon, peak_name,
                                                peak_elev_m, peak_elev_ft))
    expect_true(this_db$point_exists(round(peak_lat, digits=3), round(peak_lon, digits=3)))
  }
})

test_that("add_point, when given one of elev_m or elev_ft, calculates the other", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    peak_name <- peaks$peak_id[1]
    peak_coords <- sf::st_coordinates(peaks$geometry[1])
    peak_lat <- peak_coords[1,1]
    peak_lon <- peak_coords[1,2]
    # peak_elev_m <- 1114
    peak_elev_ft <- 3655
    new_peak_id <- expect_no_error(this_db$add_point(peak_lat, peak_lon,
                                                name=peak_name,
                                                elev_ft=peak_elev_ft))
    this_peak <- this_db$tbl("points") |>
      dplyr::filter(point_id == new_peak_id) |>
      dplyr::collect()
    expect_equal(nrow(this_peak), 1)
    expect_equal(this_peak$point_elev_ft, 3655)
    expect_equal(this_peak$point_elev_m, 1114)

    peak_name <- peaks$peak_id[2]
    peak_coords <- sf::st_coordinates(peaks$geometry[2])
    peak_lat <- peak_coords[1,1]
    peak_lon <- peak_coords[1,2]
    peak_elev_m <- 1114
    # peak_elev_ft <- 3655
    new_peak_id <- expect_no_error(this_db$add_point(peak_lat, peak_lon,
                                                name=peak_name,
                                                elev_m=peak_elev_m))
    this_peak <- this_db$tbl("points") |>
      dplyr::filter(point_id == new_peak_id) |>
      dplyr::collect()
    expect_equal(nrow(this_peak), 1)
    expect_equal(this_peak$point_elev_ft, 3655)
    expect_equal(this_peak$point_elev_m, 1114)
  }
})

test_that("add_point_sfdf works", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    peak_ids <- expect_no_error(this_db$add_point_sfdf(peaks))
    expect_true(all(!is.na(peak_ids)))
    expect_equal(nrow(peaks), length(peak_ids))
  }
})

test_that("add_point_sfdf line failure rolls back full transaction", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    bad_peaks <- peaks
    bad_peaks$geometry[3] <- NA
    peak_ids <- expect_error(this_db$add_point_sfdf(bad_peaks))
    db_table_rows <- this_db$tbl("points") |> dplyr::count() |> dplyr::pull("n")
    expect_equal(db_table_rows, 0)
  }
})

