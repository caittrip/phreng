spec <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1,
  max_diameter = 2
)

# constructor test
expect_inherits(spec, "phreng::PH_pointcloud")
expect_inherits(spec, "phreng::PH")
expect_equal(spec@filtration, "vietoris_rips")
expect_equal(spec@engine, "ripserr")
expect_equal(spec@max_dimension, 1)
expect_equal(spec@max_diameter, 2)
expect_equal(spec@max_radius, 1)


# default argument test
spec <- PH_pointcloud()

expect_inherits(spec, "phreng::PH_pointcloud")
expect_equal(spec@engine, "TDA")
expect_true(is.na(spec@library))
expect_equal(spec@max_dimension, 1)
expect_equal(spec@filtration, "vietoris_rips")
expect_true(is.na(spec@max_radius))
expect_true(is.na(spec@max_diameter))

# max_diameter and max_radius test
spec <- PH_pointcloud(max_diameter = 10)

expect_equal(spec@max_diameter, 10)
expect_equal(spec@max_radius, 5)

# max_diameter boundary test
spec <- PH_pointcloud(max_diameter = 0)

expect_equal(spec@max_diameter, 0)
expect_equal(spec@max_radius, 0)

# max_dimension boundary test
spec <- PH_pointcloud(max_dimension = 0)

expect_equal(spec@max_dimension, 0)

# filtration alias tests
spec <- PH_pointcloud(filtration = "alphacomplex")
expect_equal(spec@filtration, "alpha_complex")

spec <- PH_pointcloud(filtration = "alphashape")
expect_equal(spec@filtration, "alpha_shape")

rips_aliases <- c(
  "vietorisrips",
  "vietoris",
  "rips",
  "rips_vietoris",
  "ripsvietoris"
)

for (alias in rips_aliases) {
  spec <- PH_pointcloud(filtration = alias)
  expect_equal(spec@filtration, "vietoris_rips")
}

# library value tests
spec <- PH_pointcloud(library = "GUDHI")
expect_equal(spec@library, "GUDHI")

spec <- PH_pointcloud(library = "PHAT")
expect_equal(spec@library, "PHAT")

spec <- PH_pointcloud(library = "Dionysus")
expect_equal(spec@library, "Dionysus")

# validator tests
expect_error(
  PH_pointcloud(engine = "bad_engine"),
  "TDA"
)
expect_error(
  PH_pointcloud(engine = "bad_engine"),
  "ripserr"
)
expect_error(
  PH_pointcloud(library = "bad_engine"),
  "GUDHI"
)
expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "vietoris_rips"
)

expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "cubical"
)
expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "alpha_shape"
)
expect_error(
  PH_pointcloud(filtration = "bad_filtration"),
  "alpha_complex"
)
expect_error(
  PH_pointcloud(max_dimension = -1),
  "non-negative"
)
expect_error(
  PH_pointcloud(max_dimension = 1.5),
  "integer"
)
expect_error(
  PH_pointcloud(engine = "ripserr", library = "GUDHI"),
  "TDA"
)
expect_error(
  PH_pointcloud(filtration = "alpha_complex", engine = "ripserr"),
  "TDA"
)

# engine value tests
spec <- PH_pointcloud(engine = "TDA")
expect_equal(spec@engine, "TDA")

spec <- PH_pointcloud(engine = "ripserr")
expect_equal(spec@engine, "ripserr")


# data type test
expect_error(
  compute_persistence(spec, as.double(1:5)),
  "matrix"
)
expect_error(
  compute_persistence(spec, as.double(1:5)),
  "array"
)

# compute test
exit_if_not(
  requireNamespace("ripserr", quietly = TRUE),
  requireNamespace("phutil", quietly = TRUE)
)
data <- eurodist
spec <- PH_pointcloud(
  filtration = "vietoris_rips",
  engine = "ripserr",
  max_dimension = 1, 
  max_diameter = 2000
)
out <- compute_persistence(spec, data)
expect_inherits(out, "persistence")
