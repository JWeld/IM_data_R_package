# The default version is "latest", which is resolved against the repository.
# The suite runs against the bundled release instead, so a default `version`
# argument never goes to the network. Tests of the "latest" default set the
# option themselves and mock the repository.
withr::local_options(icpim.version = IM_BUNDLED_VERSION,
                     .local_envir = teardown_env())
