PROJECT = rock_util
PROJECT_DESCRIPTION = RedRock Util
PROJECT_VERSION = 1.2

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +'{lager_truncation_size, 512000}'
ERLC_OPTS += +'{lager_extra_sinks, [much,much0,much1,much2,much3,much4]}'
DEPS += lager
dep_lager = git https://github.com/basho/lager master
DEPS += recon
dep_recon = git https://github.com/ferd/recon master
include erlang.mk

