from gprbuild_utils import *

# Test overriding external values from aggregates.
# Only the values defined in the root aggregate are taken into account.
#   (so the INVALID value defined in childaggr.gpr is always ignored)
# MYHOST="aaa", MODE="DEBUG", VAR2="default" => compiles a.ads

gprbuild(["aggr.gpr"], verbose=True)

# The new value might itself depend on the current scenario
# MYHOST="bbb", MODE="PRODUCTION", VAR2="default" => compiles b.ads

print "----- -XMYHOST=bbb"
gprbuild(["aggr.gpr", "-XMYHOST=bbb"], verbose=True)

# They are overridden by the command line
# MYHOST="aaa", MODE="PRODUCTION" (from cmd line), VAR2=default => compiles b.ads

rm("b.ali")
print "----- -XMODE=PRODUCTION"
gprbuild(["aggr.gpr", "-XMODE=PRODUCTION"], verbose=True)

# The new value can be referenced later in the aggregate project
# MYHOST="ccc", VAR2="set" => no aggregated project

print "----- -XMYHOST=ccc"
gprbuild(["aggr.gpr", "-XMYHOST=ccc"], verbose=True)
