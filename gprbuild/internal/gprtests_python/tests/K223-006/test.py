from gprbuild_utils import *

def grep_auto_cgpr(pattern):
    with open("auto.cgpr") as f:
        for line in f:
            if pattern in line:
                print line

print "adiru_fit sjlj"
gprbuild ("-f -XPLATFORM=native-full -P adiru_fit.gpr -q", verbose=True)
grep_auto_cgpr("RTS=")
print "adiru_fit zfp"
gprbuild ("-f -XPLATFORM=native-zfp -P adiru_fit.gpr -q", verbose=True)
grep_auto_cgpr("RTS=")
print "prj sjlj"
gprbuild ("-f -q prj.gpr", verbose=True)
grep_auto_cgpr("RTS=")
print "two different --RTS"
gprbuild ("-q -f --RTS=sjlj --RTS:Ada= prj.gpr", verbose=True)
print "two similar --RTS"
gprbuild ("-q -f --RTS=sjlj --RTS:Ada=sjlj prj.gpr", verbose=True)
grep_auto_cgpr("RTS=")
print "prj default"
gprbuild ("-q -f --RTS:Ada= prj.gpr", verbose=True)
grep_auto_cgpr("RTS=")
print "--RTS without auto-configuration"
gprbuild ("-f -q --RTS=zfp --config=auto.cgpr prj.gpr", verbose=True)

