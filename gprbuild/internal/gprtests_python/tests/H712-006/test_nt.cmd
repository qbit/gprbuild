gnatmake -q build_pkgs.adb
build_pkgs
gprbuild_debug -j2 -q main.gpr -p
PATH=%PATH%;lib
main
gprclean -q -r main.gpr
