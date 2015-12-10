set libcdir=%CD%
cd ..\..\
CALL set_root.bat
cd %libcdir%
gprbuild -Plibc.gpr -d -XPRCROOT=%EIU_PRC_ROOT_PATH% --config=%EIU_PRC_ROOT_PATH%\powerpc-elf.cgpr
