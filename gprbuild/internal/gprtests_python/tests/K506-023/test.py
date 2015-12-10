from gprbuild_utils import *

print "-----"
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
sleep (3.5)
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
sleep (3.5)
touch ("test.adb");
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
sleep (3.5)
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
sleep (3.5)
touch ("test.ads");
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
sleep (3.5)
machin = Run (['gprbuild', "-ws", 'test.gpr', '--config=gnat2why.cgpr'])
print machin.out
print "-----"
