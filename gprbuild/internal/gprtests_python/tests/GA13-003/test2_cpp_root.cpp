#include "test2_cpp_root.h"

Test2_Common_root::Test2_Common_root()
{
  std::cout <<"CPP:Test2_Common_Root::constructor called" << std::endl;
  state = 0;
}

void Test2_Common_root::execute()
{
  std::cout <<"CPP:Test2_Common_Root::execute() called" << std::endl;
}
