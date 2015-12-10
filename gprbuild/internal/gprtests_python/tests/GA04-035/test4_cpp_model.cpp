
#include <iomanip>
#include <ostream>
#include "test4_cpp_model.h"

extern "C"
{
   Test4_Cpp_model* create_cpp_model()
   {
      return new Test4_Cpp_model();
   }
}

Test4_Cpp_model::Test4_Cpp_model()
{
}

Test4_Cpp_model::~Test4_Cpp_model()
{
}

void Test4_Cpp_model::do_initialize()
{
   std::cout <<"Test4_Cpp_model::do_initialize()" << std::endl;
}

void Test4_Cpp_model::do_stabilize()
{
   std::cout <<"Test4_Cpp_model::do_stabilize()" << std::endl;
}

void Test4_Cpp_model::do_freeze()
{
   std::cout <<"Test4_Cpp_model::do_freeze()" << std::endl;
}

void Test4_Cpp_model::do_run()
{
   std::cout <<"Test4_Cpp_model::do_run()" << std::endl;
}

void Test4_Cpp_model::do_end_of_exercise()
{
   std::cout <<"Test4_Cpp_model::do_end_of_exercise()" << std::endl;
}

void Test4_Cpp_model::do_shutdown()
{
   std::cout <<"Test4_Cpp_model::do_shutdown()" << std::endl;
}
