#include <iostream>
#include "test2_cpp_root.h"

using namespace std;

extern "C"
{
  void adainit (void);
  void adafinal (void);
  Test2_Common_root* create2_cpp_model();
  Test2_Common_root* create2_ada_model();
}

int test2_call_dispatcher()
{
   std::cout << "test2_cpp_main: creating cpp_model"
             << std::endl;
   Test2_Common_root* cpp_model= (*create2_cpp_model)();

   if(cpp_model == 0) {
      std::cout << "test2_cpp_main:  create_cpp_model failed" << std::endl;
      return -1;
   }
   std::cout << "test2_cpp_main: cpp_model created "
//           << ", addr = "
//           << cpp_model
             << std::endl
             << std::endl;

   std::cout << "test2_cpp_main: creating ada_model" << std::endl;
   Test2_Common_root* ada_model = (*create2_ada_model)();

   if(ada_model == 0) {
      std::cout << "test2_cpp_main:  create_ada_model failed" << std::endl;
      return -1;
   }
   std::cout << "test2_cpp_main: ada_model created"
//           << ", addr = "
//           << ada_model
             << std::endl
             << std::endl;
   
   std::cout << "test2_cpp_main: executing models by state" << std::endl;
   for(int i=1; i<=6; ++i) {
      std::cout << std::endl << "CPP:test2_cpp_main: State = " << i << std::endl;
      cpp_model->state = i;
      cpp_model->execute();

      std::cout << "Ada:test2_cpp_main: State = " << i << std::endl;
      ada_model->state = i;
      ada_model->execute();
   }
   return 0;
}

int main()
{
 std::cout << "--- test2_cpp_main: called" << std::endl;
 adainit ();
 test2_call_dispatcher();
 adafinal ();
 std::cout << "--- test2_cpp_main: returning" << std::endl;
 return 0;
}
