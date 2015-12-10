#include <iostream>
#include "test4_cpp_root.h"
//#include <string>

using namespace std;

extern "C"
{
  void adainit (void);
  void adafinal (void);
  Test4_Common_root* create_cpp_model();
  Test4_Common_root* create4_ada_model();
}
// static const string CPP_MODELS_LIB = "libmodels.so";
// static const string ADA_MODELS_LIB = "libmodels.so";

int test4_call_dispatcher()
{
   typedef Test4_Common_root* (*Model_create_fn)();

   // std::cout << "cpp_main: loading libcpp_models.so" << std::endl;
   // void* cpp_lib = dlopen(CPP_MODELS_LIB.c_str(), RTLD_NOW);
   // if(cpp_lib == 0)
   // {
      // std::cout << "cpp_main: cpp dlopen failed" << std::endl;
      // return -1;
   // }
   // std::cout << "cpp_main: finding create_cpp_model" << std::endl;
   // Model_create_fn create_cpp_model = (Model_create_fn)dlsym(cpp_lib, "create_cpp_model");
   // if(create_cpp_model == 0)
   // {
      // std::cout << "cpp_main: cpp dlsym failed" << std::endl;
      // return -1;
   // }
   std::cout << "test4_cpp_main: creating cpp_model" << std::endl;
   Test4_Common_root* cpp_model= (*create_cpp_model)();
   if(cpp_model == 0)
   {
      std::cout << "test4_cpp_main:  create_cpp_model failed" << std::endl;
      return -1;
   }
   std::cout << "test4_cpp_main: cpp_model created"
//             << ", addr = " << cpp_model
             << std::endl;
   
   // std::cout << "cpp_main: loading libada_models.so" << std::endl;
   // void* ada_lib = dlopen(ADA_MODELS_LIB.c_str(), RTLD_NOW);
   // if(ada_lib == 0)
   // {
      // std::cout << "cpp_main: ada dlopen failed" << std::endl;
      // return -1;
   // }
   // std::cout << "cpp_main: finding create_ada_model" << std::endl;
   // Model_create_fn create4_ada_model = (Model_create_fn)dlsym(ada_lib, "create_ada_model");
   // if(create4_ada_model == 0)
   // {
      // std::cout << "cpp_main: ada dlsym failed" << std::endl;
      // return -1;
   // }
   std::cout << "test4_cpp_main: creating ada_model" << std::endl;
   Test4_Common_root* ada_model = (*create4_ada_model)();
   if(ada_model == 0)
   {
      std::cout << "test4_cpp_main:  create_ada_model failed" << std::endl;
      return -1;
   }
   std::cout << "test4_cpp_main: ada_model created"
//           << ", addr = " << ada_model
             << std::endl;
   
   std::cout << "test4_cpp_main: executing models by state" << std::endl;
   for(int i=1; i<=6; ++i)
   {
      std::cout << std::endl << "test4_cpp_main: State = " << i << std::endl;
      cpp_model->state = i;
      cpp_model->execute();
      ada_model->state = i;
      ada_model->execute();
   }
   return 0;
}

int main()
{
 std::cout << "test4_cpp_main: called" << std::endl;
 adainit ();
 test4_call_dispatcher();
 adafinal ();
 std::cout << "test4_cpp_main: returning" << std::endl;
 return 0;
}
