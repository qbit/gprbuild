#include <iomanip>
#include <iostream>
#include "test2_cpp_model_base.h"

void Test2_Cpp_model_base::do_initialize(){};
void Test2_Cpp_model_base::do_stabilize(){};
void Test2_Cpp_model_base::do_freeze(){};
void Test2_Cpp_model_base::do_run(){};
void Test2_Cpp_model_base::do_end_of_exercise(){};
void Test2_Cpp_model_base::do_shutdown(){};

void Test2_Cpp_model_base::execute() {
      std::cout <<"CPP:Test2_Cpp_model_base::Execute called, State = "
                << state << std::endl;

      switch(state) {
         case initialize_state: {
            do_initialize();
         }
         break;

         case stabilize_state: {
            do_stabilize();
         }
         break;

         case freeze_state: {
            do_freeze();
         }
         break;

         case run_state: {
            do_run();
         }
         break;

         case end_of_exercise_state: {
            do_end_of_exercise();
         }
         break;

         case shutdown_state: {
            do_shutdown();
         }
         break;

         default: {
         }
         break;
      }
   };
