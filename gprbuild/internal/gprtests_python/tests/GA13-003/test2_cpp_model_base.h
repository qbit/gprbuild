///
/// @author S. S. Kapur     s.s.kapur@lmco.com
//
#ifndef CPP_MODEL_BASE_H
#define CPP_MODEL_BASE_H

#include <iomanip>
#include <iostream>
#include "test2_cpp_root.h"

enum Model_states
{
    initialize_state = 1,
    stabilize_state = 2,
    freeze_state = 3,
    run_state = 4,
    end_of_exercise_state = 5,
    shutdown_state = 6
};

class Test2_Cpp_model_base : Test2_Common_root
{
public:
   //  Cpp_model_base();

   /// Over-ride with model specific implementation of initialize state
   virtual void do_initialize();

   /// Over-ride with model specific implementation of stablize state
   virtual void do_stabilize();

   /// Over-ride with model specific implementation of freeze state
   virtual void do_freeze();

   /// Over-ride with model specific implementation of run state
   virtual void do_run();

   /// Over-ride with model specific implementation of end_of_exercise state
   virtual void do_end_of_exercise();

   /// Over-ride with model specific implementation of shutdown state
   virtual void do_shutdown();

   /// 'suite' models must translate state to a function entry
   virtual void execute();

   /// Get the suite state name for any suite state id known by a suite of models
   const std::string get_state_name()
   {
      switch(state)
      {
         case initialize_state:
         {
            return "INITIALIZE";
         }

         case stabilize_state:
         {
            return "STABILIZE";
         }

         case freeze_state:
         {
            return "FREEZE";
         }

         case run_state:
         {
            return "RUN";
         }

         case end_of_exercise_state:
         {
            return "END_OF_EXERCISE";
         }

         case shutdown_state:
         {
            return "SHUTDOWN";
         }
      }
   }

};
#endif // CPP_MODEL_BASE_H

