///
/// @author S. S. Kapur     s.s.kapur@lmco.com
//
#ifndef CPP_MODEL_H
#define CPP_MODEL_H

#include <iomanip>
#include <iostream>
#include "test4_cpp_model_base.h"

//////////////////////////////////////////////////////////////////////////////
/// Defines the base class that all suite model base classes must derive from.
//////////////////////////////////////////////////////////////////////////////

class Test4_Cpp_model:Test4_Cpp_model_base
{
public:
	/// model specific implementation of initialize state
	void do_initialize();

	/// model specific implementation of stabilize state
	void do_stabilize();

	/// model specific implementation of freeze state
	void do_freeze();

	/// model specific implementation of run state
	void do_run();

	/// model specific implementation of end_of_exercise state
	void do_end_of_exercise();

	/// model specific implementation of shutdown state
	void do_shutdown();

	/// contructor
	Test4_Cpp_model();

	/// destructor
	~Test4_Cpp_model();
};
#endif // CPP_MODEL_H

