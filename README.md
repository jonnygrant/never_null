#never_null
This is derived from CppCoreGuidelines's GSL not_null.
My change makes it at compile time, not runtime.
That means programmers get information about issues when building, rather than at runtime when it calls std::terminate() and quits.
