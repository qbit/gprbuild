package Some_Package is

  function Some_Subprog return Integer;
  pragma Export (StdCall, Some_Subprog, "sub");

end Some_Package;
