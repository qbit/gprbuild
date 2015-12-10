with cma_1;
--with pack_a; -- apps package
with ada.text_io;


package body comp_b_1 is

   procedure proc_comp_b_1 is
   begin
      ada.text_io.Put_Line("in comp_b_1.proc_comp_b_1");
      cma_1.proc_cma_1;
      --pack_a.proc_a;
      --pack_a.proc_a_del;
   end proc_comp_b_1;

end comp_b_1;

