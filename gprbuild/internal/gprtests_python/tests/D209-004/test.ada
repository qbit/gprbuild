with test1;
with ada.text_io;
procedure test is
   procedure test_cpp;
   pragma import(c,test_cpp,"test_cpp");
begin
   ada.text_io.put_line("test - before test_cpp");
   test_cpp;
   ada.text_io.put_line("test - after test_cpp");
end;
