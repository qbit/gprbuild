package Unit1 is
  Auxiliary_Text_To_Display_C : constant string := "hello world";

  procedure Display_Title;
end Unit1;

package Unit1.Child is
  procedure Display_Auxiliary_Text;
end Unit1.Child;

