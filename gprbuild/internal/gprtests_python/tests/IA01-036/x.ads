package X is
   type T is new integer;
   procedure Execute;
   pragma Inline (Execute);
end X;
