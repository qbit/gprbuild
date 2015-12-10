with ada.text_io;
with libB;
package body libA is
	procedure DummyA is
	begin
	   ada.text_io.put_line( "hello");
	   libB.DummyB;
	end DummyA;
end libA;
