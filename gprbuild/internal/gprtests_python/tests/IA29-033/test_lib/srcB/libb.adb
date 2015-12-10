with ada.text_io;
with system;
package body libB is
	procedure DummyB is
	begin
	   ada.text_io.put_line( "hello");
	end DummyB;
end libB;
