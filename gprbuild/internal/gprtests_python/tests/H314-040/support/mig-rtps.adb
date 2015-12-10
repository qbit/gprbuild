pragma Ada_05;
package body Mig.Rtps is


   function Image (This : in Guid) return Standard.String is
   begin
      return This.HostId'Img & "." & This.AppId'Img & "." & This.ObjectId'Img;
   end Image;

end Mig.Rtps;
