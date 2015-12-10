pragma Ada_05;

package DDS.Listener is

   -- Base class for all listeners
   type Ref is limited interface;
   type Ref_Access is access all Ref'Class;

end DDS.Listener;
