pragma Ada_05;

--
--  From log/log_common.h
--


package RTI.Log is
   pragma Pure;
   type RTILogBitmap is mod 2 ** 32;

   BIT_SILENCE     : constant RTILogBitmap := 2#0000_0000_0000_0000#;
   BIT_EXCEPTION   : constant RTILogBitmap := 2#0000_0000_0000_0001#;
   BIT_WARN        : constant RTILogBitmap := 2#0000_0000_0000_0010#;
   BIT_LOCAL       : constant RTILogBitmap := 2#0000_0000_0000_0100#;
   BIT_REMOTE      : constant RTILogBitmap := 2#0000_0000_0000_1000#;
   BIT_TIMESTAMP   : constant RTILogBitmap := 2#0000_0000_0001_0000#;
   BIT_PERIODIC    : constant RTILogBitmap := 2#0000_0000_0010_0000#;
   BIT_CONTENT     : constant RTILogBitmap := 2#0000_0000_0100_0000#;
   BIT_OTHER       : constant RTILogBitmap := 2#0000_0000_1000_0000#;
   BIT_WORKER_STAT : constant RTILogBitmap := 2#0000_0001_0000_0000#;
   BITMAP_DEFAULT  : constant RTILogBitmap := BIT_EXCEPTION;

end RTI.Log;
