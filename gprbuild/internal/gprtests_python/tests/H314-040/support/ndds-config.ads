pragma Ada_05;

with RTI.Log;

package NDDS.Config is
   pragma Preelaborate;
   use RTI.Log;

   type LOG_CATEGORY is (PLATFORM, COMMUNICATION, DATABASE, ENTITIES, API);
   for LOG_CATEGORY'Object_Size use 32;

   subtype Log_Verbosity is RTI.Log.RTILogBitmap;

   LOG_VERBOSITY_SILENT        : constant Log_Verbosity := RTI.Log.BIT_SILENCE;
   LOG_VERBOSITY_ERROR         : constant Log_Verbosity := RTI.Log.BIT_EXCEPTION;
   LOG_VERBOSITY_WARNING       : constant Log_Verbosity := LOG_VERBOSITY_ERROR or RTI.Log.BIT_EXCEPTION;
   LOG_VERBOSITY_STATUS_LOCAL  : constant Log_Verbosity := LOG_VERBOSITY_WARNING or RTI.Log.BIT_LOCAL;
   LOG_VERBOSITY_STATUS_REMOTE : constant Log_Verbosity := LOG_VERBOSITY_STATUS_LOCAL or RTI.Log.BIT_REMOTE;
   LOG_VERBOSITY_ALL           : constant Log_Verbosity := LOG_VERBOSITY_STATUS_REMOTE or RTI.Log.BIT_PERIODIC;

end NDDS.Config;
