pragma Ada_05;

with DDS.Sequences_Generic;
with DDS.Condition; use DDS.Condition;

package DDS.ConditionSeq is new DDS.Sequences_Generic
  (DDS.Condition.Ref_Access,
   DDS.Condition.Ref_Access_Access,
   Natural,
   1,
   DDS.Condition.Ref_Access_Array);

--  old
--  with DDS.AbstractNativeSequence;
--  package DDS.ConditionSeq is
--     type Ref is new DDS.AbstractNativeSequence.Ref with null record;
--  end DDS.ConditionSeq;
