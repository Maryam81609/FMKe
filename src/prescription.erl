%% This module represents the prescription entity in the FMK system.
%% Prescriptions are associated with patients, treatment facilities, medicall staff
%% and pharmacies.
-module(prescription).
-include("fmk.hrl").

-export([
  id/1,
  new/7,
  facility_id/1,
  patient_id/1,
  pharmacy_id/1,
  prescriber_id/1,
  drugs/1,
  process/2,
  is_processed/1,
  date_prescribed/1,
  date_processed/1,
  add_drugs/1,
  remove_drugs/1
  , key/1]).

%% This function returns a list of operations ready to be inserted into antidote.
%% In order to create an prescription, multiple ids must be supplied (self-explanatory),
%% as well as a date when the treatment was prescribed.
%% All Ids must be of type pos_integer() prescription date (DatePrescribed) should
%% be supplied in binary
-spec new(binary(), id(), id(), id(), id(), string(), [crdt()]) -> antidote_lib:update().
new(Id, PatientId, PrescriberId, PharmacyId, FacilityId, DatePrescribed, Drugs) ->
  IdOp = build_id_op(?PRESCRIPTION_ID, ?PRESCRIPTION_ID_CRDT, Id),
  PatientOp = build_id_op(?PRESCRIPTION_PATIENT_ID, ?PRESCRIPTION_PATIENT_ID_CRDT, PatientId),
  PharmacyOp = build_id_op(?PRESCRIPTION_PHARMACY_ID, ?PRESCRIPTION_PHARMACY_ID_CRDT, PharmacyId),
  PrescriberOp = build_id_op(?PRESCRIPTION_PRESCRIBER_ID, ?PRESCRIPTION_PRESCRIBER_ID_CRDT, PrescriberId),
  FacilityOp = build_id_op(?PRESCRIPTION_FACILITY_ID, ?PRESCRIPTION_FACILITY_ID_CRDT, FacilityId),
  DatePrescribedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PRESCRIBED, ?PRESCRIPTION_DATE_PRESCRIBED_CRDT, DatePrescribed),
  IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED, ?PRESCRIPTION_IS_PROCESSED_CRDT, ?PRESCRIPTION_NOT_PROCESSED),
  [DrugsOp] = add_drugs(Drugs),
  {key(Id), update, [
    IdOp, PatientOp, PharmacyOp, PrescriberOp, FacilityOp, DatePrescribedOp, IsProcessedOp, DrugsOp
  ]}.

key(Id) ->
  antidote_lib:create_bucket(Id, antidote_crdt_gmap).


%% Returns the facility ID from an already existant prescription object.
-spec facility_id(crdt()) -> id().
facility_id(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_FACILITY_ID, ?PRESCRIPTION_FACILITY_ID_CRDT).

%% Returns the pharmacy ID from an already existant prescription object.
-spec pharmacy_id(crdt()) -> id().
pharmacy_id(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_PHARMACY_ID, ?PRESCRIPTION_PHARMACY_ID_CRDT).

%% Returns the prescriber ID from an already existant prescription object.
-spec prescriber_id(crdt()) -> id().
prescriber_id(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_PRESCRIBER_ID, ?PRESCRIPTION_PRESCRIBER_ID_CRDT).

%% Returns the prescription ID from an already existant prescription object.
-spec id(crdt()) -> id().
id(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_ID, ?PRESCRIPTION_ID_CRDT).

%% Returns the patient ID from an already existant prescription object.
-spec patient_id(crdt()) -> id().
patient_id(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_PATIENT_ID, ?PRESCRIPTION_PATIENT_ID_CRDT).

%% Returns the prescription date from an already existant prescription object.
-spec date_prescribed(crdt()) -> string().
date_prescribed(Prescription) ->
  binary_to_list(antidote_lib:find_key(Prescription, ?PRESCRIPTION_DATE_PRESCRIBED, ?PRESCRIPTION_DATE_PRESCRIBED_CRDT)).

%% Returns the processing date from an already existant prescription object.
-spec date_processed(crdt()) -> string().
date_processed(Prescription) ->
  DateProcessed = antidote_lib:find_key(Prescription, ?PRESCRIPTION_DATE_PROCESSED, ?PRESCRIPTION_DATE_PROCESSED_CRDT),
  case DateProcessed of
    not_found ->
      "not_found";
    Date ->
      binary_to_list(Date)
  end.

%% Returns the prescription drugs from an already existant prescription object.
-spec drugs(crdt()) -> [term()].
drugs(Prescription) ->
  antidote_lib:find_key(Prescription, ?PRESCRIPTION_DRUGS, ?PRESCRIPTION_DRUGS_CRDT).

%% Returns the prescription state (if it is processed) from an already existant prescription object.
-spec is_processed(crdt()) -> binary().
is_processed(Prescription) ->
  IsProcessed = antidote_lib:find_key(Prescription, ?PRESCRIPTION_IS_PROCESSED, ?PRESCRIPTION_IS_PROCESSED_CRDT),
  case IsProcessed of
    not_found ->
      <<"not_found">>;
    Result when is_binary(Result) ->
      Result
  end.

%% Returns a list of antidote operations to modify a prescription in order to fill in the processing
%% date and update the prescription date.
-spec process(binary(), string()) -> antidote_lib:update().
process(PrescriptionKey, CurrentDate) ->
  IsProcessedOp = build_lwwreg_op(?PRESCRIPTION_IS_PROCESSED, ?PRESCRIPTION_IS_PROCESSED_CRDT, ?PRESCRIPTION_PROCESSED),
  ProcessedOp = build_lwwreg_op(?PRESCRIPTION_DATE_PROCESSED, ?PRESCRIPTION_DATE_PROCESSED_CRDT, CurrentDate),
  {antidote_lib:create_bucket(PrescriptionKey, antidote_crdt_gmap), update, [IsProcessedOp, ProcessedOp]}.

%% Returns a list of antidote operations to modify a prescription in order to add drugs to a prescription.
-spec add_drugs([term()]) -> [term()].
add_drugs(Drugs) ->
  [antidote_lib:build_map_op(?PRESCRIPTION_DRUGS, ?PRESCRIPTION_DRUGS_CRDT, antidote_lib:set_add_elements(Drugs))].

%% Returns a list of antidote operations to modify a prescription in order to remove drugs from a prescription.
-spec remove_drugs([term()]) -> [term()].
remove_drugs(Drugs) ->
  [antidote_lib:build_map_op(?PRESCRIPTION_DRUGS, ?PRESCRIPTION_DRUGS_CRDT, antidote_lib:set_remove_elements(Drugs))].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key, KeyType, Id) when is_integer(Id) -> % TODO remove this case
  build_lwwreg_op(Key, KeyType, integer_to_binary(Id));
build_id_op(Key, KeyType, Id) ->
  build_lwwreg_op(Key, KeyType, Id).

build_lwwreg_op(Key, KeyType, Value) ->
  antidote_lib:build_map_op(Key, KeyType, antidote_lib:lwwreg_assign(Value)).
