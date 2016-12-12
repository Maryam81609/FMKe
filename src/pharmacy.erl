%% This module represents the pharmacy entity in the FMK system.
%% Pharmacies have associated prescriptions.
-module(pharmacy).
-include("fmk.hrl").

%% Functions to handle single Pharmacy objects
-export([
  new/3,
  name/1,
  id/1,
  address/1,
  prescriptions/1,
  update_details/2,
  add_prescription/2,
  process_prescription/2,
  add_prescription_drugs/2
  , prescriptions_key/1, prescriptions_processed_key/1]).

%% Returns a list of operations ready to be inserted into antidote.
%% All Ids must be of type pos_integer() and name and address should be binary()
-spec new(id(), string(), string()) -> [term()].
new(Id, Name, Address) ->
  IdOp = build_id_op(?PHARMACY_ID, ?PHARMACY_ID_CRDT, Id),
  NameOp = build_lwwreg_op(?PHARMACY_NAME, ?PHARMACY_NAME_CRDT, Name),
  AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS, ?PHARMACY_ADDRESS_CRDT, Address),
  [IdOp, NameOp, AddressOp].

%% Returns a list of operations ready to be inserted into antidote, with the purpose
%% of updating a specific pharmacy's details.
-spec update_details(string(), string()) -> [term()].
update_details(Name, Address) ->
  NameOp = build_lwwreg_op(?PHARMACY_NAME, ?PHARMACY_NAME_CRDT, Name),
  AddressOp = build_lwwreg_op(?PHARMACY_ADDRESS, ?PHARMACY_ADDRESS_CRDT, Address),
  [NameOp, AddressOp].

%% Returns the name of the pharmacy in the form of a list.
-spec name(crdt()) -> string().
name(Pharmacy) ->
  binary_to_list(antidote_lib:find_key(Pharmacy, ?PHARMACY_NAME, ?PHARMACY_NAME_CRDT)).

%% Returns the ID of the pharmacy in the form of an integer.
-spec id(crdt()) -> id().
id(Pharmacy) ->
  antidote_lib:find_key(Pharmacy, ?PHARMACY_ID, ?PHARMACY_ID_CRDT).

%% Returns the address of the pharmacy in the form of a list.
-spec address(crdt()) -> string().
address(Pharmacy) ->
  binary_to_list(antidote_lib:find_key(Pharmacy, ?PHARMACY_ADDRESS, ?PHARMACY_ADDRESS_CRDT)).

%% Returns a orset of prescriptions keys (index) associated with this pharmacy object.
-spec prescriptions(crdt()) -> term().
prescriptions(Pharmacy) ->
  antidote_lib:find_key(Pharmacy, ?PHARMACY_PRESCRIPTIONS, ?PHARMACY_PRESCRIPTIONS_CRDT).

%% Returns an update operation for adding a prescription to a specific pharmacy.
-spec add_prescription(binary(), binary()) -> antidote_lib:update().
add_prescription(PharmacyId, PrescriptionKey) ->
  {prescriptions_key(PharmacyId), add, PrescriptionKey}.

prescriptions_key(PharmacyId) ->
  antidote_lib:create_bucket(<<PharmacyId/binary, "_prescriptions">>, antidote_crdt_orset).

prescriptions_processed_key(PharmacyId) ->
  antidote_lib:create_bucket(<<PharmacyId/binary, "_prescriptions_processed">>, antidote_crdt_orset).


-spec process_prescription(binary(), binary()) -> [antidote_lib:update()].
process_prescription(PharmacyKey, PrescriptionKey) ->
  [
    {prescriptions_key(PharmacyKey), remove, PrescriptionKey},
    {prescriptions_processed_key(PharmacyKey), add, PrescriptionKey}
  ].

-spec add_prescription_drugs(id(), [string()]) -> [term()].
add_prescription_drugs(PrescriptionId, Drugs) ->
  PrescriptionUpdate = prescription:add_drugs(Drugs),
  %% now to insert the nested operations inside the prescriptions map
  PharmacyPrescriptionsKey = fmk_core:binary_prescription_key(PrescriptionId),
  %% return a top level patient update that contains the prescriptions map update
  PharmacyPrescriptionsOp = antidote_lib:build_nested_map_op(?PHARMACY_PRESCRIPTIONS, ?NESTED_MAP,
    PharmacyPrescriptionsKey, PrescriptionUpdate),
  [PharmacyPrescriptionsOp].

%%-----------------------------------------------------------------------------
%% Internal auxiliary functions - simplifying calls to external modules
%%-----------------------------------------------------------------------------
build_id_op(Key, KeyType, Id) ->
  build_lwwreg_op(Key, KeyType, integer_to_list(Id)).

build_lwwreg_op(Key, KeyType, Value) ->
  antidote_lib:build_map_op(Key, KeyType, antidote_lib:lwwreg_assign(Value)).
