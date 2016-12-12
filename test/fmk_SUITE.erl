-module(fmk_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% common_test callbacks
-export([%% suite/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0]).

%% tests
-export([
  test_create_prescription/1,
  test_get_pharmacy_prescription/1,
  test_get_staff_prescriptions/1
]).

all() -> [
  test_create_prescription,
  test_get_pharmacy_prescription,
  test_get_staff_prescriptions
].

init_per_suite(Config) ->
  %application:start(fmk),
  %application:ensure_all_started([fmk]),
  Pid = 0,


  %% Seed random number
  rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
  BaseId = 100000000 + 100 * (rand:uniform(100000)),

  test_setup(BaseId),

  hackney:start(),

  ConnRef = connect(),
  ct:pal("Connection = ~p~n", [ConnRef]),

  [{fmk_pid, Pid}, {hackney, ConnRef}, {baseid, BaseId} | Config].

connect() ->
  Transport = hackney_tcp,
  Host = <<"localhost">>,
  Port = 9090,
  Options = [{pool, default}],
  {ok, ConnRef} = hackney:connect(Transport, Host, Port, Options),
  ConnRef.

end_per_suite(Config) ->
%%  fmk_app:stop(ignore),
%%  Pid = proplists:get_value(fmk_pid, Config),
%%  exit(Pid, kill),
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.




test_create_prescription(Conf) ->
  BaseId = proplists:get_value(baseid, Conf),
  Data = [
    {id, BaseId},
    {facility_id, BaseId},
    {patient_id, BaseId},
    {pharmacy_id, BaseId},
    {prescriber_id, BaseId},
    {drugs, "aspirin,paracetamol"},
    {date_prescribed, "today"}
  ],
  Response = post_request("prescriptions", Data),
  ct:pal("Response = ~p", [Response]),
  ok.

test_get_pharmacy_prescription(Conf) ->
  BaseId = integer_to_list(proplists:get_value(baseid, Conf)),
  Response = get_request("pharmacies/" ++ BaseId ++ "/prescriptions"),
  ct:pal("Response = ~p", [Response]),
  BaseIdBin = list_to_binary(BaseId),
  Expected = [[{<<"prescriptionId">>,<<"prescription_",BaseIdBin/binary>>},
    {<<"prescriptionFacilityId">>,BaseIdBin},
    {<<"prescriptionPatientId">>,BaseIdBin},
    {<<"prescriptionPharmacyId">>,BaseIdBin},
    {<<"prescriptionPrescriberId">>,BaseIdBin},
    {<<"prescriptionDrugs">>,[<<"aspirin">>,<<"paracetamol">>]},
    {<<"prescriptionIsProcessed">>,<<"prescription_not_processed">>},
    {<<"prescriptionDatePrescribed">>,<<"today">>},
    {<<"prescriptionDateProcessed">>,<<"not_found">>}]]
  ,
  ?assertEqual(Expected, Response),

  ok.

test_get_staff_prescriptions(Conf) ->
  BaseId = integer_to_list(proplists:get_value(baseid, Conf)),
    Response = get_request("staff/" ++ BaseId ++ "/prescriptions"),
    ct:pal("Response = ~p", [Response]),
    BaseIdBin = list_to_binary(BaseId),
    Expected = [[{<<"prescriptionId">>,<<"prescription_",BaseIdBin/binary>>},
      {<<"prescriptionFacilityId">>,BaseIdBin},
      {<<"prescriptionPatientId">>,BaseIdBin},
      {<<"prescriptionPharmacyId">>,BaseIdBin},
      {<<"prescriptionPrescriberId">>,BaseIdBin},
      {<<"prescriptionDrugs">>,[<<"aspirin">>,<<"paracetamol">>]},
      {<<"prescriptionIsProcessed">>,<<"prescription_not_processed">>},
      {<<"prescriptionDatePrescribed">>,<<"today">>},
      {<<"prescriptionDateProcessed">>,<<"not_found">>}]]
    ,
    ?assertEqual(Expected, Response),

    ok.


test_get_processed_pharmacy_prescriptions(Conf) ->
  BaseId = integer_to_list(proplists:get_value(baseid, Conf)),
  Response = get_request("pharmacies/" ++ BaseId ++ "/processed_prescriptions"),
  ct:pal("Response = ~p", [Response]),
  BaseIdBin = list_to_binary(BaseId),
  Expected = [[{<<"prescriptionId">>,<<"prescription_",BaseIdBin/binary>>},
    {<<"prescriptionFacilityId">>,BaseIdBin},
    {<<"prescriptionPatientId">>,BaseIdBin},
    {<<"prescriptionPharmacyId">>,BaseIdBin},
    {<<"prescriptionPrescriberId">>,BaseIdBin},
    {<<"prescriptionDrugs">>,[<<"aspirin">>,<<"paracetamol">>]},
    {<<"prescriptionIsProcessed">>,<<"prescription_not_processed">>},
    {<<"prescriptionDatePrescribed">>,<<"today">>},
    {<<"prescriptionDateProcessed">>,<<"not_found">>}]]
  ,
  ?assertEqual(Expected, Response),

  ok.


test_get_patient(_Conf) ->
  Patient = get_request("patients/1"),
  ct:pal("Pattient = ~p~n", [Patient]),
  ok.

get_request(Path) ->
  request(get, Path, <<>>).

post_request(Path, Data) ->
  request(post, Path, jsx:encode(Data)).

put_request(Path, Data) ->
  request(put, Path, jsx:encode(Data)).

request(Method, Path, Payload) ->
  % request(Method, #hackney_url{}=URL0, Headers, Body, Options0)
  URL = list_to_binary("http://localhost:9090/" ++ Path),
  Headers = [{<<"Connection">>, <<"keep-alive">>}],

  Headers = [{<<"Connection">>, <<"keep-alive">>}],
  Req = {Method, URL, Headers, Payload},

  case hackney:send_request(connect(), Req) of
    {ok, 200, _RespHeaders, Ref} ->
      {ok, ResponseBody} = hackney:body(Ref),
      Json = decode_json(ResponseBody),
      case proplists:get_value(<<"success">>, Json) of
        true ->
          proplists:get_value(<<"result">>, Json);
        _ ->
          erlang:error(proplists:get_value(<<"result">>, Json))
      end;
    {ok, Status, _RespHeaders, Ref} ->
      {ok, ResponseBody} = hackney:body(Ref),
      erlang:error({request_failed, Status, ResponseBody});
    Err ->
      erlang:error({error, Err})
  end.


decode_json(Body) ->
  try
    jsx:decode(Body)
  catch
    error:Err ->
      io:format("JSON error ~p~nfor JSON:~n~p~n~p~n~n~n", [Err, Body, erlang:get_stacktrace()]),
      []
  end.



test_setup(BaseId) ->

  net_kernel:start(['ct@127.0.0.1', longnames]),
  erlang:set_cookie(node(), antidote),
  FmkNode = 'fmk@127.0.0.1',
  case net_adm:ping(FmkNode) of
    pang ->
      error("cannot connect to fmk.\n");
    pong ->
      ok
  end,
  run_op(FmkNode, create_facility, [BaseId, "Amager Hospital", "Amager Island, DK", "Hospital"]),
  run_op(FmkNode, create_pharmacy, [BaseId, "Chai Pharmacy", "Costa da Caparica, Portugal"]),
  run_op(FmkNode, create_patient, [BaseId, "Phineas Gage", "New Hampshire, United States"]),
  run_op(FmkNode, create_staff, [BaseId, "Alexander Fleming", "London, UK", "Pharmacologist"]),
  ok.


run_op(FmkNode, create_pharmacy, Params) ->
  [_Id, _Name, _Address] = Params,
  run_rpc_op(FmkNode, create_pharmacy, Params);
run_op(FmkNode, create_facility, Params) ->
  [_Id, _Name, _Address, _Type] = Params,
  run_rpc_op(FmkNode, create_facility, Params);
run_op(FmkNode, create_patient, Params) ->
  [_Id, _Name, _Address] = Params,
  run_rpc_op(FmkNode, create_patient, Params);
run_op(FmkNode, create_staff, Params) ->
  [_Id, _Name, _Address, _Speciality] = Params,
  run_rpc_op(FmkNode, create_staff, Params);
run_op(FmkNode, create_prescription, Params) ->
  [_PrescriptionId, _PatientId, _PrescriberId, _PharmacyId, _FacilityId, _DatePrescribed, _Drugs] = Params,
  run_rpc_op(FmkNode, create_prescription, Params).

run_rpc_op(FmkNode, Op, Params) ->
  ok =
    case rpc:call(FmkNode, fmk_core, Op, Params) of
      {error, Reason} ->
        error("Error ~p in ~p with params ~p\n", [Reason, Op, Params]);
      ok -> ok
    end.