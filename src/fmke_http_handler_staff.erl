-module (fmke_http_handler_staff).
-include ("fmk_http.hrl").

-export([init/2]).

init(Req0, Opts) ->
	try
		Method = cowboy_req:method(Req0),
		HasBody = cowboy_req:has_body(Req0),
		Req = handle_req(Method, HasBody, Req0),
		{ok, Req, Opts}
	catch
		Err:Reason ->
			ErrorMessage = io_lib:format("Error ~p:~n~p~n~p~n", [Err, Reason, erlang:get_stacktrace()]),
			io:format(ErrorMessage),
			Req2 = cowboy_req:reply(500, #{}, ErrorMessage, Req0),
			{ok, Req2, Opts}
	end.

handle_req(<<"POST">>, true, Req) ->
		create_staff(Req);
handle_req(<<"POST">>, false, Req) ->
		cowboy_req:reply(400, #{}, ?ERR_MISSING_BODY, Req);

handle_req(<<"PUT">>, true, Req) ->
		update_staff(Req);
handle_req(<<"PUT">>, false, Req) ->
		cowboy_req:reply(400, #{}, ?ERR_MISSING_BODY, Req);

handle_req(<<"GET">>, true, Req) ->
		cowboy_req:reply(400, #{}, ?ERR_BODY_IN_A_GET_REQUEST, Req);
handle_req(<<"GET">>, false, Req) ->
		get_staff(Req).

create_staff(Req) ->
		{ok, Data, _Req2} = cowboy_req:read_body(Req),
		Json = jsx:decode(Data),
		Id = proplists:get_value(<<"id">>, Json),
		Name = proplists:get_value(<<"name">>, Json),
		Address = proplists:get_value(<<"address">>, Json),
		Speciality = proplists:get_value(<<"speciality">>, Json),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, #{}, ?ERR_INVALID_STAFF_ID, Req);
				false ->
						StringName = binary_to_list(Name),
						StringAddress = binary_to_list(Address),
						StringSpeciality = binary_to_list(Speciality),
						ServerResponse = fmke:create_staff(IntegerId,StringName,StringAddress,StringSpeciality),
						Success = ServerResponse =:= ok,
						JsonReply =	jsx:encode([{success,Success},{result,ServerResponse}]),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

update_staff(Req) ->
		{ok, Data, _Req2} = cowboy_req:read_body(Req),
		Json = jsx:decode(Data),
		Name = proplists:get_value(<<"name">>, Json),
		Address = proplists:get_value(<<"address">>, Json),
		Speciality = proplists:get_value(<<"speciality">>, Json),
		Id = cowboy_req:binding(?BINDING_STAFF_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, #{}, ?ERR_INVALID_STAFF_ID, Req);
				false ->
						StringName = binary_to_list(Name),
						StringAddress = binary_to_list(Address),
            StringSpeciality = binary_to_list(Speciality),
						ServerResponse = fmke:update_staff_details(IntegerId,StringName,StringAddress,StringSpeciality),
						Success = ServerResponse =:= ok,
						JsonReply =	jsx:encode([{success,Success},{result,ServerResponse}]),
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.

get_staff(Req) ->
		Id = cowboy_req:binding(?BINDING_STAFF_ID, Req, -1),
		IntegerId = binary_to_integer(Id),
		case IntegerId =< ?MIN_ID of
				true ->
						cowboy_req:reply(400, #{}, ?ERR_INVALID_STAFF_ID, Req);
				false ->
						ServerResponse = fmke:get_staff_by_id(IntegerId),
						Success = ServerResponse =/= {error,not_found},
						JsonReply = case Success of
								true ->
										jsx:encode([{success,Success},{result,fmke_proplists:encode_object(staff,ServerResponse)}]);
								false ->
										jsx:encode([{success,Success},{result,ServerResponse}])
						end,
						cowboy_req:reply(200, #{
								<<"content-type">> => <<"application/json">>
						}, JsonReply, Req)
		end.
