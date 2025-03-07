%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_param_values).

-include_lib("igor/include/igor_http.hrl").

-behaviour(cowboy_handler).

-export([
    init/2
]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"PUT">>, Req) ->
    case cowboy_req:has_body(Req) of
        true -> handle_put(Req);
        false -> cowboy_req:reply(400, Req)
    end;
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"PUT">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

handle_put(Req) ->
    try
        {ok, RequestBody, Req1} = cowboy_req:read_body(Req),
        Request = jsx:decode(RequestBody, [return_maps]),
        Project = cowboy_req:binding(project, Req1),
        Database = cowboy_req:binding(database, Req1),
        Param = cowboy_req:binding(param, Req1),
        Response = web_rest_callback_parameters:retrieve_parameter_values(Request, Project, Database, Param),
        Body = jsx:encode(data_protocol:collection_to_json(Response, {custom, fun data_protocol:json_point_to_json/1})),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req1)
    catch
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

