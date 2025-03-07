%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.4
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_patch_notes).

-include_lib("igor/include/igor_http.hrl").

-behaviour(cowboy_handler).

-export([
    init/2
]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"GET">>, Req) ->
    handle_get(Req);
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"GET">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

handle_get(Req) ->
    try
        Project = cowboy_req:binding(project, Req),
        Response = web_rest_callback_patch_notes:get_patch_notes(Project),
        Body = jsx:encode(data_protocol:collection_to_json(Response, {custom, fun card_protocol:card_patch_note_to_json/1})),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req)
    catch
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

