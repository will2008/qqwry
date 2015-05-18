%%%-------------------------------------------------------------------
%%% @author 徐仙华 <will@josenhuas-Mac-mini.local>
%%% @copyright (C) 2015, 徐仙华
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2015 by 徐仙华 <will@josenhuas-Mac-mini.local>
%%%-------------------------------------------------------------------
-module(qqwry).
-author('xuxianhua1985@126.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, start/1, lookup/1, go/0, getProvinceName/1, look/1]).

-define(SERVER, ?MODULE).

-include("qqwry.hrl").

start() ->
    start(app, qqwry).

start(File) ->
    application:load(qqwry),
    application:set_env(qqwry, dbfile, File),
    start().

start(app, App) ->
    start_ok(App, application:start(App, transient)).

start_ok(_, ok) -> ok;
start_ok(_, {error, {already_started, _}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    case start(app, Dep) of
        ok ->
            start(app, App);
        _ ->
            erlang:error({app_start_fail, App})
    end;
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

    
    

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link()->
    start_link(?SERVER, 'qqwry.dat').

start_link(Name, File) ->
    gen_server:start_link({local, Name}, ?MODULE, File, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(DbFile) ->
    case default_db(DbFile) of
        error ->
            {error, {qqwry_db_not_found, DbFile}};
        DbFilePath ->
            case file:read_file(DbFilePath) of
                {ok, Data} ->
                    <<FirstIndex:32/integer-unsigned-little, LastIndex:32/integer-unsigned-little, _/binary>> = Data,
                    Count = (LastIndex - FirstIndex) div 7 + 1,
                    {ok, #qqwry_state{
                            data = Data,
                            length = byte_size(Data),
                            count = Count,
                            position = 0,
                            first_index = FirstIndex,
                            last_index = LastIndex,
                            filename = DbFile
                           }};
                _ ->
                    {error, {qqwry_db_read_error,DbFile}} 
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({lookup, Ip}, _From, #qqwry_state{data=Data, first_index=FirstIndex, count=Count} = State) ->
    IpIndex = find(0, Count, Ip, FirstIndex, Data),
    IpOffset = FirstIndex + IpIndex * 7,
    AddressOffset = getInt3(Data, IpOffset + 4) + 4,
    <<_:AddressOffset/binary, AddressMode:1/binary, _/binary>> = Data,
    Reply = getAddress(AddressMode, Data, AddressOffset),
    {reply, Reply, State};
handle_call(filename, _From, State) ->
    {reply, State#qqwry_state.filename, State};
handle_call(_, _From, State) ->
    getAddress(0, <<0>>,0),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

default_db(File) ->
    AppDir = case code:which(?MODULE) of
                 cover_compiled -> "..";
                 F -> filename:dirname(filename:dirname(F))
             end,
    DbFile = filename:join([AppDir, "priv", erlang:atom_to_list(File)]),
    case filelib:is_file(DbFile) of
        true ->
            DbFile;
        false ->
            error
    end.

lookup(Ip) when is_integer(Ip) ->
    ProccessPid = get_worker(Ip),
    case whereis(ProccessPid) of 
        undefined ->
            35;
        Pid ->
            case gen_server:call(Pid, {lookup, Ip}) of
                {ok, Address, _ISP} ->
                    getProvince(unicode:characters_to_binary(Address));
                _ ->
                    35
            end
    end;
    %% case whereis(qqwry) of
    %%     undefined ->
    %%         case gen_server:call(get_worker(Ip), {lookup, Ip}) of
    %%             {ok, _Address, _ISP} ->
    %%                 getProvince(unicode:characters_to_binary(Address));
    %%             _ ->
    %%                 "未知"
    %%         end;
    %%     Pid ->
    %%         unregister(qqwry),
    %%         register(qqwry_0, Pid),
    %%         FileName = gen_server:call(Pid, filename),
    %%         [qqwry_0|Workers] = qqwry_sup:worker_names(),
    %%         Specs = qqwry_sup:worker(Workers, FileName),
    %%         lists:map(fun(Spec) ->
    %%                           {ok, _Pid} = supervisor:start_child(qqwry_sup, Spec)
    %%                   end, Specs),
    %%         lookup(Ip)
    %% end;
lookup(Ip) ->
    IntIp = ip2long(Ip),
    lookup(IntIp).

look(Ip) ->
    ProvinceCode = lookup(Ip),
    getProvinceName(ProvinceCode).
    

ip2long(Ip) ->
    [A,B,C,D] = lists:map(fun erlang:list_to_integer/1, string:tokens(Ip, ".")),
    <<E:32/integer-big>> = <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>>,
    E.

%% long2ip(Ip) ->
%%     <<A:8/integer, B:8/integer, C:8/integer, D:8/integer>> = <<Ip:32/integer>>,
%%     string:join(lists:map(fun erlang:integer_to_list/1, [A,B,C,D]), ".").

find(Bi, Ei, Ipint, FirstIndex, Data) ->
    case Ei - Bi of
        V when V =< 1 ->
            Bi;
        _ ->
            Mi = (Bi + Ei) div 2,
            Offset = FirstIndex + Mi * 7,
            <<_:Offset/binary, Index:32/integer-unsigned-little, _/binary>> = Data,
            if 
                Ipint =< Index ->
                    find(Bi, Mi, Ipint, FirstIndex, Data);
                true ->
                    find(Mi, Ei, Ipint, FirstIndex, Data)
            end
    end.

getInt3(Data, Offset) ->
    <<_:Offset/binary, Address:24/unsigned-integer-little, _/binary>> = Data,
    Address.

getAddress(<<16#01>>, Data, Offset) ->
    %% 国家记录地址
    AddressOffset = getInt3(Data, Offset + 1),
    <<_:AddressOffset/binary, CountryMode:1/binary, _/binary>> = Data,
    {CountryName, ISPOffset} = case CountryMode of
                                   <<16#02>> ->
                                       {Country, _} = getString(Data, getInt3(Data, AddressOffset + 1)),
                                       Country1 = case Country of
                                                      no_found ->
                                                          "海外";
                                                      _ ->
                                                          Country
                                                  end,
                                       {Country1, AddressOffset + 4};
                                   _ ->
                                       case getString(Data, AddressOffset) of
                                           {no_found, NextOffset} ->
                                               {"海外", NextOffset};
                                           {Country, NextOffset} ->
                                               {Country, NextOffset}
                                       end
                               end,
    ISPName = getISP(Data, ISPOffset),
    {ok, CountryName, ISPName};
getAddress(<<16#02>>, Data, Offset) ->
    CountryName = case getString(Data, getInt3(Data, Offset + 1)) of
                      {no_found, _} ->
                          "海外";
                      {Country, _} ->
                          Country;
                      _ ->
                          "海外"
                  end,
    ISPName = getISP(Data, Offset + 4),
    {ok, CountryName, ISPName};
getAddress(_, Data, Offset) ->
    {CountryName, ISPOffset} = case getString(Data, Offset) of
                                   {no_found, ISPAddress} ->
                                       {"海外", ISPAddress};
                                   R ->
                                       R
                               end,
    ISPName = getISP(Data, ISPOffset),
    {ok, CountryName, ISPName}.


getISP(Data, Offset) ->
    <<_:Offset/binary, ISPMode:1/binary, _/binary>> = Data,
    case ISPMode of
        <<16#01>> ->
            ISPAddress = getInt3(Data, Offset + 1),
            case ISPAddress of
                0 ->
                    "未知";
                _ ->
                    case getString(Data, ISPAddress) of
                        {no_found, _} ->
                            "未知";
                        {Name, _} ->
                            Name;
                        _  ->
                            "未知"
                    end
            end;
        <<16#02>> ->
            ISPAddress = getInt3(Data, Offset + 1),
            case ISPAddress of
                0 ->
                    "未知";
                _ ->
                    case getString(Data, ISPAddress) of
                        {no_found, _} ->
                            "未知";
                        {Name, _} ->
                            Name;
                        _  ->
                            "未知"
                    end
            end;
        _  ->
            case getString(Data, Offset) of
                {no_found, _} ->
                    "未知";
                {Name, _} ->
                    Name;
                _  ->
                    "未知"
            end
    end.

getString(Data, Offset) ->
    getString(Data, Offset, 0).


getString(Data, Offset, L) ->
    RelateOffset = Offset + L,
    <<_:RelateOffset/binary, Char:8/integer, _/binary>> = Data,
    case Char of 
        0 ->
            if 
                L > 0 ->
                    <<_:Offset/binary, Address:L/binary, _/binary>> = Data,
                    {gbk:decode(binary_to_list(Address)), RelateOffset + 1};
                true ->
                    {no_found, RelateOffset + 1}
            end;
        _ ->
            getString(Data, Offset, L + 1)
    end.

get_worker(Ip) ->
    lists:nth(1 + erlang:phash2(Ip) band 7, qqwry_sup:worker_names()).


getProvince(<<"北京"/utf8, _/binary>>) ->
    1;
getProvince(<<"天津"/utf8, _/binary>>) ->
    2;
getProvince(<<"上海"/utf8, _/binary>>) ->
    3;
getProvince(<<"重庆"/utf8, _/binary>>) ->
    4;
getProvince(<<"河北"/utf8, _/binary>>) ->
    5;
getProvince(<<"河南"/utf8, _/binary>>) ->
    6;
getProvince(<<"云南"/utf8, _/binary>>) ->
    7;
getProvince(<<"辽宁"/utf8, _/binary>>) ->
    8;
getProvince(<<"黑龙江"/utf8, _/binary>>) ->
    9;
getProvince(<<"湖南"/utf8, _/binary>>) ->
    10;
getProvince(<<"湖北"/utf8, _/binary>>) ->
    11;
getProvince(<<"安徽"/utf8, _/binary>>) ->
    12;
getProvince(<<"山东"/utf8, _/binary>>) ->
    13;
getProvince(<<"新疆"/utf8, _/binary>>) ->
    15;
getProvince(<<"江苏"/utf8, _/binary>>) ->
    17;
getProvince(<<"江西"/utf8, _/binary>>) ->
    16;
getProvince(<<"浙江"/utf8, _/binary>>) ->
    18;
getProvince(<<"广西"/utf8, _/binary>>) ->
    19;
getProvince(<<"广东"/utf8,_/binary>>) ->
    20;
getProvince(<<"甘肃"/utf8, _/binary>>) ->
    21;
getProvince(<<"山西"/utf8, _/binary>>) ->
    14;
getProvince(<<"内蒙古"/utf8, _/binary>>) ->
    22;
getProvince(<<"陕西"/utf8, _/binary>>) ->
    23;
getProvince(<<"吉林"/utf8, _/binary>>) ->
    24;
getProvince(<<"福建"/utf8, _/binary>>) ->
    25;
getProvince(<<"贵州"/utf8, _/binary>>) ->
    26;
getProvince(<<"青海"/utf8, _/binary>>) ->
    28;
getProvince(<<"西藏"/utf8, _/binary>>) ->
    31;
getProvince(<<"四川"/utf8, _/binary>>) ->
    30;
getProvince(<<"宁夏"/utf8, _/binary>>) ->
    27;
getProvince(<<"海南"/utf8, _/binary>>) ->
    29;
getProvince(<<"台湾"/utf8, _/binary>>) ->
    32;
getProvince(<<"香港"/utf8, _/binary>>) ->
    33;
getProvince(<<"澳门"/utf8, _/binary>>) ->
    34;
getProvince(Address) ->
    CNAddress = unicode:characters_to_list(Address),
    ProvinceList = [
                 {"北京",1},
                 {"天津", 2}, 
                 {"重庆", 3},
                 {"上海", 4},
                 {"河北", 5}, 
                 {"河南", 6}, 
                 {"云南", 7},
                 {"辽宁", 8}, 
                 {"黑龙江", 9}, 
                 {"湖南", 10},
                 {"湖北", 11},
                 {"安徽", 12},
                 {"山东", 13},
                 {"山西", 14},
                 {"新疆", 15},
                 {"江西", 16},
                 {"江苏", 17},
                 {"浙江", 18},
                 {"广西", 19},
                 {"广东", 20},
                 {"甘肃", 21},
                 {"内蒙古", 22},
                 {"陕西", 23},
                 {"吉林", 24},
                 {"福建", 25},
                 {"贵州", 26},
                 {"宁夏", 27},
                 {"青海", 28},
                 {"海南", 29},
                 {"四川", 30},
                 {"西藏", 31},
                 {"台湾", 32},
                 {"香港", 33},
                 {"澳门", 34} 
               ],
    getProvince(normal, CNAddress, ProvinceList).

getProvince(normal, _, []) ->
    35;
getProvince(normal, Address, [{ProvinceT, ProvinceCode}|T]) ->
    IsProvince = isProvince(Address, ProvinceT),
    if 
        IsProvince ->
            ProvinceCode;
        true ->
            getProvince(normal, Address, T)
    end.

isProvince(Address, TestP) ->
    string:str(Address, TestP) > 0.


getProvinceName(1) ->
    "北京";
getProvinceName(2) ->
    "天津";
getProvinceName(3) ->
    "重庆";
getProvinceName(4) ->
    "上海";
getProvinceName(5) ->
    "河北";
getProvinceName(6) ->
    "河南";
getProvinceName(7) ->
    "云南";
getProvinceName(8) ->
    "辽宁";
getProvinceName(9) ->
    "黑龙江";
getProvinceName(10) ->
    "湖南";
getProvinceName(11) ->
    "湖北";
getProvinceName(12) ->
    "安徽";
getProvinceName(13) ->
    "山东";
getProvinceName(14) ->
    "山西";
getProvinceName(15) ->
    "新疆";
getProvinceName(16) ->
    "江西";
getProvinceName(17) ->
    "江苏";
getProvinceName(18) ->
    "浙江";
getProvinceName(19) ->
    "广西";
getProvinceName(20) ->
    "广东";
getProvinceName(21) ->
    "甘肃";
getProvinceName(22) ->
    "内蒙古";
getProvinceName(23) ->
    "陕西";
getProvinceName(24) ->
    "吉林";
getProvinceName(25) ->
    "福建";
getProvinceName(26) ->
    "贵州";
getProvinceName(27) ->
    "宁夏";
getProvinceName(28) ->
    "青海";
getProvinceName(29) ->
    "海南";
getProvinceName(30) ->
    "四川";
getProvinceName(31) ->
    "西藏";
getProvinceName(32) ->
    "台湾";
getProvinceName(33) ->
    "香港";
getProvinceName(34) ->
    "澳门";
getProvinceName(_) ->
    "喵星人".



































go() ->
    case file:consult(default_db('ip.txt')) of
        {error, Reason} ->
            io:format("~p~n", [Reason]);
        {ok, Data} -> 
            case file:open("log/ip_cn.txt", [append, {encoding, utf8}]) of
                {ok, IoDevice} ->
                    [IPL|_] = Data,
                    [ cnlocation(IP, IoDevice) || IP <- IPL ],
                    file:close(IoDevice);
                {error, R} ->
                    io:fomart("~p~n", [R])
            end
    end.

cnlocation(Ip,  IoDevice) ->
    Address = lookup(Ip),
    io:format(IoDevice, "~ts|~p~n", [Address, Ip]).

