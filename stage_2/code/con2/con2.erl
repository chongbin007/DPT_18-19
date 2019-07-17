-module(con2).
% runing guide:  c(con2). -- con2:start_server(). -- server ! {range, 1, 4000}.
-export([start_server/0, worker_1/0, worker_2/0, server/2,
     hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2]). % all method related to process has to export

%% TotientRange.erl - Sequential Euler Totient Function (Erlang Version)

%% hcf x 0 = x
%% hcf x y = hcf y (rem x y) 两个数最大公约数

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) -> 
  V = hcf(X,Y),
  if 
    V == 1 
      -> true;
    true 
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) -> 
  RelprimeN = fun(Y) -> relprime(N,Y) end,  
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).



%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
%% 计算欧拉和
sumTotient(Lower,Upper) -> 
  Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
  io:format("Sum of totients: ~p~n", [Res]). %输出第二个参数需要是一个列表。

%% caculate time
printTime() ->
    {_, Time2} = statistics(wall_clock),
    U2 = Time2 * 1000,   % actual runtime
    io:format("Time taken in ~p  seconds~n",[U2/1000000]).

%% server 线程
server(0,TOTAL) -> % finish sum and print time
    io:format("Server 0: Sum of totients: ~p ~n", [TOTAL]),
    printTime();

server(N,TOTAL) ->
     receive   % waiting for recieve
        {finished,SUM} ->
            io:format("Server :Received Sum ~p ~n", [SUM]),
            io:format("Worker : Finished! ~n", []),
            server(N-1,TOTAL+SUM); % keep receiving when the N is 0
        %% 传入消息range和数
        {range, LOW,UPPER} ->  % the order for recieving
            statistics(wall_clock), % start setting clock

            io:format("Server : Compute range ~p ~p ~n", [LOW,UPPER]), 
            worker_1 ! {sum,round(LOW),round(UPPER/2)},  % send argument to worker and start it
            worker_2 ! {sum,round(UPPER/2+1), round(UPPER)},
            server(N-1 ,0)  % recall this method waiting for recieving
     end.
   
worker_1() ->
    receive
        {sum,L,U} ->
            io:format("Worker 1 : compute range ~p ~p ~n", [L,U]),
            SUM = lists:sum(lists:map(fun euler/1,lists:seq(L, U))),%  worker calculate the sum
            server ! {finished,SUM}  % when finishing sum send it back to server
    end.
   
worker_2() ->
    receive
        {sum,L,U} ->
            io:format("Worker 2 : compute range ~p ~p ~n", [L,U]),
            SUM = lists:sum(lists:map(fun euler/1,lists:seq(L, U))), % caculate erla
            server ! {finished,SUM}  % send sum to server
    end.
   
start_server() ->
    %% 开启线程并传参
    register(server, spawn(con2, server, [3,0])),   % create a server processe and register name ,the last parameter is argument
    register(worker_1, spawn(con2, worker_1, [])),  % create two workers and register name
    register(worker_2, spawn(con2, worker_2, [])). 
    