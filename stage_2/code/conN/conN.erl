-module(conN).
% running guide： c(conN). -- conN:start_server(16). -- server ! {range, 1, 15000}.
-export([start_server/1, worker/0,server/2,workerName/1,printTime/0,setWorkerCaculate/4,
     hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2]). 

%% TotientRange.erl - Sequential Euler Totient Function (Erlang Version)
%% hcf x 0 = x
%% hcf x y = hcf y (rem x y) 

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

sumTotient(Lower,Upper) -> 
  Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
  io:format("Sum of totients: ~p~n", [Res]). 
  


%% caculate time
printTime() ->
    {_, Time2} = statistics(wall_clock),
    U2 = Time2 * 1000,   % actual runtime
    io:format("Time taken in ~p  seconds~n",[U2/1000000]).

%% produce worker name
workerName(Num) ->
list_to_atom( "worker" ++ integer_to_list( Num )).

server(0,TOTAL) ->
    io:format("Server : Sum of totients: ~p ~n", [TOTAL]),
    printTime();
server(N,TOTAL) ->
     receive   
       %%用来接收每个线程产生的结果
     {finished,SUM} ->
            io:format("Server :received Sum ~p ~n", [SUM]),
            server(N-1,TOTAL+SUM); % waiting for N worker's result and loop
      {range, LOW,UPPER} ->  % order to recieve value
            statistics(wall_clock), % start time clock
            io:format("Server :received range ~p ~p ~n", [LOW,UPPER]),

            setWorkerCaculate(LOW,UPPER,N,N), % 产生N个线程方法
            server(N,TOTAL)
     end.

%% this method is used for loop to produce  N worker process
%% 该方法用来产生一个worker线程，并把要计算的数传给worker
setWorkerCaculate(_,_,0,_) -> % _ is occupation , it means we do not care this argument
  io:format("calculate  ~n", []);  % finish start N workers
setWorkerCaculate(LOW,UPPER,N,NUM) ->
            register(workerName(N), spawn(conN, worker, [])), % register one worker
            workerName(N) ! {caculate_sum,round((UPPER/NUM)*(N-1)+1),round((UPPER/NUM)*N)}, % give the worker range and start worker to work
            setWorkerCaculate(LOW,UPPER,N-1,NUM).  % loop to produce N workers


worker() ->
    receive
        {caculate_sum,L,U} ->
            io:format("Worker : started ~n", []),
            io:format("Worker : compute range ~p ~p ~n", [L,U]),
            SUM = lists:sum(lists:map(fun euler/1,lists:seq(L, U))),% calculate sum and pass it to server
            io:format("Worker : finished ~n", []), 
            server ! {finished,SUM}  
    end.

%% start server process and pass the number of processes as N
start_server(N) ->
    register(server, spawn(conN, server, [N,0])).%%注册Server线程
    
  
  
    