-module(reliable).
%% running guide： c(reliable). -- reliable:testRobust(4,3). 
-export([start_server/1, 
    testRobust/2,
    workerChaos/2,
    workerWatcher/3,
    worker/1,
    setWorkerCaculate/4,
    server/2,
    workerName/1,
    printTime/0,
      
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
  Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),%把lower 到upper的数一个一个放到欧拉函数里面，欧拉函数只接收一个变量，然后返回结果列表，再计算结果的和
  io:format("Sum of totients: ~p~n", [Res]). %输出第二个参数需要是一个列表。
  



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
     {finished,SUM} ->
            io:format("Server :received Sum ~p ~n", [SUM]),
            server(N-1,TOTAL+SUM); % waiting for N worker's result and loop
      {range, LOW,UPPER} ->  % order to recieve value
            statistics(wall_clock), % start time clock
            io:format("Server :received range ~p ~p ~n", [LOW,UPPER]),
            setWorkerCaculate(LOW,UPPER,N,N), % 
            server(N,TOTAL)
     end.

%% this method is used for loop to produce  N worker process
setWorkerCaculate(_,_,0,_) ->
  io:format("calculate  ~n", []);  % finish start N workers
setWorkerCaculate(LOW,UPPER,N,NUM) ->
            register(workerName(N), spawn(reliable, worker, [N])), % register one worker
            workerName(N) ! {caculate_sum,round((UPPER/NUM)*(N-1)+1),round((UPPER/NUM)*N)}, % give the worker range and start worker to work
            setWorkerCaculate(LOW,UPPER,N-1,NUM).  % loop to produce N workers
worker(WorkerNum) ->
    receive
        {caculate_sum,L,U} ->
          % 设置链接线程，每个worker都设置一个链接线程，返回值是该链接进程的id，但是不需要用到这个pid
            spawn_link(reliable,workerWatcher,[WorkerNum,L,U]), % set a watcher link
            io:format("~p : started ~n", [workerName(WorkerNum)]),
            io:format("~p : compute range ~p ~p ~n", [workerName(WorkerNum),L,U]),
            SUM = lists:sum(lists:map(fun euler/1,lists:seq(L, U))),% calculate sum and pass it to server
            io:format("~p : finished ~n", [workerName(WorkerNum)]), 
            server ! {finished,SUM}  
    end.
%% set workerWatcher
workerWatcher(WorkerNum,L,U) ->  
    io:format("Watcher : Watching  ~p~n", [workerName(WorkerNum)]),
    process_flag(trap_exit, true),   % get Exit infromation设置这句话来获取非正常退出进程
    receive  
      % 如果监管的进程异常终止，则重新创建该进程，这时worker和workerwatcher都会exit，而重新创建的进程会生成新的workerwatcher
        {'EXIT',_,chaos} ->  % if kill restart the worker
        io:format("Watcher: restart : ~p~n", [workerName(WorkerNum)]),
        register(workerName(WorkerNum), spawn(reliable, worker, [WorkerNum])),
        workerName(WorkerNum) ! {caculate_sum,L,U};

        {'EXIT',_,normal} -> 
          io:format("watcher: nomal : ~p~n", [workerName(WorkerNum)])
    end. 
%% chaos
workerChaos(NVictims,NWorkers) ->
  lists:map(fun( _ ) ->timer:sleep(500), %% Sleep for .5s%% Choose a random victim
        WorkerNum = rand:uniform(NWorkers),
        io:format("workerChaos killing ~p~n",
        [workerName(WorkerNum)]),
        WorkerPid = whereis(workerName(WorkerNum)),
        if   %% Check if victim is alive
          WorkerPid == undefined ->
            io:format("workerChaos already dead: ~p~n",[workerName(WorkerNum)]);
          true ->   %% Kill Kill Kill
            exit(whereis(workerName(WorkerNum)),chaos)
            end
          end,
  lists:seq( 1, NVictims ) ).


%% start server process and pass the number of processes as N
start_server(N) ->
    register(server, spawn(reliable, server, [N,0])).

  %% test method
testRobust(NWorkers,NVictims) ->
      ServerPid = whereis(server),
      if ServerPid == undefined ->
          start_server(NWorkers);
        true ->
          ok
        end,
      server ! {range, 1, 15000},
      workerChaos(NVictims,NWorkers).