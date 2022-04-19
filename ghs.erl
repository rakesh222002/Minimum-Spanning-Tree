-module(ghs).

-export([start/0, startProcess/0]).
%useful functions
findMin([H|Edges]) -> findMin(Edges, H).
findMin([], Mini) -> Mini;
findMin([[P, W, St] | Edges], [Mp, Mw, Mst]) ->
    if
        W < Mw ->
            findMin(Edges, [P, W, St]);
        true ->
            findMin(Edges, [Mp, Mw, Mst])    
    end.

getStatus([[A, B, C] | Edges], Id) when A==Id -> C;
getStatus([H | Edges], Id) -> getStatus(Edges, Id).

getWt([[A, B, C] | Edges], Id) when A==Id -> B;
getWt([H | Edges], Id) -> getStatus(Edges, Id).

sendInitiates([], Upid, L, F, S, FindCount) -> FindCount;
sendInitiates([[Pid, Wt, _] | Edges], Upid, L, F, S, FindCount) ->
    Pid ! {initiate, Upid, L, F, S, Wt},
    if
        S==find ->
            sendInitiates(Edges, Upid, L, F, S, FindCount+1);
        true ->
            sendInitiates(Edges, Upid, L, F, S, FindCount)
    end.
% code
trigger(Pid) -> Pid ! wakeup.
processData(X, Edges) -> X ! {setEdges, Edges}.
startProcess() ->
    receive
        {setEdges,  Edges} ->
            startGhs(none, none, [ [X, Y, basic] || [X, Y] <- Edges], sleep, none, none, none, none, none)
    end.

startGhs(FN, LN, Edges, SN, BestEdge, BestWt, TestEdge, InBranch, FindCount) ->
    receive 
        wakeup->
            [Mpid, Mwt, Mst] = findMin(Edges),
            Mpid ! {connect,self(), 0}, %pid, LN, wt
            NewEdges = [[EXA, EXB, EXC] || [EXA, EXB, EXC] <- Edges, EXA /= Mpid] ++ [[Mpid, Mwt, branch]],
            io:fwrite("~w is awake~n", [self()]),
            startGhs(FN, 0, NewEdges , found, BestEdge, BestWt, TestEdge, InBranch, 0);

        {connect, Upid, L} when SN == sleep ->
            io:fwrite("Connect from ~w to ~w~n", [Upid, self()]),
            self() ! wakeup,
            self() ! {connect, Upid, L},
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);

        {connect, Upid, L} when L<LN ->
            NewEdges = [[EXA, EXB, EXC] || [EXA, EXB, EXC] <- Edges, EXA =/= Upid] ++ [[Upid, getWt(Edges, Upid), branch]],
            Upid ! {initiate, self(), LN, FN, SN},
            if
                SN == find ->
                    startGhs(FN, LN, NewEdges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount+1);
                true ->
                    startGhs(FN, LN, NewEdges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount)
            end;
        
        {connect, Upid, L}->
            MsgEdgeStatus = getStatus(Edges, Upid),
            if
                MsgEdgeStatus == basic ->
                    self() ! {connect, Upid, L};
                true ->
                    Upid ! {initiate, self(), LN+1, getWt(Edges, Upid), find}
            end,
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
        
        {initiate, Upid, L, F, S} ->
            io:fwrite("Initiate from ~w to ~w~n", [Upid, self()]),
            BranchEdges = [[A, B, C] || [A, B, C] <- Edges, C == branch, A /= Upid],
            NewFindCount = sendInitiates(BranchEdges, Upid, L, F, S, 0) + FindCount,
            NewLN = L,
            NewFN = F,
            NewSN = S,
            NewInBranch = Upid,
            NewBestEdge = none,
            NewBestWeight = 1000000,
            if
                S == find ->
                    % procedure test
                    BasicEdges = [[A, B, C] || [A, B, C] <-Edges, C==basic],
                    if
                        BasicEdges == [] ->
                            % test edge nil, execute report
                            ExTest = none,
                            if
                                NewFindCount == 0 andalso ExTest==none->
                                    InBranch ! {report, self(),  BestWt},
                                    startGhs(NewFN, NewLN, Edges , found, NewBestEdge, NewBestWeight, ExTest, NewInBranch, NewFindCount);
                                true ->
                                    startGhs(NewFN, NewLN, Edges , NewSN, NewBestEdge, NewBestWeight, ExTest, NewInBranch, NewFindCount)
                            end;
                        true ->
                            MinBasic = lists:nth(1, BasicEdges),
                            lists:nth(1, MinBasic) ! {test, self(), LN, FN},
                            startGhs(NewFN, NewLN, Edges , NewSN, NewBestEdge, NewBestWeight, lists:nth(1, MinBasic), NewInBranch, NewFindCount)
                    end
            end,
            startGhs(F, L, Edges, S, BestEdge, BestWt, TestEdge, Upid, NewFindCount);

        {test, Upid, L, F} when SN==sleep->
            io:fwrite("Test from ~w to ~w~n", [Upid, self()]),
            self() ! wakeup,
            self() ! {test, Upid, L, F},
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
        
        {test, Upid, L, F} when L > LN ->
            self() ! {test, Upid, L, F},
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
        
        {test, Upid, L, F} when F /= FN ->
            Upid ! {accept, self()},
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
        
        {test, Upid, L, F} ->
            StatusT = getStatus(Edges, Upid), 
            WeightT = getWt(Edges, Upid),
            if 
                StatusT == basic ->
                    NewEdges = [[EXA, EXB, EXC] || [EXA, EXB, EXC] <- Edges, EXA /= Upid] ++ [[Upid, WeightT, rejected]];
                true ->
                    NewEdges = Edges
            end,
            if
                TestEdge /= Upid ->
                    Upid ! {reject, self()};
                true ->
                    BasicEdges = [[A, B, C] || [A, B, C] <-Edges, C==basic],
                    if
                        BasicEdges == [] ->
                            % test edge nil, execute report
                            if
                                FindCount == 0 andalso TestEdge == none->
                                    InBranch ! {report, self(),  BestWt},
                                    startGhs(FN, LN, Edges , found, BestEdge, BestWt, TestEdge, InBranch, FindCount);
                                true ->
                                    startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount)
                            end;
                        true ->
                            MinBasic = lists:nth(1, BasicEdges),
                            lists:nth(1, MinBasic) ! {test, self(), LN, FN},
                            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, lists:nth(1, MinBasic), InBranch, FindCount)
                    end
            end;

        {accept, Upid} ->
            io:fwrite("Accept from ~w to ~w~n", [Upid, self()]),
            NewTestEdge = none,
            EdgeWeight = getWt(Edges, Upid),
            if
                EdgeWeight < BestWt ->
                    NewBestEdge = Upid,
                    NewBestWeight = EdgeWeight;
                true ->
                    NewBestEdge = BestEdge,
                    NewBestWeight = BestWt
            end,
            if
                FindCount == 0 andalso TestEdge == none->
                    InBranch ! {report, self(),  BestWt},
                    startGhs(FN, LN, Edges , found, NewBestEdge, NewBestWeight, TestEdge, InBranch, FindCount);
                true ->
                    startGhs(FN, LN, Edges , SN, NewBestEdge, NewBestWeight, TestEdge, InBranch, FindCount)
            end;
        
        {reject, Upid} ->
            io:fwrite("Reject from ~w to ~w~n", [Upid, self()]),
            EdgeStatus = getStatus(Edges, Upid),
            EdgeWeight = getWt(Edges, Upid),
            if
                EdgeStatus == basic ->
                    NewEdges = [[EXA, EXB, EXC] || [EXA, EXB, EXC] <- Edges, EXA /= Upid] ++ [[Upid, EdgeWeight, rejected]];
                true ->
                    NewEdges = Edges
            end,
            BasicEdges = [[A, B, C] || [A, B, C] <-Edges, C==basic],
            if
                BasicEdges == [] ->
                    % test edge nil, execute report
                    ExTest = none,
                    if
                        FindCount == 0 andalso ExTest==none->
                            InBranch ! {report, self(),  BestWt},
                            startGhs(FN, LN, Edges , found, BestEdge, BestWt, ExTest, InBranch, FindCount);
                        true ->
                            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, ExTest, InBranch, FindCount)
                    end;
                true ->
                    MinBasic = lists:nth(1, BasicEdges),
                    lists:nth(1, MinBasic) ! {test, self(), LN, FN},
                    startGhs(FN, LN, Edges , SN, BestEdge, BestWt, lists:nth(1, MinBasic), InBranch, FindCount)
            end,
            startGhs(FN, LN, NewEdges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
        
        {report, Upid, W} ->
            io:fwrite("Report from ~w to ~w~n", [Upid, self()]),
            EdgeStatus = getStatus(Edges, Upid),
            if
                EdgeStatus /=branch ->
                    if
                        W < BestWt ->
                            NewBestEdge = Upid,
                            NewBestWeight = W;
                        true ->
                            NewBestEdge = BestEdge,
                            NewBestWeight = BestWt
                    end,
                    if
                        FindCount == 0 andalso TestEdge == none->
                            InBranch ! {report, self(),  BestWt},
                            startGhs(FN, LN, Edges , found, NewBestEdge, NewBestWeight, TestEdge, InBranch, FindCount);
                        true ->
                            startGhs(FN, LN, Edges , SN, NewBestEdge, NewBestWeight, TestEdge, InBranch, FindCount)
                    end;
                SN == find ->
                    self() ! {report, Upid, W},
                    startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
                
                W > BestWt ->
                    self() ! {changeRoot},
                    startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount);
                true ->
                    if 
                        W==1000000 andalso BestWt == 1000000 ->
                            io:fwrite("Node ~w exitted with Edges ~w", [self(), Edges]);
                        true ->
                            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount)
                    end
            end;
                    

        {changeRoot} ->
            io:fwrite("Change root on ~w", [self()]),
            EdgeStatus = getStatus(Edges, BestEdge),
            if
                EdgeStatus == branch ->
                    BestEdge ! {changeRoot};
                true ->
                    BestEdge ! {connect, self(), LN}
            end,
            startGhs(FN, LN, Edges , SN, BestEdge, BestWt, TestEdge, InBranch, FindCount)
    end.

start() ->
    A = spawn(ghs, startProcess, []),
    B = spawn(ghs, startProcess, []),
    C = spawn(ghs, startProcess, []),
    D = spawn(ghs, startProcess, []),
    E = spawn(ghs, startProcess, []),
    
    processData(A, [[B, 2], [D, 6]]),
    processData(B, [[A, 2], [C, 3], [D, 8], [E, 5]]),
    processData(C, [[B, 3], [E, 7]]),
    processData(D, [[A, 6], [B, 8], [E, 9]]),
    processData(E, [[B, 5], [C, 7], [D, 9]]),
    trigger(A).