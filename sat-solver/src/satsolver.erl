-module(satsolver).

%TODO use internal spawn and don't export node/5
-export([solve/1, node/5]).

solve(CNF) ->
    Unit = gb_sets:new(),
    {Formula, NumVariables} = CNF,
    spawn(?MODULE, node, [Formula, Unit, self(), 4, pow(2, NumVariables)]),
    receive
        {sat, Solution} ->
            io:format("found solution\n"),
            {sat, Solution};
        {unsat, _, BurnedSolutions} ->
            io:format("tested ~B possibilities (unsat)\n", [BurnedSolutions]),
            unsat
    end.

node(Gamma, Unit, Parent, Resources, Solutions) ->
    {NGamma, NUnit} = unitPropagation(Gamma, Unit),
    case gb_sets:is_empty(NGamma) of
        true -> Parent ! {sat, gb_sets:to_list(NUnit)};
        false ->
            case gb_sets:is_element(gb_sets:new(), NGamma) of
                true -> Parent ! {unsat, self(), Solutions};
                false ->
                    case Resources of
                        0 -> ok;
                        _ -> self() ! Resources
                    end,
                    receiveLoop(NGamma, NUnit, Parent, gb_trees:empty(), {Solutions, 0})
            end
    end.

receiveLoop(Gamma, Unit, Parent, Children, {Solutions, BurnedSolutions}) ->
    receive
        {sat, Solution} ->
            Parent ! {sat, Solution};
            %TODO kill children
            %lists:map(fun(Child) -> exit(Child, sat) end, Children);
        {unsat, Child, NewBurnedSolutions} ->
            io:format("burned ~B possibilities\n", [NewBurnedSolutions]),
            ReleasedResources = gb_trees:get(Child, Children),
            NChildren = gb_trees:delete(Child, Children),
            NBurnedSolutions = NewBurnedSolutions + BurnedSolutions,
            case Solutions - NBurnedSolutions of
                0 ->
                    Parent ! {unsat, self(), NBurnedSolutions};
                _ ->
                    {ReceivingChild, CurrentResources} = gb_trees:smallest(NChildren),
                    ReceivingChild ! ReleasedResources,
                    NNChildren = gb_trees:update(ReceivingChild, CurrentResources + ReleasedResources, NChildren),
                    receiveLoop(Gamma, Unit, Parent, NNChildren, {Solutions, NBurnedSolutions})
            end;
        NewResources ->
            NChildren = case gb_trees:is_empty(Children) of
                false ->
                    {ReceivingChild, CurrentResources} = gb_trees:smallest(Children),
                    ReceivingChild ! NewResources,
                    gb_trees:update(ReceivingChild, CurrentResources + NewResources, Children);
                true ->
                    Literal = someLiteral(Gamma),

                    UnitClause = gb_sets:singleton(Literal),
                    UnitClauseNegated = gb_sets:singleton(-Literal),

                    {Res1, Res2} = halves(NewResources),
                    Child1 = spawn(?MODULE, node, [gb_sets:insert(UnitClause, Gamma), Unit, self(), Res1, Solutions div 2]),
                    Child2 = spawn(?MODULE, node, [gb_sets:insert(UnitClauseNegated, Gamma), Unit, self(), Res2, Solutions div 2]),
                    gb_trees:insert(Child1, Res1, gb_trees:insert(Child2, Res2, gb_trees:empty()))
            end,
            receiveLoop(Gamma, Unit, Parent, NChildren, {Solutions, BurnedSolutions})
    end.

unitPropagation(Gamma, Unit) ->
    UnitClauses = gb_sets:filter(fun(Clause) -> gb_sets:size(Clause) == 1 end, Gamma),
    StandaloneLiterals = map(fun(UnitClause) -> gb_sets:smallest(UnitClause) end, UnitClauses),  %unpack unit clause literals

    case gb_sets:is_empty(StandaloneLiterals) of
        true -> {Gamma, Unit};
        false ->
            NGamma = eliminate(gb_sets:smallest(StandaloneLiterals), Gamma),  %optinally subtract UnitClauses from Gamma in advance (for a litte speedup)
            NUnit = gb_sets:union(Unit, StandaloneLiterals),
            unitPropagation(NGamma, NUnit)
    end.

eliminate(Literal, Gamma) ->
    NGamma = gb_sets:filter(fun(Clause) -> gb_sets:is_element(Literal, Clause) == false end, Gamma), %only keep clauses containing Literal
    map(fun(Clause) -> gb_sets:delete_any(-Literal, Clause) end, NGamma).   %remove -Literal from all clauses

someLiteral(Gamma) ->
    gb_sets:smallest(gb_sets:smallest(Gamma)).

halves(N) when N rem 2 == 0 -> {N div 2, N div 2};
halves(N) -> {N div 2 + 1, N div 2}.



pow(N, 1) -> N;
pow(N, M) ->
    {M1,M2} = halves(M),
    pow(N,M1) * pow(N,M2).

map(F, Set) ->
    map(F, gb_sets:iterator(Set), gb_sets:new()).
map(F, SetIterator, Acc) ->
    Next = gb_sets:next(SetIterator),
    case Next of
        {Element, NextIterator} ->
            map(F, NextIterator, gb_sets:add(F(Element), Acc));
        none -> Acc
    end.
