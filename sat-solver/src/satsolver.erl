-module(satsolver).

-export([solve/1]).

-compile(export_all).

solve(CNF) ->
    Unit = gb_sets:new(),
    {Formula, NumVariables} = CNF,
    %TODO spawn and listen
    spawn(?MODULE, master, [Formula, Unit, self(), [], 4, NumVariables]),
    receive
        {sat, Solution} -> {sat, Solution};
        X -> X
    end.

master(OGamma, OUnit, Parent, Children, Resources, NumVariables) ->
%    Literal = someLiteral(Gamma),
    
    
    
    
    {Gamma, Unit} = unitPropagation(OGamma, OUnit),
    case gb_sets:is_empty(Gamma) of
        true -> Parent ! {sat, gb_sets:to_list(Unit)};
        false ->
            case gb_sets:is_element(gb_sets:new(), Gamma) of
                true -> unsat;
                false ->
                    Literal = someLiteral(Gamma),
                        
                    UnitClause = gb_sets:singleton(Literal),
                    UnitClauseNegated = gb_sets:singleton(-Literal),
                    
                    case Resources of
                        0 ->
                            Child = spawn(?MODULE, master, [gb_sets:insert(UnitClause, Gamma), Unit, Literal, self(), 0]),
                            receiveLoop(Gamma, Unit, Parent, [Child|Children], Literal, 0, NumVariables);
                        _ ->
                            {Res1, Res2} = halves(Resources),
                            Child1 = spawn(?MODULE, master, [gb_sets:insert(UnitClause, Gamma), Unit, Parent, Res1, NumVariables]),
                            Child2 = spawn(?MODULE, master, [gb_sets:insert(UnitClauseNegated, Gamma), Unit, Parent, Res2, NumVariables]),
                            Parent ! {Child1, Child2}
                    end




            end
    end.





receiveLoop(Gamma, Unit, Parent, Children, Literal, X, NumVariables) ->
    receive
        {sat, Solution} ->
            Parent ! {sat, Solution},
            map(fun(Child) -> exit(Child, sat) end, Children);
        {unsat, UsedLiterals, Child} ->
            NX = X + math:pow(2, (UsedLiterals - length(Unit))),   %TODO length nicht jedes mal neu aufrufen
            case math:pow(2, (NumVariables - length(Unit))) of
                NX -> Parent ! length(Unit);
                _ ->
                    self() ! 1,
                    receiveLoop(Gamma, Unit, Parent, lists:delete(Child, Children), Literal, X, NumVariables)
            end;
        NewResources ->
            map(fun(Child) -> Child ! Parent end, Children),
            
            UnitClauseNegated = gb_sets:singleton(-Literal),
            Child = spawn(?MODULE, master, [gb_sets:insert(UnitClauseNegated, Gamma), Unit, Parent, NewResources, NumVariables]),
            Parent ! [Child|Children]
    end.


halves(N) when N rem 2 == 0 -> {N div 2, N div 2};
halves(N) -> {N div 2, N div 2 - 1}.


dpll(OGamma, OUnit) ->
    {Gamma, Unit} = unitPropagation(OGamma, OUnit),
    case gb_sets:is_empty(Gamma) of
        true -> {sat, gb_sets:to_list(Unit)};
        false ->
            case gb_sets:is_element(gb_sets:new(), Gamma) of
                true -> unsat;
                false ->
                    Literal = someLiteral(Gamma),
                    Disjunction = gb_sets:singleton(Literal),
                    case dpll(gb_sets:insert(Disjunction, Gamma), Unit) of
                        unsat ->
                            DisjunctionWithNegatedLiteral = gb_sets:singleton(-Literal),
                            NGamma = gb_sets:insert(DisjunctionWithNegatedLiteral, Gamma),
                            dpll(NGamma, Unit);
                        SAT -> SAT
                    end
            end
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


%kann man leider nicht verwenden, da sonst die Performanz in den Keller geht

%eliminate(Literals, Gamma) ->
%    NGamma = gb_sets:filter(fun(Clause) -> gb_sets:is_empty(gb_sets:intersection(Literals, Clause)) end, Gamma),  %only keep clauses that don't contain any literal in Literals
%
%    NegatedLiterals = negateLiterals(Literals),
%    F = fun(Clause) ->
%            gb_sets:subtract(Clause, NegatedLiterals)
%        end,
%
%    map(F, NGamma).

eliminate(Literal, Gamma) ->
    NGamma = gb_sets:filter(fun(Clause) -> gb_sets:is_element(Literal, Clause) == false end, Gamma), %only keep clauses containing Literal
    map(fun(Clause) -> gb_sets:delete_any(-Literal, Clause) end, NGamma).   %remove -Literal from all clauses



map(F, Set) ->
    map(F, gb_sets:iterator(Set), gb_sets:new()).
map(F, SetIterator, Acc) ->
    Next = gb_sets:next(SetIterator),
    case Next of
        {Element, NextIterator} ->
            map(F, NextIterator, gb_sets:add(F(Element), Acc));
        none -> Acc
    end.

negateLiterals(Clause) ->
    map(fun(Literal) -> -Literal end, Clause).

someLiteral(Gamma) ->
    gb_sets:smallest(gb_sets:smallest(Gamma)).

nonContradictoryLiterals(Literals) ->
    gb_sets:is_empty(gb_sets:filter(fun(Literal) -> gb_sets:is_element(-Literal, Literals) end, Literals)).
    






testSolution(CNF, {sat, Solution}) ->    
    ContradictoryLiterals = gb_sets:filter(fun(Literal) -> gb_sets:is_element(-Literal, Solution) end, Solution),
    case gb_sets:is_empty(ContradictoryLiterals) of
        false -> invalid_contradictory_literals;
        true ->
            UnsatisfiedClauses = gb_sets:filter(fun(Clause) -> gb_sets:is_disjoint(Clause, Solution) end, CNF),
            case gb_sets:is_empty(UnsatisfiedClauses) of
                true -> ok;
                false -> invalid_unsatisfied_clauses
            end
    end.
