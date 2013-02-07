-module(satsolver).

-export([solve/1]).

-compile(export_all).

solve(CNF) ->
    Unit = gb_sets:new(),
    {Formula, NumVariables} = CNF,
    %TODO spawn and listen
    spawn(?MODULE, master, [Formula, Unit, self(), 4, pow(2, NumVariables)]),
    receive
        {sat, Solution} ->
            %TODO kill children
            {sat, Solution};
        {unsat, _, _, BurnedSolutions} ->
            io:format("tested ~B possibilities\n", [BurnedSolutions]),
            unsat
    end.

master(Gamma, Unit, Parent, Resources, Solutions) ->
%    Literal = someLiteral(Gamma),
    
    
    
    
    {NGamma, NUnit} = unitPropagation(Gamma, Unit),
    case gb_sets:is_empty(NGamma) of
        true -> Parent ! {sat, gb_sets:to_list(NUnit)};
        false ->
            case gb_sets:is_element(gb_sets:new(), NGamma) of
                true -> Parent ! {unsat, self(), Resources, Solutions};
                false ->
                    Literal = someLiteral(NGamma),
                        
                    UnitClause = gb_sets:singleton(Literal),
                    UnitClauseNegated = gb_sets:singleton(-Literal),

                    {Res1, Res2} = halves(Resources),
                    Child1 = spawn(?MODULE, master, [gb_sets:insert(UnitClause, NGamma), NUnit, self(), Res1, Solutions div 2]),
                    Child2 = spawn(?MODULE, master, [gb_sets:insert(UnitClauseNegated, NGamma), NUnit, self(), Res2, Solutions div 2]),
                    receiveLoop(NGamma, NUnit, Parent, {[Child1|[Child2]], 0}, Literal, {Solutions, 0})
            end
    end.





receiveLoop(Gamma, Unit, Parent, {Children, NumBiologicalChilds}, Literal, {Solutions, BurnedSolutions}) ->
    receive
        {sat, Solution} ->
            Parent ! {sat, Solution};
            %lists:map(fun(Child) -> exit(Child, sat) end, Children);
        {unsat, Child, Resources, NewBurnedSolutions} ->
            io:format("burned ~B possibilities\n", [NewBurnedSolutions]),
            NChildren = lists:delete(Child, Children),
            NBurnedSolutions = NewBurnedSolutions + BurnedSolutions,
            case Solutions - NBurnedSolutions of
                0 ->
                    Parent ! {unsat, self(), Resources, NBurnedSolutions};
                _ ->
                    %Parent ! Resources,
                    receiveLoop(Gamma, Unit, Parent, {NChildren, NumBiologicalChilds}, Literal, {Solutions, NBurnedSolutions})
            end;
        NewResources ->
 %           case NumBiologicalChilds of
 %               0 ->
 %                   
 %               1 ->
                    todo
        %Resources -> Parent ! Resources
    end.


halves(N) when N rem 2 == 0 -> {N div 2, N div 2};
halves(N) -> {N div 2 + 1, N div 2}.


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

pow(N, M) ->
    pow(N, M, 1).
pow(_, 0, Acc) ->
    Acc;
pow(N, M, Acc) ->
    pow(N, M-1, Acc * N).



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
