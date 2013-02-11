-module(satsolver_seq).

-export([solve/1]).

solve(CNF) ->
    {Formula, _NumVariables} = CNF,
    dpll(Formula, gb_sets:new()).

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
    NGamma = gb_sets:filter(fun(Clause) -> gb_sets:is_element(Literal, Clause) == false end, Gamma), %only keep clauses containing Literals
    map(fun(Clause) -> gb_sets:delete_any(-Literal, Clause) end, NGamma).   %remove -Literal from all clauses



someLiteral(Gamma) ->
    gb_sets:smallest(gb_sets:smallest(Gamma)).

map(F, Set) ->
    map(F, gb_sets:iterator(Set), gb_sets:new()).
map(F, SetIterator, Acc) ->
    Next = gb_sets:next(SetIterator),
    case Next of
        {Element, NextIterator} ->
            map(F, NextIterator, gb_sets:add(F(Element), Acc));
        none -> Acc
    end.