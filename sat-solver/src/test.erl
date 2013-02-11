-module(test).

-export([solver/0, testSolution/2]).

solver() ->
    CNFs = [{"../cnf/3sat-unsat-minimal.cnf", unsat},
            {"../cnf/aim-50-1_6-yes1-4.cnf", sat},
            {"../cnf/aim-50-1_6-yes1-4_UNSAT.cnf", unsat}
            ],
    
    Results = lists:map(fun({CNF, ExpectedResult}) -> check(CNF, ExpectedResult) end, CNFs),

    io:format("---------\n"),
    lists:foldl(fun(Result, N) -> print(N, Result), N+1 end, 1, Results),
    io:format("---------\n"),

    BothOk = fun(First, Second) ->
                 case (First == Second) and (Second == ok) of
                     true -> ok;
                     false -> fail
                 end
             end,

    _AllOk = lists:foldl(BothOk, ok, Results).

check(DimacsFile, ExpectedResult) ->
    {ok, CNF} = dimacs:parse(DimacsFile),
    Solution = satsolver:solve(CNF),

    case Solution of
        {sat, _Unit} ->
            case ExpectedResult of
                unsat -> fail;
                sat ->
                    Result = testSolution(CNF, Solution),
                    case Result of
                        ok -> ok;
                        _ -> fail
                    end
            end;
        unsat ->
            case ExpectedResult of
                unsat -> ok;
                sat -> fail
            end
    end.

print(N, ok) ->
    io:format("~B: ok\n", [N]),
    ok;
print(N, fail) ->
    io:format("~B: FAIL\n", [N]),
    fail.

testSolution({CNF, _NumVariables}, {sat, SolutionLiterals}) ->
Solution = gb_sets:from_list(SolutionLiterals),
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
