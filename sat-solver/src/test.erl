-module(test).

-export([test/0]).

-compile(export_all).


test() ->
    {ok, CNF} = dimacs:parse("../cnf/3sat-unsat-minimal.cnf"),
    Solution = satsolver:solve(CNF),
    R1 = check(CNF, Solution, unsat),

    {ok, CNF2} = dimacs:parse("../cnf/aim-50-1_6-yes1-4.cnf"),
    Solution2 = satsolver:solve(CNF2),
    R2 = check(CNF2, Solution2, sat),

    {ok, CNF3} = dimacs:parse("../cnf/aim-50-1_6-yes1-4_UNSAT.cnf"),
    Solution3 = satsolver:solve(CNF3),
    R3 = check(CNF3, Solution3, unsat),

    io:format("---------\n"),
    print(1, R1),
    print(2, R2),
    print(3, R3),
    io:format("---------\n"),

    case (R1 == R2) and (R2 == R3) and (R3 == ok) of
        true -> ok;
        false -> fail
    end.

check(CNF, Solution, ExpectedResult) ->
    case Solution of
        {sat, _Unit} ->
            case ExpectedResult of
                unsat -> fail;
                sat ->
                    Result = satsolver:testSolution(CNF, Solution),
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