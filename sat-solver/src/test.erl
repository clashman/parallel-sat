-module(test).

-export([solver/0]).

-compile(export_all).

solver() ->
    R1 = check("../cnf/3sat-unsat-minimal.cnf", unsat),
    R2 = check("../cnf/aim-50-1_6-yes1-4.cnf", sat),
    R3 = check("../cnf/aim-50-1_6-yes1-4_UNSAT.cnf", unsat),

    io:format("---------\n"),
    print(1, R1),
    print(2, R2),
    print(3, R3),
    io:format("---------\n"),

    case (R1 == R2) and (R2 == R3) and (R3 == ok) of
        true -> ok;
        false -> fail
    end.

check(DimacsFile, ExpectedResult) ->
    {ok, CNF} = dimacs:parse(DimacsFile),
    Solution = satsolver:solve(CNF),

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
