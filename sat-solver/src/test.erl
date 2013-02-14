-module(test).

-export([solver/0, testSolution/2, benchmark/0]).

solvers() -> [ fun satsolver_seq:solve/1,
               fun satsolver:solve/1
             ].

cnfs() -> [{"../cnf/3sat-unsat-minimal.cnf", unsat},
           {"../cnf/aim-50-1_6-yes1-4.cnf", sat},
           {"../cnf/aim-50-1_6-yes1-4_UNSAT.cnf", unsat},
           {"../cnf/zebra_v155_c1135.cnf", sat}
          ].

solver() ->
    Results = lists:map(fun({CNF, ExpectedResult}) -> check(CNF, ExpectedResult) end, cnfs()),

    io:format("---------\n"),
    lists:foldl(fun(Result, N) -> print(N, Result), N+1 end, 1, Results),
    io:format("---------\n"),

    case lists:all(fun(Result) -> Result == ok end, Results) of
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

print(N, Result) ->
    case N of
        -1 -> ok;
        _ -> io:format("~B: ", [N])
    end,
    io:format(atom_to_list(Result) ++ "\n"),
    Result.

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

benchmark() -> benchmark(solvers()).

% Compares runtimes of all Solvers to the first (head) Solver.
benchmark(Solvers) ->
    Timetable = lists:map(fun(Solver) ->
                    Times = lists:map(fun({DimacsFile, _}) ->
                        runBenchmark(DimacsFile, Solver)
                    end, cnfs()),
                    Mean = lists:sum(Times)/length(Times),
                    [Mean|Times]
                end, Solvers),
    Reference = hd(Timetable),
    lists:map(fun(TimesOfSolver) ->
            Percentages = lists:map(fun({R, T}) ->
                T/R
            end, lists:zip(Reference, TimesOfSolver)),
            printPercentages(Percentages)
        end, Timetable).

% returns runtime of the Solver with the DimacsFile in milliseconds (Int)
runBenchmark(DimacsFile, Solver) ->
    {ok, CNF} = dimacs:parse(DimacsFile),
    {StartMegaSec, StartSec, StartMicroSec} = now(),
    Solver(CNF),
    {StopMegaSec, StopSec, StopMicroSec} = now(),
    (StopMegaSec - StartMegaSec) * helper:pow(10, 6 + 6) + (StopSec - StartSec) * helper:pow(10, 6) + StopMicroSec - StartMicroSec.

printPercentages(Percentages) ->
    lists:map(fun(P) -> io:format("~f%,\t", [P*100]) end, Percentages),
    io:format("\n").
