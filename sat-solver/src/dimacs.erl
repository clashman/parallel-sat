-module(dimacs).

-export([parse/1]).


parse(FileName) when is_list(FileName) ->
    try
        case file:open(FileName, read) of
            {ok, Fd} ->
                {CNF, Variables} = nextLine(Fd, [[]], gb_sets:empty()),
                {ok, {to_sets(CNF), gb_sets:size(Variables)}};
            Error ->
                Error
        end
    catch
        throw:E -> {error, E}
    after
        file:close(FileName)
    end.


nextLine(Fd, CNF, Variables) ->
    Line = case file:read_line(Fd) of
        {ok, Data} -> Data;
        Error -> Error
    end,

    case Line of
        [$c|_] ->  %comment line
            nextLine(Fd, CNF, Variables);
        [$p|_] -> parseProblemLine(Fd, Line, CNF, Variables);
        eof -> case CNF of
                   [[]|Clauses] -> Clauses;
                   _ -> {CNF, Variables}% throw(last_clause_not_terminated_with_0)
               end;
        _  -> parseClause(Fd, Line, CNF, Variables)
    end.


parseProblemLine(Fd, ProblemLine, CNF, Variables) ->
    Elements = string:tokens(ProblemLine, " "),
    case Elements of
        [_, "cnf", _Clauses, _Variables] -> nextLine(Fd, CNF, Variables);
        _ -> throw(unsupported_problem_type)
    end.


parseClause(Fd, Line, CNF, Variables) ->
    Elements = lists:map(fun string_to_integer/1, string:tokens(Line, " \t\r\n")),
    parseVariables(Fd, Elements, CNF, Variables).

parseVariables(Fd, [], CNF, Variables) ->
    nextLine(Fd, CNF, Variables);
parseVariables(Fd, [0|Elements], CNF, Variables) ->
    parseVariables(Fd, Elements, [[]|CNF], Variables);   %new clause
parseVariables(Fd, [Element|Elements], [Clause|Clauses], Variables) ->
    parseVariables(Fd, Elements, [[Element|Clause]|Clauses], gb_sets:add_element(abs(Element), Variables)).




string_to_integer(String) ->
    case string:to_integer(String) of
        {I, _} when is_integer(I) -> I;
        _ -> throw(non_integer_in_clause)
    end.


to_sets(CNF) ->
    ClauseList = lists:map(fun gb_sets:from_list/1, CNF),
    gb_sets:from_list(ClauseList).
