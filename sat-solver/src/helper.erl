-module(helper).

-compile(export_all).

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
