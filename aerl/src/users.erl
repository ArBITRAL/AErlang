-module(users).
-compile(export_all).

bof(_, none, _) -> true;
bof([], _, _) -> false;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
      _ -> bof(T, Partner, Y)
  end.
