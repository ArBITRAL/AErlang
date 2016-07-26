-module(test1).
-compile(export_all).

start() ->
    spawn(fun() ->
		  loop() end).

loop() ->
    A = fun(Func) ->
	    receive
		{get, X} ->
		    receive
			{set, Y} when (X == Y) ->
			    {found, X}
		    after 0 ->
			    self() ! {get, X}, Func(Func)
		    end;
		{set, X} ->
		    receive
			{get, Y} when (X == Y) ->
			    {found, X}
		    after 0 ->
			    self() ! {set, X}, Func(Func)
		    end
	    end
	end,
    A(A).
