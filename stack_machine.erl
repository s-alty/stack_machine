-module(stack_machine).
-export([example/0, function_call_example/0, run/2]).

-record(machine, {stack, memory, function_table}).
-record(function, {nparams, instructions}).

run(Instructions, FunctionTable) ->
    Machine = #machine{stack=[], memory=array:new([{size, 8192}, {default, 0}]), function_table=FunctionTable},
    run(Instructions, Machine, array:new()).

run([], Machine, _) -> Machine;
run([Instruction|Rest], Machine=#machine{stack=Stack, memory=Memory, function_table=Functions}, Locals) ->
    io:format("~w~n", [Stack]),
    case Instruction of
        {const, C} -> run(Rest, Machine#machine{stack=[C|Stack]}, Locals);

        add ->
            [Right,Left|Stack2] = Stack,
            run(Rest, Machine#machine{stack=[Left+Right|Stack2]}, Locals);

        mul ->
            [Right,Left|Stack2] = Stack,
            run(Rest, Machine#machine{stack=[Left*Right|Stack2]}, Locals);

        load ->
            %% use top of the stack as address into memory
            %% push value at address onto the stack
            [Addr|Stack2] = Stack,
            Val = array:get(Addr, Memory),
            run(Rest, Machine#machine{stack=[Val|Stack2]}, Locals);

        store ->
            %% use top of the stack as a value and then an address
            %% set memory at address to value
            [Val,Addr|Stack2] = Stack,
            run(Rest, Machine#machine{stack=Stack2, memory=array:set(Addr, Val, Memory)}, Locals);

        {local_get, Idx} ->
            %% Pull a value from local variables and put it on the stack
            %% used for function calls
            Val = array:get(Idx, Locals),
            run(Rest, Machine#machine{stack=[Val|Stack]}, Locals);

        {local_set, Idx} ->
            %% Pull a value off of the stack and update the local variables
            [Val|Stack2] = Stack,
            run(Rest, Machine#machine{stack=Stack2}, array:set(Idx, Val, Locals));

        {call, FuncTableIdx} ->
            #function{nparams=Nparams, instructions=FunctionsInstructions} = lists:nth(FuncTableIdx+1, Functions),
            %% pull off the top Nparams values from the stack to use as locals in the function call
            %% they're in reverse order
            {Params, Stack2} = lists:split(Nparams, Stack),
            FunctionLocals = array:from_list(lists:reverse(Params)),
            %% recurse with the function's instructions
            Machine2 = run(FunctionsInstructions, Machine#machine{stack=Stack2}, FunctionLocals),

            run(Rest, Machine2, Locals)
        end.

%% for conditionals, what if we spawn a new erlang process to run each branch?
%% they can each get a copy of the machine since it's purely functional


example() ->
    %% x = 2
    %% v = 3
    %% x = x + v*0.1
    Xaddr = 1337,
    Vaddr = 4567,
    run(
      [
       {const, Xaddr},
       {const, Xaddr},
       {const, 2},
       store,
       {const, Vaddr},
       {const, 3},
       store,
       {const, 0.1},
       {const, Vaddr},
       load,
       mul,
       {const, Xaddr},
       load,
       add,
       store
      ],
      array:new()
     ).

function_call_example() ->
    %% f (x, v, dt) = x + v * dt
    Function = #function{
                  nparams=3,
                  instructions=[
                                {local_get, 0},
                                {local_get, 1},
                                {local_get, 2},
                                mul,
                                add
                               ]
                  },
    FunctionTable = [Function],
    %% run two iterations
    Program = [
               {const, 2},
               {const, 3},
               {const, 0.1},
               {call, 0},
               {const, 3},
               {const, 0.1},
               {call, 0}
              ],
    run(Program, FunctionTable).
