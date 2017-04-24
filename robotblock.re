module Utils = {
  /*
   * Builds a list of n stacks
     Each stack is initialized with their index
     3 => [] => [[0],[1],[2]]
   */
  let rec buildListOfStack n stacks =>
    switch (n - 1) {
    | (-1) => stacks
    | n => buildListOfStack n [[n], ...stacks]
    };
  /*
   * Concat a string and a int, separated by a space
   * "move" => 2 => "move 2"
   */
  let concatPair word n => word ^ " " ^ string_of_int n;
  let reverse l => List.fold_left (fun l2 e => [e, ...l2]) [] l;
};

module MaybeInt = {
  type t =
    | Int int
    | String string;
  let makeInt =
    fun
    | Int a => a
    | String s => int_of_string s;
};

module RobotBlock = {
  /* The block world is a list of stacks containing ints */
  type blockStack = {position: int, stack: list int};
  type blockWorld = list blockStack;
  /* The list of specific action commands */
  module Commands = {
    type instruction =
      | Onto
      | Over;
    type action 'intstruction =
      | Move instruction
      | Pile instruction
      | InvalidInstruction;
    type command = action instruction;
    type commands =
      | List command;
    /* The list of all possible commands */
    type commandType =
      | Init int
      | Order command int int
      | Quit;
    type program =
      | List commandType program
      | NoMore;
    type state =
      | BlockWorld blockWorld
      | BlockWorldProcessor blockWorld program;
    /* Map all action commands to their string equivalent */
    let mapWords =
      fun
      | Move Onto => ("Move", "onto")
      | Move Over => ("Move", "over")
      | Pile Onto => ("Pile", "onto")
      | Pile Over => ("Pile", "over")
      | InvalidInstruction => ("Invalid", "Instruction");
    /* Print an action command */
    let print (first, second) a b => Utils.concatPair first a ^ " " ^ Utils.concatPair second b;
  };
  module Make = {
    open Commands;
    let init n => Init n;
    let initOfString s => init (int_of_string s);
    let order command a b => Order command a b;
    let quit = Quit;
    let makeCommand =
      fun
      | ["move", "onto"] => Move Onto
      | ["move", "over"] => Move Over
      | ["pile", "onto"] => Pile Onto
      | ["pile", "over"] => Pile Over
      | _ => Commands.InvalidInstruction;
    let orderOfString command a b => Order command (int_of_string a) (int_of_string b);
    let orderOfTokens =
      fun
      | [action, a, instruction, b] => orderOfString (makeCommand [action, instruction]) a b
      | _ => Quit;
    let state world =>
      fun
      | NoMore => Commands.BlockWorld world
      | List cmd rest => Commands.BlockWorldProcessor world (Commands.List cmd rest);
  };
  module Parser = {
    let break cmd => Str.split (Str.regexp " +") cmd;
    let breakList l => List.map break l;
    let print token => List.iter (fun s => print_string s) token;
    let parseSingleCommandType =
      fun
      | "quit" => Commands.Quit
      | _ as n => Make.initOfString n;
    let parseCommand =
      fun
      | [token] => parseSingleCommandType token
      | [action, a, instruction, b] as tokens => Make.orderOfTokens tokens
      | _ => Commands.Quit;
    let parseCommandList l => List.map parseCommand l;
    let makeInitialState p =>
      List.fold_left (fun rest cmd => Commands.List cmd rest) Commands.NoMore (Utils.reverse p);
    let exec program => program |> breakList |> parseCommandList |> makeInitialState;
  };
  /* All actions to execute on the block world */
  module ActionHelpers = {
    let find b world =>
      List.fold_left
        (fun index {position, stack} => List.exists (fun x => x == b) stack ? position : index)
        1
        world;
    let splitStack s n => {
      let rec splitRec found l (s1, s2) =>
        switch l {
        | [] => (Utils.reverse s1, Utils.reverse s2)
        | [e, ...rest] =>
          found ?
            splitRec true rest (insertRight e (s1, s2)) :
            e == n ?
              splitRec true rest (insertLeft e (s1, s2)) :
              splitRec false rest (insertLeft e (s1, s2))
        }
      and insertLeft e (s1, s2) => ([e, ...s1], s2)
      and insertRight e (s1, s2) => (s1, [e, ...s2]);
      splitRec false s ([], [])
    };
    let takeStack (stack, unstack) => stack;
    let takeUnstack (stack, unstack) => unstack;
    let push n p world =>
      List.map
        (
          fun ({position, stack: currentStack} as record) =>
            position == p ?
              List.fold_left
                (fun {position, stack} e => {position, stack: [e, ...stack]})
                {position, stack: [n]}
                (Utils.reverse record.stack) :
              record
        )
        world;
    let pop n p world =>
      List.map
        (
          fun ({position, stack: currentStack} as record) =>
            position == p ?
              List.fold_left
                (fun {position, stack} e => {position, stack: n == e ? stack : [e, ...stack]})
                {position, stack: []}
                (Utils.reverse record.stack) :
              record
        )
        world;
    let runstack b p world =>
      List.fold_left
        (
          fun finalStack {position, stack} =>
            position == p ? b |> splitStack stack |> takeUnstack : finalStack
        )
        []
        world;
    let restack s world => List.fold_left (fun world x => push x x world) world s;
    let unstack n p world => {
      let newWorld =
        List.map
          (
            fun ({position, stack} as record) =>
              position == p ? {position, stack: n |> splitStack stack |> takeStack} : record
          )
          world;
      let currentUnstack = runstack n p world;
      newWorld |> restack currentUnstack
    };
    let move a positionA positionB world => world |> push a positionB |> pop a positionA;
  };
  module Actions = {
    exception MalFormedStack (string, list int);
    let indexStack =
      fun
      | [position, ...rest] as stack => {position, stack}
      | _ as stack => raise (MalFormedStack ("Stack is malformed", stack));
    let mapIndexStack world => List.map indexStack world;
    let init n => [] |> Utils.buildListOfStack n |> mapIndexStack;
    let moveOnto a b world => {
      let positionA = ActionHelpers.find a world;
      let positionB = ActionHelpers.find b world;
      world |> ActionHelpers.unstack a positionA |> ActionHelpers.unstack b positionB |>
      ActionHelpers.move a positionA positionB
    };
    let moveOver a b world => {
      let positionA = ActionHelpers.find a world;
      let positionB = ActionHelpers.find b world;
      world |> ActionHelpers.unstack a positionA |> ActionHelpers.move a positionA positionB
    };
  };
  /* Allow to render the block world */
  module Render = {
    /* Allow to display a stack of blocks separated by a space
     * [1,2,3] => "1 2 3"
     */
    let displayStack stack => List.fold_left Utils.concatPair "" stack |> String.trim;
    /* Displays "i: "
     * 1 => "1: "
     */
    let displayIndex i => string_of_int i ^ ": ";
    /* Display a formatted stack
     * 1 => [1,2,3] => "1: 1 2 3"
     */
    let displayLine i stack => print_endline (displayIndex i ^ displayStack stack);
    /* Renders the block world
     * [[0],[1,2],[]] =>
     * 0: 0
     * 1: 1 2
     * 2:
     */
    let deIndexStack world => List.map (fun {position, stack} => stack) world;
    let output blockWorld => List.iteri displayLine (blockWorld |> deIndexStack);
    /* String conterpart of a RobotBlock.CommandType
     * Only there for convenience
     * Move 2 3 => "Move 2 onto 3"
     */
    let prnt cmd => Commands.print @@ Commands.mapWords @@ cmd;
  };
  module Processor = {
    /* Takes a blockWorld and a list of commands
     * Returns the new blockWorld
     * blockWorld => commandType => blockWorld
     * [[]] => Init 3 => [[1],[2],[3]]
     *
     */
    let executeCommand (blockWorld: blockWorld) cmd =>
      switch cmd {
      | Commands.Init n => Actions.init n
      | Commands.Order command a b => blockWorld
      | Commands.Quit => blockWorld
      };
    /* Execute all commands and output final blockWorld state */
    let executeProgram commandList =>
      List.fold_left executeCommand [[]] commandList |> Render.output;
  };
};

open RobotBlock;

let myState =
  Commands.BlockWorldProcessor myBlockWorld (Commands.List (Make.init 10) Commands.NoMore);

module type WorldProcessor = {let process: Commands.state => blockWorld;};

module Execute = {
  let extractWorld =
    fun
    | Commands.BlockWorld blockWorld => blockWorld
    | Commands.BlockWorldProcessor blockWorld cmds => blockWorld;
  let extractCommand =
    fun
    | Commands.BlockWorld blockWorld => Commands.NoMore
    | Commands.BlockWorldProcessor blockWorld cmd => cmd;
  /* | Commands.BlockWorldProcessor blockWorld Commands.NoMore => Commands.NoMore */
  /* | Commands.BlockWorldProcessor blockWorld (Commands.Command cmd) => Commands.Command cmd */
  /* | Commands.BlockWorldProcessor blockWorld (Commands.List cmd _) => Commands.Command cmd; */
  let rec process state => {
    let (world, command) = (extractWorld state, extractCommand state);
    Render.output world;
    /* let rec execCommand => */
    switch command {
    | Commands.NoMore => print_endline "No more commands to execute! :("
    | Commands.List cmd rest =>
      let newWorld =
        switch cmd {
        | Commands.Init n =>
          print_endline ("I will happily execute this init " ^ string_of_int n ^ " command!");
          world
        | Commands.Order order a b =>
          switch order {
          | Commands.Move Commands.Onto =>
            print_endline ("OK Chief! Moving " ^ string_of_int a ^ " onto " ^ string_of_int b);
            world
          | Commands.Move Commands.Over =>
            print_endline ("OK Chief! Moving " ^ string_of_int a ^ " over " ^ string_of_int b);
            world
          | Commands.Pile Commands.Onto =>
            print_endline ("OK Chief! Piling " ^ string_of_int a ^ " onto " ^ string_of_int b);
            world
          | Commands.Pile Commands.Over =>
            print_endline ("OK Chief! Piling " ^ string_of_int a ^ " over " ^ string_of_int b);
            world
          | _ =>
            print_endline "I don't take your orders";
            world
          }
        | Commands.Quit =>
          print_endline "Already leaving???";
          world
        };
      process (Commands.BlockWorldProcessor newWorld rest);
      print_endline "I need to recurse!"
    };
    state
  };
};

let p =
  Parser.exec ["10", "move 0 onto 1", "move 2 over 1", "pile 1 onto 4", "pile 7 over 0", "quit"];

let p2 = List.fold_left (fun p3 c => Commands.List c p3) Commands.NoMore (Utils.reverse p);

let myState3 = Commands.BlockWorldProcessor myBlockWorld p2;

Execute.process myState3;
/* Arbitrary command list */
