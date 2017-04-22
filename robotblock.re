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
  type blockWorld = list (list int);
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
    /* The list of all possible commands */
    type commandType =
      | Init int
      | Order command int int
      | Quit;
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
  };
  module Parser = {
    let break cmd => Str.split (Str.regexp " +") cmd;
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
    let exec program => program |> List.map break |> List.map parseCommand;
  };
  /* All actions to execute on the block world */
  module Actions = {
    let init n => Utils.buildListOfStack n []; /* ] */
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
    let output blockWorld => List.iteri displayLine blockWorld;
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
/* Arbitrary command list */
