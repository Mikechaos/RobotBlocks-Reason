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

module RobotBlock = {
  /* The block world is a list of stacks containing ints */
  type blockStack = {position: int, stack: list int};
  type blockWorld = list blockStack;
  module Exceptions = {
    exception MalFormedStack (string, list int);
  };
  /* The list of specific action commands */
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *             Grammar           * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Defines the Grammar
   */
  module Grammar = {
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
    type robot =
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
  };
  open Grammar;
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *           Helpers             * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Perform manipulations on the block world
   */
  module Helpers = {
    let push n p world =>
      /* TODO - Refactor these List.map List.fold patterns
       * The whole block world manipulation needs some love
       * This is some simple matrix manipulation
       */
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
    /* */
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
    let initialUnstack = ([], []);
    let splitStackIfFound p b finalStack {position, stack} =>
      position == p ? splitStack stack b : finalStack;
    let splitStackInWorld b p => List.fold_left (splitStackIfFound p b) initialUnstack;
    let replaceStack world {position: p, stack: s} =>
      List.map (fun ({position, stack} as s2) => position == p ? {position, stack: s} : s2) world;
    let restack world s => List.fold_left (fun world x => push x x world) world s;
    let isBlock b block => b == block;
    let blockIsInStack block index {position, stack} =>
      List.exists (isBlock block) stack ? position : index;
    let find block => List.fold_left (blockIsInStack block) 0;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *      PartialOperiation        * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Allow to use a PartialOperation type
   * to apply a list of simple operation on the block world,
   * (during which it is in an invalid state)
   * until reach fsing completion,
   * (the next valid state)
   * The chain of operationw from one valid state to the next
   * can represent any desired valid command on the block world
   */
  module PartialOperations = {
    module Type = {
      type block = {block: int, position: int};
      type annotation =
        | Unstack (blockStack, list int)
        | Restack (list int)
        /* | Push int int */
        /* | Pop int int; */
        | Move block int;
      type operation =
        | PartialOperation blockWorld annotation
        | Completed blockWorld;
    };
    open Type;
    module Make = {
      let unstack world stackPair => PartialOperation world (Unstack stackPair);
      let restack unstack world => PartialOperation world (Restack unstack);
      let move world block position => PartialOperation world (Move block position);
      let completed world => Completed world;
    };
    module Build = {
      let makeUnstack world position (stack, unstack) =>
        Make.unstack world ({position, stack}, unstack);
      let unstack b p world => world |> Helpers.splitStackInWorld b p |> makeUnstack world p;
      let move block position positionB world => Make.move world {block, position} positionB;
    };
    module Process = {
      let matchAnnotations world =>
        fun
        | Unstack (blockStack, unstack) =>
          blockStack |> Helpers.replaceStack world |> Make.restack unstack
        | Restack unstack => unstack |> Helpers.restack world |> Make.completed
        | Move {block: blockA, position: positionA} positionB =>
          world |> Helpers.push blockA positionB |> Helpers.pop blockA positionA |> Make.completed;
      let rec matchOperations =
        fun
        | PartialOperation world annot => annot |> matchAnnotations world |> matchOperations
        | Completed world => world;
    };
    /*
     * Public interface
     */
    let unstack b p world => Build.unstack b p world |> Process.matchOperations;
    let move a positionA positionB world =>
      world |> Build.move a positionA positionB |> Process.matchOperations;
    let find b => Helpers.find b;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *                Make           * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Makers for the various block world types
   */
  module Make = {
    open Grammar;
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
      | _ => Grammar.InvalidInstruction;
    let orderOfString command a b => Order command (int_of_string a) (int_of_string b);
    let orderOfTokens =
      fun
      | [action, a, instruction, b] => orderOfString (makeCommand [action, instruction]) a b
      | _ => Quit;
    let commandList rest cmd => Grammar.List cmd rest;
    let robot world =>
      fun
      | NoMore => Grammar.BlockWorld world
      | List cmd rest => Grammar.BlockWorldProcessor world (Grammar.List cmd rest);
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *            Parser             * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Parse the input
   */
  module Parser = {
    let break cmd => Str.split (Str.regexp " +") cmd;
    let breakList l => List.map break l;
    let print token => List.iter (fun s => print_string s) token;
    let parseSingleCommandType =
      fun
      | "quit" => Grammar.Quit
      | _ as n => Make.initOfString n;
    let parseCommand =
      fun
      | [token] => parseSingleCommandType token
      | [action, a, instruction, b] as tokens => Make.orderOfTokens tokens
      | _ => Grammar.Quit;
    let parseCommandList l => List.map parseCommand l;
    let makeInitialState p => p |> Utils.reverse |> List.fold_left Make.commandList Grammar.NoMore;
    let exec program => program |> breakList |> parseCommandList |> makeInitialState;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *         Action Helpers        * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Helpers for actions to execute on the block world
   */
  module ActionHelpers = {
    let indexStack =
      fun
      | [position, ...rest] as stack => {position, stack}
      | _ as stack => raise (Exceptions.MalFormedStack ("Stack is malformed", stack));
    let mapIndexStack world => List.map indexStack world;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *           Action              * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Allow to render the block world
   */
  module Actions = {
    let init n => [] |> Utils.buildListOfStack n |> ActionHelpers.mapIndexStack;
    /* TODO - Define a former grammar for all sub actions
     * Would be awesome to be able to write something like
     * let moveOnto a b world => Find a And Find B And Unstack a And Unstack b And Move a b
     * I don't know how doable this is, namely because the current memoization (positionA, positionB)
     * Definitely work exploring though
     */
    let moveOnto a b world => {
      let positionA = PartialOperations.find a world;
      let positionB = PartialOperations.find b world;
      world |> PartialOperations.unstack a positionA |> PartialOperations.unstack b positionB |>
      PartialOperations.move a positionA positionB
    };
    let moveOver a b world => {
      let positionA = PartialOperations.find a world;
      let positionB = PartialOperations.find b world;
      world |> PartialOperations.unstack a positionA |>
      PartialOperations.move a positionA positionB
    };
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *           Render              * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Allow to render the block world
   */
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
    let id blockWorld =>
      fun
      | _ => blockWorld;
    let output blockWorld => blockWorld |> deIndexStack |> List.iteri displayLine |> id blockWorld;
    /* String conterpart of a RobotBlock.CommandType
     * Only there for convenience
     * Move 2 3 => "Move 2 onto 3"
     */
    /* Print an action command */
    let print (first, second) a b => Utils.concatPair first a ^ " " ^ Utils.concatPair second b;
    let prnt cmd => print @@ Grammar.mapWords @@ cmd;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * *                               * * *
   * * *           Execute             * * *
   * * *                               * * *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   */
  module Execute = {
    open Grammar;
    let feed program => Make.robot [] program;
    let processOrder a b world =>
      fun
      | Move Onto => Actions.moveOnto a b world
      | Move Over => Actions.moveOver a b world
      | Pile Onto => world /* TODO implement pile onto */
      | Pile Over => world /* TODO implement pile over */
      | _ => world;
    let processList world rest =>
      fun
      | Init n => Make.robot (Actions.init n) rest
      | Order order a b => Make.robot (processOrder a b world order) rest
      | Quit => Make.robot world NoMore;
    let rec process =
      fun
      | BlockWorld world => world
      | BlockWorldProcessor world NoMore => world
      | BlockWorldProcessor world (List cmd rest) => process (processList world rest cmd);
  };
};

open RobotBlock;

let program = [
  "15",
  "move 0 onto 1",
  "move 2 over 0",
  "move 1 onto 4",
  "move 7 over 0",
  "move 5 onto 4",
  "move 3 over 2",
  "move 6 over 1",
  "move 9 over 0",
  "move 14 over 0",
  "move 12 over 0",
  "move 11 over 0",
  "move 13 over 0",
  "move 10 over 0",
  "quit"
];

let world = program |> Parser.exec |> Execute.feed |> Execute.process |> Render.output;
