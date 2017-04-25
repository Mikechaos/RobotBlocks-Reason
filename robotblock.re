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
  /* The list of specific action commands */
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   *                                       *
   *                 Grammar               *
   *                                       *
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
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   *                                       *
   *                    Make               *
   *                                       *
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
   *                                       *
   *                Parser                 *
   *                                       *
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
   *                                       *
   *             Action Helpers            *
   *                                       *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   * Helpers for actions to execute on the block world
   */
  module ActionHelpers = {
    let find b world =>
      List.fold_left
        (fun index {position, stack} => List.exists (fun x => x == b) stack ? position : index)
        0
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
    /* TODO - This portion needs complete rework.
     * unstack is ugly and uselessly inefficient
     * I need to create a partial operation type
     * allowing to chain unfinished operation one into another
     */
    let replaceStack world {position: p, stack: s} =>
      List.map (fun ({position, stack} as s2) => position == p ? {position, stack: s} : s2) world;
    let restack s world => List.fold_left (fun world x => push x x world) world s;
    let restack2 world s => restack s world;
    type annotation =
      | Unstack (blockStack, list int)
      | Restack (list int);
    type operation =
      | PartialOperation blockWorld annotation
      | Completed blockWorld;
    let getUnstackNice b p world =>
      List.fold_left
        (fun finalStack {position, stack} => position == p ? splitStack stack b : finalStack)
        ([], [])
        world |> (
        fun (stack, unstack) =>
          PartialOperation world (Unstack ({position: p, stack}, unstack)) /* TODO should be declared in Make*/
      );
    let makeRestack unstack world => PartialOperation world (Restack unstack);
    let makeCompleted world => Completed world;
    let matchAnnotations world =>
      fun
      | Unstack (stack, unstack) => stack |> replaceStack world |> makeRestack unstack
      | Restack unstack => unstack |> restack2 world |> makeCompleted;
    let rec matchOperations =
      fun
      | PartialOperation world annot => annot |> matchAnnotations world |> matchOperations
      | Completed world => world;
    let unstack b p world => getUnstackNice b p world |> matchOperations;
    let move a positionA positionB world => world |> push a positionB |> pop a positionA;
    exception MalFormedStack (string, list int);
    let indexStack =
      fun
      | [position, ...rest] as stack => {position, stack}
      | _ as stack => raise (MalFormedStack ("Stack is malformed", stack));
    let mapIndexStack world => List.map indexStack world;
  };
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   *                                       *
   *               Action                  *
   *                                       *
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
  /*
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   *                                       *
   *               Render                  *
   *                                       *
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
   *                                       *
   *               Execute                 *
   *                                       *
   * * * * * * * * * * * * * * * * * * * * *
   * * * * * * * * * * * * * * * * * * * * *
   */
  module Execute = {
    let feed program => Make.robot [] program;
    let processOrder a b world =>
      fun
      | Grammar.Move Grammar.Onto => Actions.moveOnto a b world
      | Grammar.Move Grammar.Over => Actions.moveOver a b world
      | Grammar.Pile Grammar.Onto => world /* TODO implement pile onto */
      | Grammar.Pile Grammar.Over => world /* TODO implement pile over */
      | _ => world;
    let processList world rest =>
      fun
      | Grammar.Init n => Make.robot (Actions.init n) rest
      | Grammar.Order order a b => Make.robot (processOrder a b world order) rest
      | Grammar.Quit => Make.robot world Grammar.NoMore;
    let rec process =
      fun
      | Grammar.BlockWorld world => world
      | Grammar.BlockWorldProcessor world Grammar.NoMore => world
      | Grammar.BlockWorldProcessor world (Grammar.List cmd rest) =>
        process (processList world rest cmd);
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
