module Utils = {
  let rec buildListOfStack n stacks =>
    switch (n - 1) {
    | (-1) => stacks
    | n => buildListOfStack n [[n], ...stacks]
    };
  let concatPair word n => word ^ " " ^ string_of_int n;
};

module RobotBlock = {
  type blockWorld = list (list int);
  module Commands = {
    type command =
      | MoveOnto
      | MoveOver
      | PileOnto
      | PileOver;
    type commandType =
      | Init int
      | Order command int int
      | Quit;
    let getWords =
      fun
      | MoveOnto => ("Move", "onto")
      | MoveOver => ("Move", "over")
      | PileOnto => ("Pile", "onto")
      | PileOver => ("Pile", "over");
    let print (first, second) a b => Utils.concatPair first a ^ " " ^ Utils.concatPair second b;
  };
  module Actions = {
    let init n => Utils.buildListOfStack n []; /* ] */
  };
  module Render = {
    let displayStack stack =>
      List.fold_left (fun output elem => Utils.concatPair output elem) "" stack |> String.trim;
    let output blockWorld =>
      List.iteri (fun i a => print_endline (string_of_int i ^ ": " ^ displayStack a)) blockWorld;
  };
};

open RobotBlock;

let prnt cmd => Commands.print @@ Commands.getWords @@ cmd;

let executeOrder command a b => prnt command a b;


/**
 * executeCommand: blockWorld => commandType => blockWorld
 *
 * @param blockWorld The intial blockWorld
 * @param cmd        The command to apply
 *
 * @return The new block world
 *
 */
let executeCommand (blockWorld: blockWorld) cmd =>
  switch cmd {
  | Commands.Init n => Actions.init n
  | Commands.Order command a b => blockWorld
  | Commands.Quit => blockWorld
  };

/* Build command list */
let commandList: list Commands.commandType = [
  Commands.Init 10,
  Commands.Order Commands.MoveOnto 2 3,
  Commands.Quit
];

/* Execute all commands and output final blockWorld state */
List.fold_left executeCommand [[]] commandList |> Render.output;
