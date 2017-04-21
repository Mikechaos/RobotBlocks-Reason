module Utils = {
  let rec buildListOfStack n stacks =>
    switch (n - 1) {
      | -1 => stacks
      | n => buildListOfStack n [[n], ...stacks]
    };

  let concatPair word n => word ^ " " ^ string_of_int n ;
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
  };

  module ApplyCommands = {
    let init n => Utils.buildListOfStack n []; /* ] */

    let print (first, second) a b => (Utils.concatPair first a) ^ " " ^ (Utils.concatPair second b);

  };
};

open RobotBlock;
open Commands;
let prnt cmd => ApplyCommands.print @@ getWords @@ cmd;
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
    | Init n => ApplyCommands.init n
    | Order command a b => blockWorld
    | Quit => blockWorld
  };

/* Build command list */
let commandList: list commandType = [Init 10, Order MoveOnto 2 3, Quit];

/* Execute all commands and output final blockWorld state */
List.fold_left executeCommand [[]] commandList;
