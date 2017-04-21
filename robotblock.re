module RobotBlock = {
  type blockWorld = array (list int);

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


    let rec _init n acc =>
      switch (n - 1) {
        | -1 => acc
        | n => _init n [[n], ...acc]
      };
    let init n => _init n [];

};

open RobotBlock;

let concatPair word n => word ^ " " ^ string_of_int n ;
let printCommand (first, second) a b => (concatPair first a) ^ " " ^ (concatPair second b);

let prnt cmd => printCommand @@ getWords @@ cmd;
let executeOrder command a b => prnt command a b;

let executeCommand =
  fun
  | Init n => concatPair "Init" n
  | Order command a b => executeOrder command a b
  | Quit => "Quit!";

let commandList: list commandType = [Init 10, Order MoveOnto 2 3, Quit];
List.map executeCommand commandList;