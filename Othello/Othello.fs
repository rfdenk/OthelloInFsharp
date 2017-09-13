(*
    Play othello!
    WikiHow will tell you all the rules.
    A minute to learn; a lifetime to master!
*)

(*
    Some Game types and utility routines
*)

// 
// Nodes
// Node describes the state of each square on the board
//

type Node = Empty|Black|White

// Convert a string of X, O, and - to an array of nodes.
// This is useful for copying the screen for a test case.
let StringToNodes (s:string) :Node[][] =
    let chars = List.toArray [for c in s -> c]
    let nodes = 
                    [|
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                        [|Empty;Empty;Empty;Empty;Empty;Empty;Empty;Empty;|];
                    |]

    for y in 0..7 do
        for x in 0..7 do
            if chars.[(y*8)+x] = 'X' then nodes.[y].[x] <- Node.Black
            elif chars.[(y*8)+x] = 'O' then nodes.[y].[x] <- Node.White
            else nodes.[y].[x] <- Node.Empty

    nodes



//
// Player
//

type Player = Black|White
let OtherPlayer (p:Player) :Player = match p with |White->Black |Black->White

// Because Nodes can have three states, but Players can only have two, they cannot
// be the same type. We still want to match a disc color with a player color and vice-versa.
let PlayerColor (p:Player) :Node = match p with |Player.Black -> Node.Black |Player.White -> Node.White
let HasPlayerColor (p:Player) (n:Node) :bool = (n = (PlayerColor p))


type PassCount = int
type Log = string list

//
// Game
// node state (W, B, or E), current player (W or B), sequential pass count, log
//

type Game = Node[][] * Player * PassCount * Log

let CurrentPlayer (game:Game) :Player =
    let (_,player,_,_) = game
    player


// Make a new game, ready to play.
let NewGame :Game =

    // This is the initial layout of the nodes.
    let nodes = StringToNodes (
                                "--------" +
                                "--------" +
                                "--------" +
                                "---XO---" +
                                "---OX---" +
                                "--------" +
                                "--------" +
                                "--------"
                                )

    // black plays first; there have been no passes; the log is empty.
    (nodes, Player.Black, 0, [])


// Describe a node as, e.g., "A4"
let LocationToString x y : string =
    string(char((int 'A') + x)) + string(y+1)


// Convert, e.g., "A4" to (0,3). Note that there is no error checking here...
let StringToLocation (loc:string) :int*int =
    let chars = List.toArray [for c in loc -> c]
    let x = (int(chars.[0]) - int('A'))
    let y = (int(chars.[1]) - int('1'))
    (x,y)


// Change the player
let NextTurn (g:Game) :Game =
    let (nodes,player,passcount,log) = g
    match player with |Black->(nodes,White,passcount,log) |White->(nodes,Black,passcount,log)


// We'll be doing a couple of [][] -> [][] mapis, so I'll define a helper for that here.
let mapi2D (mapper:int->int->'a -> 'b) (array:'a[][]): 'b[][] =
    Array.mapi(fun y row -> Array.mapi(fun x cell -> (mapper x y cell)) row) array 

// Ditto for [][] -> 'b folds.
let fold2D (folder:'b->'a->'b) (init:'b) (array:'a[][]) :'b =
    Array.fold(fun acc1 row -> (Array.fold(fun acc2 cell -> folder acc2 cell) acc1 row)) init array


// I hope this is obvious...
let SetNode x y (newColor:Node) (game:Game) :Game =
    let (nodes,player,passcount,log) = game
    // Not the most efficient way to do this, but I want to try it...
    let newNodes = mapi2D(fun xt yt curColor -> if(x=xt && y=yt) then newColor else curColor) nodes
    (newNodes,player,passcount,log)




(*
    These are routines that check to see if a player can flip a row of discs,
    and to does so.
*)

type CanFlipResult = bool*Game  // carry along the game, for pipelining (also to have some fun with some monad stuff).

// a little monad thingy for the CanFlipResult...
let bindCanFlip func =
    fun (cfr:CanFlipResult) ->
        let (b,g) = cfr
        let (b2,g2) = func g
        ((b||b2),g2)


(*
    There is certainly a more "elegant" way to test for flippability and then do so
    all in one smooth pass. And by "elegant", I mean unreadable and dense.
*)

// Scan in a straight line until you hit the edge of the board or an empty square.
// Keep a list of the disc colors that you encounter.
let rec ScanRay x y dx dy (game:Game) : Node list =
    let (nodes,_,_,_) = game
    if(x<0 || x>7 || y<0 || y>7) then []                    // off the edge of the board
    elif(nodes.[y].[x] = Empty) then []                     // encountered an empty square
    else nodes.[y].[x] :: ScanRay (x+dx) (y+dy) dx dy game // recurse & append


// A player can flip in direction <dx,dy> if the first disc in this direction is the _other_ color,
// and some disc in this direction is the same as the player's color.
let CanFlip x y dx dy (game:Game) : CanFlipResult =

    let MatchesPlayerColor = HasPlayerColor (CurrentPlayer game)                    // curry this function

    let sff = ScanRay (x+dx) (y+dy) dx dy game                                      // start scanning at the first disc in the direction.

    if (List.isEmpty sff) then (false,game)                                         // Ran off the edge, or no disc in this direction
    elif (MatchesPlayerColor (List.head sff)) then (false,game)                     // first disc is same color as player -> No flip.
    elif List.exists(fun node -> MatchesPlayerColor node) sff then (true,game)      // first disc is different color, but some other disc is same color
    else (false,game)


// Recurse in direction (dx,dy), flipping all discs to the current player's color.
let rec DoFlipRay x y dx dy (game:Game) :Game =
    let nodes,player,_,_ = game
    if(x<0 || x>7 || y<0 || y>7) then game
    elif(HasPlayerColor player nodes.[y].[x]) then game
    else 
        let newGame = SetNode x y (PlayerColor player) game 
        DoFlipRay (x+dx) (y+dy) dx dy newGame  // recurse to next node.


// comfortable entry point to DoFlipLine.
let FlipRay xP yP dx dy (game:Game) :Game =
    let (can,_) = CanFlip xP yP dx dy game
    if can then DoFlipRay (xP+dx) (yP+dy) dx dy game
    else game
    
// After a player moves (or tries to move), then the game is in
// one of two states: Legal of the resulting game, or Illegal of the original game
type MoveResult =
    | Legal of Game
    | Illegal of Game        // holds the last Legal position!


let bindMoveResult func =
    fun (mr:MoveResult) ->
        match mr with
        | Legal g -> (func g)   // if the game is still legal, move on to the next
        | Illegal g -> mr       // some previous move is illegal, so pass on the last legal move

// Since PlayTurn returns a MoveResult, sometimes we need to extract the Game from it.
let ToGame (mr:MoveResult) = 
    match mr with
    | Illegal g -> g
    | Legal g -> g


// 
// In PlayTurn, the current player places a disc at xP yP.
// Check for legality (node must be empty, and placing disc must flip at least one opponent disc).
// Update the node, perform the flips, reset the passcount, augment the log, and transfer control to the next player.
//

let PlayTurn xP yP (game:Game) :MoveResult =
    let (nodes,player,passcount,log) = game

    let CanFlipD = CanFlip xP yP
    
    let (>>=) (cfr:CanFlipResult) func = bindCanFlip func cfr
    let (>>+) (mr:MoveResult) func = bindMoveResult func mr
    

    let FlipRayD = FlipRay xP yP

    match nodes.[yP].[xP] with
    | Node.White -> Illegal game        // cannot play here; there's already a disc
    | Node.Black -> Illegal game        // ditto.
    | Node.Empty ->
        // put down the disc...
        let gameBeforeFlip = SetNode xP yP (PlayerColor player) game
        // ...check if any opponent discs can be flipped...
        let canflip = gameBeforeFlip 
                    |> CanFlipD 0 1 >>= CanFlipD 0 -1 >>= CanFlipD 1 0 >>= CanFlipD -1 0            // rank & file
                    >>= CanFlipD 1 1 >>= CanFlipD 1 -1 >>= CanFlipD -1 1 >>= CanFlipD -1 -1         // diagonals
        
        match canflip with
        | (false,_) -> Illegal game             // ...if cannot flip any discs, then this is an illegal move, so return the ORIGINAL game!
        | (true,(nodes,player,_,log))->         // ...if can flip, so update game, flip all the discs we can, and hand off to the other player.
                            
            let nextGame = (nodes,player,0,log @ [LocationToString xP yP])         // reset the game's passcount to 0, since we did not Pass
                            |> FlipRayD 1 0 
                            |> FlipRayD -1 0
                            |> FlipRayD 0 1
                            |> FlipRayD 0 -1
                            |> FlipRayD 1 1
                            |> FlipRayD -1 1
                            |> FlipRayD 1 -1
                            |> FlipRayD -1 -1
                            |> NextTurn
    
            Legal nextGame
            

//
// In PassTurn, the current player passes.
// Note that this routine does NOT check for legality.
// Switch players, increment the passcount and append to the log.
//

let PassTurn (game:Game) =
    let nodes,player,pc,log = game
    Legal (nodes, OtherPlayer player, (pc+1), log @ ["##"]) 







// Count the black and white discs, and report the result
let Score (game:Game) :int*int =
    let (nodes,player,pc,log) = game
    fold2D (fun (b,w) node -> 
                            match node with
                            | Node.White -> (b,w+1)
                            | Node.Black -> (b+1,w)
                            | Node.Empty -> (b,w)
            ) (0,0) nodes


// Print the game out on the screen.
// X = Black, O = White
let Show (mr:MoveResult) :Game =
    System.Console.Clear()

    let game = ToGame mr
    let (nodes,player,passcout,log) = game

    match mr with
    | Legal g -> printfn "OK"
    | Illegal g -> printfn "ILLEGAL"

    let game = match mr with | Legal g -> g | Illegal g -> g

    let (scoreB,scoreW) = Score game
    printfn "    A  B  C  D  E  F  G  H      X:%02d to O:%02d" scoreB scoreW

    Array.iteri(fun nr row -> 
            if(nr > 0) then printfn ""
            printfn ""
            printf " %d " (nr+1)
            (Array.iter(fun node -> 
                                    match node with
                                    | Node.White -> printf " O "
                                    | Node.Black -> printf " X "
                                    | Node.Empty -> printf " - "
                                    ) row)
                    
                ) nodes
    printfn ""
    printfn ""

    match player with
    | White -> printfn "O to play"
    | _     -> printfn "X to play"

    printfn ""

    List.iteri(fun i s -> 
        if(0 = (i%8)) then printfn ""
        printf "%s " s
        ) log

    printfn ""
    game


type Move =
    | Play of int*int       // play a disc at x,y
    | Pass                  // pass

let DescribeMove (move:Move) =
    match move with
    | Play(x,y) -> LocationToString x y
    | Pass -> "##"

let ApplyMove (move:Move) (game:Game) :MoveResult =
    match move with
    | Pass -> PassTurn game
    | Play(x,y) -> PlayTurn x y game


//
// This is the core of the recursive tree exploration.
//
// 'player' is the name of the exploring player. 'mover' is the name of the player whose move it is, in the recursion.
// at each depth, we pick the best move for 'mover' (which may be the worst move for 'player')
//
// There isn't any alpha-beta pruning here, because the tree is rarely very wide.
//
// we keep a list of 'moves' along the recursion, just for debugging.
//

let rec EvaluatePosition dbg player (depth:int) (max_depth:int) (game:Game) (moves:string list) : int =

    let (nodes,mover,passcount,log) = game

    if depth = max_depth then 
        let bs,ws = Score game
        let playerscore = match player with |Black->(bs-ws) |_->(ws-bs)
        if dbg then printfn "%A -> %d" moves playerscore
        playerscore


    else
        // yield every possible legal move from this generator.
        
        let (best_move,best_score) = 
            seq {
                // By yielding the "Pass" first, the player will move if the player can move.
                match PassTurn game with
                | Legal g -> yield (Pass,g)
                | _ -> ()                   // If we start checking for legality in PassTurn, then we will start hitting this.

                // Now, try all squares on the board. If it is a legal move, then yield it.
                for x in 0..7 do
                    for y in 0..7 do
                        match (PlayTurn x y game) with 
                        | Legal g -> yield (Play(x,y),g)    
                        | _ -> ()

            } |> Seq.fold (fun (best_move,best_score) (next_move,g) -> 

                                let move_score = EvaluatePosition dbg player (depth+1) max_depth g (moves @ [DescribeMove next_move])     // Keep digging

                                match best_move with
                                | Pass -> 
                                    // Either:
                                    //   First yield (Pass), so just take this as the initial (Pass,score); or
                                    //   Second yield (a Play); therefore player may not Pass, so replace current state w/ new state.
                                    (next_move,move_score)

                                | Play(px,py) -> 
                                    if(player=mover && move_score>best_score) then (next_move,move_score)     // better move for player
                                    elif(player<>mover && move_score<best_score) then (next_move,move_score)  // better move for other player
                                    else (best_move,best_score)

                            ) (Pass,-1000)  // The score here is never used.

        //printfn "At depth %d, best move is %s for score of %d" depth (DescribeAction best_move) best_score
        best_score


//
// This is the standard entry for EvaluatePosition.
// EvaluatePosition just returns a score, but FindBestMove needs to return the Move that generates the best score. 
// Jamming these two functions together resulted in less comprehensible code, so I separated them out.
// The main problem was returning Move*int from EvaluatePosition, because it seemed to require that Move be
// passed in as a parameter, just so we could return it when depth=max_depth
// There is a LOT of duplication here, so maybe there's some way to pull this off...
//

let randomCoin = new System.Random(System.DateTime.Now.Millisecond)

type CoinFlip = |Head |Tail
let FlipCoin : unit->CoinFlip =
    (fun () ->
        if(randomCoin.Next(2) = 0) then 
            Head
        else
            Tail
    )


let FindBestMove dbg onSameScore (max_depth:int) (game:Game): Move =
    let player = CurrentPlayer game

    // Iterate every legal first move, and keep track of the highest-scoring one.
    let (next_move,next_score) =
        seq {
            // Passing is legal, unless there's any legal move.
            match PassTurn game with
            | Legal g -> yield (Pass,g)
            | _ -> ()
        
            for x in 0..7 do
                for y in 0..7 do
                    match (PlayTurn x y game) with 
                    | Legal g -> yield (Play(x,y),g)    
                    | _ -> ()

        } |> Seq.fold (fun (best_move,best_score) (next_move,g) -> 

                        let move_score = EvaluatePosition dbg player 1 max_depth g [DescribeMove next_move]     // EvalPos starts w/ depth=1

                        match best_move with
                        | Pass -> 
                            // Either this is the first yield, in which case we just use the score for PASS as the first score; or
                            // this is the second yield, in which case we replace the best_move with this PLAY (as we must, since
                            // the player must play if there is a legal move).
                            (next_move,move_score)
                        | Play(x,y) ->
                            if(move_score > best_score) then (next_move,move_score)
                            elif (move_score = best_score) then 
                                // This new position has the same score as the best position so far.
                                // 50-50 use the previous best or the new position, just to make the games more interesting.
                                // For repeatability, just replace FlipCoin with something that always produces Head or always produces Tail
                                match onSameScore () with
                                | Head -> 
                                    (best_move,best_score)
                                | Tail -> 
                                    (next_move,move_score)
                            else (best_move,best_score)

                   ) (Pass,-1000)

    next_move





//
// Let the computer play itself.
// Black and White can be given different strengths, so that one will win.
//
let rec PlayYourself onSameScore num_moves_left (g:Game) : Game =
    System.Threading.Thread.Sleep(200)

    let nodes,player,passcount,log = g
    if num_moves_left = 0 then g
    elif passcount > 1 then g           // here's why we keep track of passcount!
    else
        let strength = match player with |Black->5 |White->5
        let best_move = FindBestMove false onSameScore strength g     
        g |> ApplyMove best_move |> Show |> PlayYourself onSameScore (num_moves_left-1)    // perform the action, show the result, and keep playing!



// Just for dev & debugging. Lets you say PlaceAt "A5"
let PlaceAt (at:string) (g:Game) :MoveResult =
    let x,y = StringToLocation at
    if(x<0 || x>7 || y<0 || y>7) then Illegal g     // illegal location; just bail.
    else PlayTurn x y g


let cointoss = 
    let R = System.Random(System.DateTime.Now.Millisecond)
    (fun n -> R.Next n)

[<EntryPoint>]
let main argv = 

    // Built-in tests
    let TestCases = [|
                           ("OOOOOOOO"+
                            "XOOOOOO-"+
                            "OOOOOOOO"+
                            "OOOOOOOO"+
                            "OOOOOOOO"+
                            "OOOOOOOO"+
                            "XOOOOO--"+
                            "OOO-OOOO", Black, 3, "H2");        // black plays H2 to force white to pass.
                            
                           ("XXXXOOOO"+
                            "XXXXXXOO"+
                            "XXXXOOOO"+
                            "XXXXOXOO"+
                            "XXXXXOOO"+
                            "XXXXXXOO"+
                            "XXXXXXOO"+
                            "XXXOOOO-", White, 1, "PASS");      // this one and the next two: white must pass.

                           ("XXXXOOOO"+
                            "XXXXXXOO"+
                            "XXXXOOOO"+
                            "XXXXOXOO"+
                            "XXXXXOOO"+
                            "XXXXXXOO"+
                            "XXXXXXOO"+
                            "XXXOOOO-", White, 2, "PASS");

                           ("XXXXOOOO"+
                            "XXXXXXOO"+
                            "XXXXOOOO"+
                            "XXXXOXOO"+
                            "XXXXXOOO"+
                            "XXXXXXOO"+
                            "XXXXXXOO"+
                            "XXXOOOO-", White, 3, "PASS");

                           ("XOOOOOOO"+
                            "XXXXXXXX"+
                            "XOOOOXOX"+
                            "OOOOOOXX"+
                            "OOOOOOOX"+
                            "OOOOOXOX"+
                            "OOOOOOOX"+
                            "OOOOOOO-", White, 3, "H8");        // there is only one spot for white to move.

                           ("XXXXXXXX"+
                            "XXXXOOOO"+
                            "XOXOXXOO"+
                            "OOOXOXO-"+
                            "XXXXXOXX"+
                            "XXXXXXOX"+
                            "XXOOXX-O"+
                            "XXXXXX--", Black, 3, "H4");

                           ("OOOOO-X-"+
                            "OOOO-X--"+
                            "OXXXX---"+
                            "OOOOO---"+
                            "OOOOOO--"+
                            "OOOOOOOX"+
                            "OOOOO-XO"+
                            "OOOOOX--", Black, 2, "G5");

                           ("OOOOO-X-"+
                            "OOOO-X--"+
                            "OXXXX---"+
                            "OOOOO---"+
                            "OOOOOO--"+
                            "OOOOOOOX"+
                            "OOOOO-XO"+
                            "OOOOOX--", Black, 3, "F7");        // same position as previous, but search one layer deeper

                           ("--------"+
                            "--------"+
                            "--------"+
                            "---XXX--"+
                            "--XXO---"+
                            "----O---"+
                            "--------"+
                            "--------", White, 3, "E3");
                    |]

    let TestFBM = FindBestMove false (fun() -> Head)   // make true to print out the scores, always use "Head" for testing.

    let testresult = 
        Array.fold( fun (pass,log) test ->
                    let (nodes, player, depth, expected) = test
                    let game =((StringToNodes nodes), player, 0, [])
                    let bm = TestFBM depth game
                    let result = match bm with
                                 | Pass -> "PASS"
                                 | Play(x,y) -> (LocationToString x y)

                    if(System.String.Equals(result,expected)) then 
                        (true, log)
                    else 
                        (false, log @ ["expected " + expected + "; got " + result ])

                  ) (true, []) TestCases

    
    let return_code = 
        match testresult with
        | false, log -> 
            printfn "Test pass failed: %A" log
            -1
        | true, _ -> 
            // BIT passed; go ahead and play.
            let sx = Legal NewGame |> Show
            let d = PlayYourself FlipCoin 64 sx
            0


    printf "Press any key to exit > "
    System.Console.ReadKey() |> ignore

    return_code