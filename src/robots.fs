(*to do: debug
1: East Wall can't hold robt
2:frame and robot interact the same time.(south frame)
3.exceptions
4:when robot already at the frame try to braek the frame
5: test the goal
*)

//to do: change the way to choose direction, use readkey


module robots

type Status = bool*bool*(string option)//RightWall*BottomWall*str


type BoardDisplay(rows:int,cols:int) =
   let fields:Status [,] = Array2D.create (rows+1) (cols+1) (false,false,None)

   member this.Set (row:int) (col:int) (str:string) =
       let (a,b,c) = fields.[row,col]
       fields.[row,col] <-(a,b,Some str)
   member this.SetBottomWall (row:int) (col:int) =
       let (a,b,c) = fields.[row,col]
       fields.[row,col] <- (a,true,c)
       
   member this.SetRightWall (row:int) (col:int) =
       let (a,b,c) = fields.[row,col]
       fields.[row,col] <- (true,b,c)
       
   member private this.SetFrame() =
       for j = 1 to cols do
           this.SetBottomWall 0 j
           this.SetBottomWall rows j
       for i = 1 to rows do
           this.SetRightWall i 0
           this.SetRightWall i cols
   member this.Show() =
       printfn "%s" (this.ToString())

   override this.ToString() =
       this.SetFrame() 
       let mutable str =""
       for i = 0 to rows do
           for j=0 to cols do
           // here is the string line,don't think about bottom wall
              match fields.[i,j] with
                  |(false,_,Some s) -> str <- str + ( sprintf "%s" (s+" "))
                  |(true,_,None) -> str <- str + ( sprintf "%s" "  |")
                  |(true,_,Some s) -> str <- str + ( sprintf "%s" (s+"|"))
                  | _-> str <- str + (sprintf "%s" "   ")
           str <- str + "\n"
            // here is the seperate line,only bottom wall matters
           for j=0 to cols do
              match fields.[i,j] with
                |(_,true,_) -> str <- str + ( sprintf "%s" "--+")
                |_-> str <- str + ( sprintf "%s" "  +")
           str <- str + "\n"               
       str


//11g1

type Position = int * int

type Direction = North | South | East | West
type Action =
   |Stop of Position
   |Continue of Direction * Position
   |Ignore

[< AbstractClass >]
type BoardElement () =
  abstract member RenderOn : BoardDisplay -> unit
  abstract member Interact : Robot -> Direction -> Action
  default __.Interact _ _ = Ignore
  abstract member GameOver : Robot list -> bool
  default __.GameOver _ = false
and Robot ( row : int , col : int , name : string ) =
  inherit BoardElement ()
  let mutable position = (row,col)
  member this.Position
          with get () = position
          and set (a) = position <- a
  override this.Interact other dir =
   if this = other then Ignore
   elif position = other.Position then
          match dir with
            |East -> Stop (fst position,(snd position)-1)
            |West -> Stop (fst position,(snd position)+1)
            |North -> Stop ((fst position)+1,snd position)
            |South -> Stop ((fst position)-1,snd position)
   else
         Ignore
     (*let (r0,c0) = position
     let (r1,c1) = other.Position
     match (r1,c1) with
       |(r0,n) when n = c0-1-> match dir with
                                  |East -> Stop other.Position
                                  |_-> Ignore
       |(r0,n) when n = c0+1 -> match dir with
                                  |West -> Stop other.Position
                                  |_ -> Ignore
       |(n,c0) when n = r0-1-> match dir with
                                  |South -> Stop other.Position
                                  |_-> Ignore
       |(n,c0) when n = r0+1 -> match dir with
                                  |North -> Stop other.Position
                                  |_-> Ignore
       |_-> Ignore *)
  override this.RenderOn display =
       display.Set (fst position) (snd position) name
       
  member val Name = name
  member robot.Step dir =
       match dir with
             |North-> position <- ((fst position)-1,snd position)
             |South-> position <- ((fst position)+1,snd position)
             |East -> position <- (fst position,(snd position)+1)
             |West -> position <- (fst position,(snd position)-1)

//todo below 4 class need to be finished. the important parts is the
//override Interact method.


type Goal (r:int, c:int) =
  inherit BoardElement ()
  let pos = (r,c)
  member this.Position = pos
  override this.RenderOn display =
      display.Set r c "gg"
  override this.GameOver (robots:Robot list) =
      let mutable gameover = false
      for elm in robots do
          if (fst elm.Position) = r && (snd elm.Position) = c then
             gameover <- true
          else ()
      gameover
//to do: 4 class
type BoardFrame (r:int, c:int) =
  inherit BoardElement ()
  override this.RenderOn board =
     ()
  override this.Interact robot dir:Action =     
     if (fst robot.Position) = 0 && dir = North then
          Stop (1,snd robot.Position) 
     elif (fst robot.Position) = r && dir = South then 
          Stop (r,snd robot.Position) 
     elif (snd robot.Position) = 0 && dir = West then 
          Stop (fst robot.Position,1) 
     elif (fst robot.Position) = c && dir = East then 
          Stop (fst robot.Position,c) 
     else Ignore
     (*match ((robot.Position),dir) with
         | ((1,_),North) -> Stop (1,snd robot.Position)
         | ((r,_),South) -> Stop (r,snd robot.Position)
         |((_,1),West) -> Stop (fst robot.Position,1)
         |((_,c),East) -> Stop (fst robot.Position,c)
         |_ -> Ignore 
        *)
  
(*type VerticalWall(r:int,c:int,n:int) =
type HorizontalWall((r:int,c:int,n:int))
type Portal (r:int,c:int) *)


//11g2
type Board(rows:int,cols:int) =
  
  let mutable robots:Robot list = []
  let mutable elements:BoardElement list = []
  member this.Robots = robots
  member this.Elements = elements
  member this.AddRobot robot =
    robots <- robot::robots
    elements <- (robot :> BoardElement)::elements
    
  member this.AddElement element =
    elements <- element::elements
  
  member this.Move (robot:Robot) (dir:Direction) =
    robot.Step dir
    let rec move (r:Robot) (d:Direction) lst =
       if List.forall (fun (x:BoardElement) ->x.Interact r d = Ignore) lst then
            r.Step d
            move r d lst
       else
           let e = List.find (fun (x:BoardElement)-> x.Interact r d <> Ignore) lst
           match (e.Interact r d) with
                   |Stop pos -> r.Position <- pos
                   |Continue (dirt,pos) -> ()
      
    move robot dir elements
    



type Game (board) =  //to do: write the class
   member this.Play()=
     printfn "%s" "Please enter rows and columns of the board (5-15)"
     printf "Rows:"
     let rows = int (System.Console.ReadLine())
     printf "Columns:"
     let cols = int (System.Console.ReadLine())
     let board = new Board(rows,cols)
     let random = new System.Random()
     //let goal = new Goal(random.Next(1,rows),random.Next(1,cols))
     //if goal go random, then robots need compare start place with it
     let goal = new Goal((rows-2),(cols-2))
     board.AddElement goal
     let bf = new BoardFrame (rows,cols)
     board.AddElement bf  //frame need to be the last in the list

     
     let r1 = new Robot (1,random.Next(1,cols),"AA")
     let r2 = new Robot (2,random.Next(1,cols),"BB")
     let r3 = new Robot (rows,random.Next(1,cols),"CC")
     board.AddRobot r1
     board.AddRobot r2
     board.AddRobot r3
      
     //let w1 = new HorizontalWall(2,3,0)
     //let w2 = new HorizontalWall(2,4,1)
     //let w3 = new VerticalWall (2,3,0)
     //board.AddElement w1
     //board.AddElement w2
     //board.AddElement w3
     board.AddElement goal
     let bd = new BoardDisplay (rows,cols)
     List.iter (fun (x:BoardElement) -> x.RenderOn bd) board.Elements
     bd.Show()  // this is show the game setup
     
     //game use a while loop
     while not (goal.GameOver board.Robots) do
         printfn "%s" "Choose the robot you want move:"
         let n = (System.Console.ReadLine())
         let r = List.find (fun (x:Robot) -> x.Name = n) board.Robots
         //if more time, change this to use system.readKay
         printfn "%s" "Choose direction by input the letter  \n
                    a-West d-East w-North x-South"
         let input:string = System.Console.ReadLine()
         let mutable dir = East
         match input with  
            |"a" -> dir <- West
            |"w" -> dir <- North
            |"x" -> dir <- South
            |"d" -> dir <- East
            |_-> printfn "%s" "System is too lazy to handle exception here"         
         board.Move r dir
         System.Console.Clear()
         let bd = new BoardDisplay (rows,cols)
         List.iter (fun (x:BoardElement) -> x.RenderOn bd) board.Elements
         bd.Show()        
     printfn "%s" "Finally, you reach the goal!"