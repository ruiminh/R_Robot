
//to do:documentation
//to do:quit game in the middle


module robots

type Status = bool*bool*(string option)//RightWall*BottomWall*str

///<summary>Displays the board</summary>
/// <param name="rows">Number of rows of the printed board. Int</param>
/// <param name="cols">Number of colons of the printed board. Int</param>
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
   |Continue of Direction*Position
   |Ignore

///<summary>Abstract class used for elements on the board</summary>
[< AbstractClass >]
type BoardElement () =
  abstract member RenderOn : BoardDisplay -> unit
  abstract member Interact : Robot -> Direction -> Action
  default __.Interact _ _ = Ignore
  abstract member GameOver : Robot list -> bool
  default __.GameOver _ = false
///<summary>Class for robot elements</summary>
and Robot ( row : int , col : int , name : string ) =
  inherit BoardElement ()
  let mutable position = (row,col)
  member this.Position
          with get () = position
          and set (a) = position <- a
  /// Controlls colliding with walls
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


  override this.RenderOn display =
       display.Set (fst position) (snd position) name
    
  member val Name = name
  member robot.Step dir =
       match dir with
             |North-> position <- ((fst position)-1,snd position)
             |South-> position <- ((fst position)+1,snd position)
             |East -> position <- (fst position,(snd position)+1)
             |West -> position <- (fst position,(snd position)-1)
///<summary>The goal class, checks if a robot has ended their turn on top, and ends the game in a win</summary>
/// <param name="r">Number of rows of the printed board. Int</param>
/// <param name="c">Number of colons of the printed board. Int</param>
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

///<summary>The BoardFrame class, manages the outer board walls and stops the robots if they collide with them</summary>
/// <param name="r">Number of rows of the printed board. Int</param>
/// <param name="c">Number of colons of the printed board. Int</param>
type BoardFrame (r:int, c:int) =
  inherit BoardElement ()
  override this.RenderOn display =
     ()
  override this.Interact robot dir:Action =     
     if (fst robot.Position) = 0 && dir = North then
          Stop (1,snd robot.Position) 
     elif (fst robot.Position) = r && dir = South then 
          Stop (r,snd robot.Position) 
     elif (snd robot.Position) = 0 && dir = West then 
          Stop (fst robot.Position,1) 
     elif (snd robot.Position) = c && dir = East then 
          Stop (fst robot.Position,c) 
     else Ignore

///<summary>VerticalWall element class, manages display and collision with vertical walls</summary>
/// <param name="r">Number of rows of the printed board. Int</param>
/// <param name="c">Number of colons of the printed board. Int</param>
/// <param name="n">Length of the wall. Int</param>
type VerticalWall(r:int,c:int,n:int) =
  inherit BoardElement ()
  override this.RenderOn display =
    if n > 0 then 
      for i = 1 to n do
           display.SetRightWall (r+i-1) c
    else
      for i = 1 to (-n) do
       display.SetRightWall (r-i+1) c
  override this.Interact robot dir:Action =       
    if (snd robot.Position = c) && (fst robot.Position > (r-1)) && (fst robot.Position < (r+n)) then
          match dir with
            |East -> Stop (fst robot.Position,c)
            |West -> Stop (fst robot.Position,(c+1))
            |_ -> Ignore
    else
         Ignore      
///<summary>HorizontallWall element class, manages display and collision with Horizontal walls</summary>
/// <param name="r">Number of rows of the printed board. Int</param>
/// <param name="c">Number of colons of the printed board. Int</param>
/// <param name="n">Length of the wall. Int</param>
type HorizontalWall(r:int,c:int,n:int)=
  inherit BoardElement ()
  override this.RenderOn display =
    if n > 0 then 
         for i = 1 to n do
             display.SetBottomWall r (c+i-1)
    else
         for i = 1 to (-n) do
             display.SetBottomWall r (c-i+1)
  override this.Interact robot dir:Action =
    if (fst robot.Position = r) && (snd robot.Position > (c-1)) && (snd robot.Position < (c+n)) then
          match dir with
            |South -> Stop (r,snd robot.Position)
            |North -> Stop ((r+1),snd robot.Position)
            |_ -> Ignore
    else
         Ignore

///<summary>Portal element class, manages display and teleportation with portals</summary>
/// <param name="r">Number of rows of the printed board. Int</param>
/// <param name="c">Number of colons of the printed board. Int</param>
type Portal (r:int,c:int) =
  inherit BoardElement ()
  member this.Position = (r,c)
  override this.RenderOn display =
     display.Set r c "☯ "
  override this.Interact robot dir:Action =     
     if this.Position = robot.Position then
          Continue (North,(0,1)) 
     else Ignore

//11g2
///<summary>Board class, manages display of given elements and the movement of the robots</summary>
/// <param name="rrows">Number of rows of the printed board. Int</param>
/// <param name="cols">Number of colons of the printed board. Int</param>
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
    //robot.Step dir
    let rec move (r:Robot) (d:Direction) lst =
       if List.forall (fun (x:BoardElement) ->x.Interact r d = Ignore) lst then
            r.Step d
            move r d lst
       else
           let e = List.find (fun (x:BoardElement)-> x.Interact r d <> Ignore) lst
           match (e.Interact r d) with
                   |Stop pos -> r.Position <- pos
                   |Continue (dirt,pos) -> r.Position <-pos
                                          
    move robot dir elements
    


///<summary>Game class, initializes the game, controlls the game and user input </summary>
type Game (board) =  //to do: write the class
   member this.Play()=
     System . Console . BackgroundColor <- System . ConsoleColor . Blue
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
     let p = new Portal(3,2)
     board.AddElement p
     let w1 = new HorizontalWall(2,3,1)
     let w2 = new HorizontalWall(1,4,2)
     let w3 = new VerticalWall (2,3,1)
     board.AddElement w1
     board.AddElement w2
     board.AddElement w3
     board.AddElement goal
     let bd = new BoardDisplay (rows,cols)
     List.iter (fun (x:BoardElement) -> x.RenderOn bd) board.Elements
     bd.Show()  // this is show the game setup
     let mutable counter = 0
     
     //game use a while loop
     while not (goal.GameOver board.Robots) do
         printfn "%s" "Choose the robot you want move:"
         let n = (System.Console.ReadLine())
         let r = List.find (fun (x:Robot) -> x.Name = n) board.Robots
         //here can handle exception
         printfn "%s" "Press Arrow keys to move the robot:"
         let k = System.Console.ReadKey(true)
         let mutable dir = East
         match k.Key with  
            |System.ConsoleKey.UpArrow -> dir <- North
            |System.ConsoleKey.DownArrow -> dir <- South
            |System.ConsoleKey.LeftArrow -> dir <- West
            |System.ConsoleKey.RightArrow -> dir <- East
            |_-> printfn "%A" k.Key         
         board.Move r dir
         counter <- counter + 1
         System.Console.Clear()
         let bd = new BoardDisplay (rows,cols)
         List.iter (fun (x:BoardElement) -> x.RenderOn bd) board.Elements
         bd.Show()        
     printfn "You reach the goal! It takes you %d steps" counter


