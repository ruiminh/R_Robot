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
  member this.Position = position
  override this.Interact other dir =
     let (r0,c0) = position
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
       |_-> Ignore
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
  override this.RenderOn display =
      display.Set r c "gg"
  override this.GameOver (robots:Robot list) =
      let mutable gameover = false
      for elm in robots do
          if (fst elm.Position) = r && (snd elm.Position) = c then
             gameover <- true
          else ()
      gameover

type BoardFrame (r:int, c:int) =
  override this.Interact robot dir =
  override this.RenderOn display =
type VerticalWall(r:int,c:int,n:int) =
type HorizontalWall((r:int,c:int,n:int))
type Portal (r:int,c:int)


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
    
 (* member this.Move robot dir =
    let rec move robot dir lst =
      robot.Step (dir:Direction):()
      match lst with  
         |[] -> robot.Step (dir:Direction)
         |x::ys -> if (x.Interact robot dir = Stop pos) then ()
                   else move robot dir ys
    move robat dir elements*) // to do: debug or maybe rewrite
    



type Game (board) =  //to do: write the class
   member this.play()=
   // getting lazy, just take arguments of rows and cols when run exe file
     let b = new Board()
     b.addRobot robot
     b.addElement element
   // Mulighed for at loade forskellige startposition fra en fil.
     //ask player input which robot he want move
     //move robot, when stop,check if game over,if not
     start an other move ????? how to do it?
        

     
  
  
       
			          
