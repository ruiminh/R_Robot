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
               match fields.[i,j] with
                  |(_,true,_) -> str <- str + ( sprintf "%s" "--+")
                  |_-> str <- str + ( sprintf "%s" "  +")
               str <- str + "\n"	               
       str
  
