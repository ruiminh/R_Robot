module robots

type status = bool*bool*(string option)// VerticalWall* HorizontalWall* str


type BoardDisplay(rows:int,cols:int） =
   let fields = Array2D.create rows cols (false,false,None)
   member this.Set (row:int) (col:int) (str:string) =
       let (v,h,s) =fields.[row,col]
       s <- str
   member this.SetBottomWall (row:int) (col:int) =
       let (v,h,s) =fields.[row,col]
       h <- true
   member this.SetRightWall (row:int) (col:int) =
       let (v,h,s) =fields.[row,col]
       v <- true
   
   override this.ToString() =
      let mutable str ="+"
      for j = 1 to cols do
          str <- str + ( sprintf "%s" "--+")
      str <- str + "\n"
      for i = 1 to rows-1 do
           str <- str + (sprintf "%s" "|")
           for j = 1 to cols-1 do
	       let (v,h,s) = fields.[i,j]
	       match fields.[i,j] with
	         |(true,_,None) -> str <- str + (sprintf "%s" "  |")
		 |(true,_,Some) -> str <- str +  (sprintf "%s" "s.Value|")
		 |(false,_,Some) -> str <- str +  (sprintf "%s" "s.Value")
	         |_-> str <- str + (sprintf "%s" "   ")
           let (v,h,s) = fields.[i,cols]
	   match fields.[i,cols] with
	      |(_,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
	      |_-> str <- str + (sprintf "%s" "  |")
           str <- str + "\n"
           str <- str + (sprintf "%s" "+")
           for j = 1 to cols do
	       let (v,h,s) = fields.[i,j]
	       match fields.[i,j] with
	         |(_,true,_) -> str <- str + (sprintf "%s" "__+")
		 |_-> str <- str + (sprintf "%s" "  +")		  	                    str <- str + "\n"
      str <- str + (sprintf "%s" "|")
      for j = 1 to cols-1 do
           let (v,h,s) = fields.[i,j]
	   match fields.[i,j] with
	        |(true,_,None) -> str <- str + (sprintf "%s" "  |")
		|(true,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
		|_-> str <- str + (sprintf "%s" "   ")
      let (v,h,s) = fields.[i,j]
      match fields.[i,j] with
          |(_,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
          |_-> str <- str + (sprintf "%s" "  |")
      str <- str + "\n"
      str <- str + (sprintf "%s" "+")
      for j = 1 to cols do
              str <-  str + (sprintf "%s" "--+")	               
      str
  member this.Show() =
      printfn "%s" this.ToString()
