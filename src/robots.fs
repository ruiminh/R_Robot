module robots

type Status = bool*bool*(string option)// VerticalWall* HorizontalWall* str


type BoardDisplay(rows:int,cols:int) =
   let fields:Status [,] = Array2D.create rows cols (false,false,None)
   member this.Set (row:int) (col:int) (str:string) =
       let (v,h,s) = fields.[row,col]
       fields.[row,col]<- (v,h,Some str)  
   member this.SetBottomWall (row:int) (col:int) =
       let (v,h,s) =fields.[row,col]
       fields.[row,col] <- (v,true,s)
   member this.SetRightWall (row:int) (col:int) =
       let (v,h,s) =fields.[row,col]
       fields.[row,col] <- (true,h,s)
   member this.Show() =
      printfn "%s" this.ToString()

   override this.ToString() =
       let mutable str ="+"
       for j = 1 to cols do //ceilling
          str <- str + ( sprintf "%s" "--+")
       str <- str + "\n"
       for i = 1 to rows-1 do //main body except the bottom row
           str <- str + (sprintf "%s" "|") //every row start with "|"
           for j = 1 to cols-1 do //row's body except the last column.
               let (v,h,s) = fields.[i,j]//easy to get out value in tripler
               match fields.[i,j] with  //h wall doesn't matter
                 |(true,_,None) -> str <- str + (sprintf "%s" "  |")
                 |(true,_,Some) -> str <- str +  (sprintf "%s" "s.Value|")
                 |(false,_,Some) -> str <- str +  (sprintf "%s" "s.Value")
                 |_-> str <- str + (sprintf "%s" "   ")
           let (v,h,s) = fields.[i,cols] //the last column in the row
           match fields.[i,cols] with  //verticalwall doesn't matter either
              |(_,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
              |_-> str <- str + (sprintf "%s" "  |")
           str <- str + "\n"
           str <- str + (sprintf "%s" "+") //rows bottom line start
           for j = 1 to cols do  //v wall and str don't matter
	       let (v,h,s) = fields.[i,j]
	       match fields.[i,j] with
	         |(_,true,_) -> str <- str + (sprintf "%s" "__+")
                 |_-> str <- str + (sprintf "%s" "  +")
           str <- str + "\n"
       str <- str + (sprintf "%s" "|") // start with the last row
       for j = 1 to cols-1 do //row body,h wall doesn't matter
           let (v,h,s) = fields.[rows,j]
           match fields.[i,j] with
                |(true,_,None) -> str <- str + (sprintf "%s" "  |")
                |(true,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
                |_-> str <- str + (sprintf "%s" "   ")
       let (v,h,s) = fields.[i,j] //row's last col, only s matter
       match fields.[rows,j] with
          |(_,_,Some) -> str <- str + (sprintf "%s" "s.Value|")
          |_-> str <- str + (sprintf "%s" "  |")
       str <- str + "\n"
       str <- str + (sprintf "%s" "+") //bottom line, nothing matters
       for j = 1 to cols do
              str <-  str + (sprintf "%s" "--+")	               
       str
  member this.Show() =
      printfn "%s" this.ToString()
