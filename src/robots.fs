

type Coordinate = int * int

type BoardDisplay(rows:int,cols:int） =
   let this.Rows = rows
   let this.Cols = cols
   override this.ToString() =
      let mutable str ="+"
      for j = 1 to cols do
          str <- str + ( sprintf "%s" "--+")
      str <- str + "\n"
      for i = 1 to rows-1 do
           str <- str + (sprintf "%s" "|")
           for j = 1 to cols-1 do
	         str <- str + (sprintf "%s" "   ")
           str <- str + (sprintf "%s" "  |")
           str <- str + "\n"
           str <- str + (sprintf "%s" "+")
           for j = 1 to cols do
	         str <- str + (sprintf "%s" "  +")
           str <- str + "\n"
      str <- str + (sprintf "%s" "|")
      for j = 1 to cols-1 do
	    str <- str + (sprintf "%s" "   ")
      str <- str + (sprintf "%s" "  |")
      str <- str + "\n"
      str <- str + (sprintf "%s" "+")
      for j = 1 to cols do
           str <-  str + (sprintf "%s" "--+")
      str
