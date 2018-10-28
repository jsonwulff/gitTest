let xs = [7;55;34;23;5;42;32;34;8]

//Selection sort
let rec select (xs:int list) (m,ys) = 
  if List.isEmpty xs then (m,ys)
  else let x = List.head xs
    let xs = List.tail xs 
    in if x < m then
      if m <> System.Int32.MaxValue then 
        select xs (x,m::ys)
      else select xs (x,ys) 
    else select xs (m,x::ys)
let rec ssort xs =
  if List.isEmpty xs then xs
  else let (m,xs) = select xs (System.Int32.MaxValue,[])
    in m :: ssort xs

//7ø.0 Insertion Sort
let rec insert xs y =
  if List.isEmpty xs then [y] 
  else 
    let x = List.head xs
    if y < x then y :: xs 
      else x :: insert (List.tail xs) y

let isort xs = List.fold (fun acc x -> insert acc x) [] xs


do printf "%A\n" (isort xs)

//7ø.1 Bubble Sort


let rec bubble (xs:int list) =
  if List.isEmpty xs then []          // x::y::ys
  else 
    let x = List.head xs           //  => y::bubble(x::ys) (y<x)
    let ys = List.tail xs 
    if List.isEmpty ys then 
      [x] 
    else
      let y = List.head ys 
      if x < y then 
        x :: bubble ys 
      else 
        y :: bubble (x::List.tail ys)

let bsort xs =
  List.fold (fun acc _ -> bubble acc) xs xs

do printf "%A\n" (bsort xs)