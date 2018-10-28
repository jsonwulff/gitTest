let xs = [7;55;34;23;5;42;32;34;8]
let xs0 = []

let rec insert xs y =
  match xs with
  | [] -> [y]
  | x::xs -> if y < x then
                y :: x :: xs
              else
                x :: (insert xs y)

let isort xs = List.fold (fun acc x -> insert acc x) [] xs
printf "%A\n" (isort xs)


let rec bubble (xs:int list) =
  match xs with
  | [] ->  []
  | x :: xs ->
    match xs with
    | [] -> [x]
    | y :: ys -> if x < y then 
                    x :: (bubble (y::ys)) 
                  else 
                    y :: (bubble (x :: ys ))


let bsort xs = List.fold (fun acc _ -> bubble acc) xs xs
printf "%A\n" (bsort xs)

