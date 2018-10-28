open ImgUtil

let rec triangle bmp len (x,y) =
  if len < 25 then setBox red (x,y) (x+len,y+len) bmp
  else 
    let half = len / 2
    do triangle bmp half (x+half/2,y)
    do triangle bmp half (x,y+half)
    do triangle bmp half (x+half,y+half)

