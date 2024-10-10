let eval str =
  let cells = Array.make 30000 0 in
	let ptr = ref 0 in
	let len = String.length str in
  let rec skip c n =
    if c >= len then
      c
    else
      match String.get str c with
				| '[' -> skip (c+1) (n+1)
				| ']' -> if n = 0 then c else skip (c+1) (n-1)
				| _ -> skip (c+1) n
  in
	let rec evalbf c =
		if c >= len then
			c
		else
			match String.get str c with
				| '>' -> ptr := !ptr + 1; evalbf (c+1)
				| '<' -> ptr := !ptr - 1; evalbf (c+1)
				| '+' -> cells.(!ptr) <- cells.(!ptr) + 1; evalbf (c+1)
				| '-' -> cells.(!ptr) <- cells.(!ptr) - 1; evalbf (c+1)
				| '.' -> Printf.printf "%c" (Char.chr cells.(!ptr)); evalbf (c+1)
				| '[' ->
					(* evalbf returns the point in the input where it returns, so we use it to jump past the loop *)
					let newp = ref c in
          if cells.(!ptr) = 0 then
            newp := skip (c+1) 0
          else
            while cells.(!ptr) != 0 do
              newp := evalbf (c+1)
            done;
					evalbf (!newp+1)
				| ']' -> c
				| _ -> failwith "Unknown op"
	in
	ignore (evalbf 0)