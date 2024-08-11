exception Partial;;

type 'a thunk = Thunk of (unit -> 'a) * (('a option) ref)

type 'a dllNode = First of ('a dllNode) ref
                | Last of (('a dllNode) thunk) ref
                | Node of ('a dllNode) ref * 'a * ('a dllNode) ref

type 'a dllList = DLL of 'a dllNode * 'a dllNode

(* let head = function
  | DLL (head, _) -> head

let tail = function
  | DLL (_, tail) -> tail *)

let force = function
  | Thunk (f, v) -> match !v with
                  | None -> let res = f () in v := Some res; res
                  | Some res -> res

let laz f = Thunk (f, ref None) 
let defer x = Thunk ((fun () -> x), ref None) 

let oops () = raise Partial

let push x = function
  | DLL (_, ((Last prev) as last)) -> let oldPrev = force !prev in
                            let newLast = Node ((ref oldPrev), x, ref last) in
                            prev := defer newLast;
                            (match oldPrev with
                            | First next | Node (_, _, next) -> next := newLast
                            | Last _ -> oops ())
  | DLL (_, _) -> oops ()

let unshift x = function
  | DLL ((First next) as first, _) -> let oldNext = !next in
                                      let newFirst = Node ((ref first), x, ref !next) in
                                      next := newFirst;
                                      (match oldNext with
                                      | Last prev -> prev := defer newFirst
                                      | Node (prev, _, _) -> prev := newFirst
                                      | First _ -> oops ())
  | DLL (_, _) -> oops ()


let makeList _ =  let firstThunk = ref None in
                  let last = ref None in
                  firstThunk := (Some (laz (fun () -> 
                                                let (Some last) = !last in
                                                (First (ref (force last))))));
                  last := (Some (laz (fun () -> 
                                      let (Some firstThunk) = !firstThunk in
                                      (Last (ref firstThunk)))));
                  let (Some last) = !last in
                  let (Some firstThunk) = !firstThunk in
                  DLL ((force firstThunk), (force last))

let rec string_of_dll_node string_of_x = function
  | First next -> "#START " ^ string_of_dll_node string_of_x !next
  | Node (_, x, next) -> string_of_x x ^ " " ^ string_of_dll_node string_of_x !next
  | Last _ -> "#END"
let string_of_dll string_of_x (DLL (head, _)) = string_of_dll_node string_of_x head

let print_stringln s = print_string (s ^ "\n")

let main () = 
  let xs = makeList () in
  push 1 xs;
  push 2 xs;
  push 3 xs;
  unshift 0 xs;
  push 4 xs;
  unshift (-1) xs;
  print_stringln (string_of_dll string_of_int xs)

let _ = main ()