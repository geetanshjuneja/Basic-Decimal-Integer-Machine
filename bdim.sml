exception wrongOPcode;
exception indexOutofBound;
exception memnotbool;
exception incorrectInput;
exception divbyzero;
exception modbyzero;
(*declaring memory array of size 100 with element at each index is 0*)
val mem = Array.array(100,0);

(*function to read input*)
fun readLine() = let
  val y = "input: "
in
   print(y);
  valOf(TextIO.inputLine(TextIO.stdIn))
end
 


fun intTobool(0) = false | intTobool(1) = true | intTobool(x)  = raise memnotbool; 
fun boolToint(false) = 0 | boolToint(true) = 1;

(*function to evaluate instuctions in code vector*)
fun interpretCode(vec:(int*int*int*int) vector,t:int) = let
  val (a,b,c,d) = Vector.sub(vec,t)
  fun  evaluateOPcode(0,_,_,_) = () |
  evaluateOPcode(1,_,_,k) = let
    val x = readLine()
    val y = String.substring(x,0,size x - 1)
    val z = Int.fromString(y)
  in
    if z=NONE then raise incorrectInput else Array.update(mem,k,valOf(z)); interpretCode(vec,t+1)
  end  |
  evaluateOPcode(2,i,_,k) = let
    val x = Array.sub(mem,i)
  in
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end|
  evaluateOPcode(3,i,_,k) = let
    val h = intTobool(Array.sub(mem,i))
    val x = not h
    val y = boolToint(x)
  in
    Array.update(mem,k,y);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(4,i,j,k) = let
    val x = intTobool(Array.sub(mem,i))
    val y = intTobool(Array.sub(mem,j))
    val z = boolToint(x orelse y)
  in
    Array.update(mem,k,z);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(5,i,j,k) = let
    val x = intTobool(Array.sub(mem,i))
    val y = intTobool(Array.sub(mem,j))
    val z = boolToint(x andalso y)
  in
    Array.update(mem,k,z);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(6,i,j,k) = let
    val x = Array.sub(mem,i) + Array.sub(mem,j)
  in
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(7,i,j,k) = let
    val x = Array.sub(mem,i) - Array.sub(mem,j)
  in
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(8,i,j,k) = let
    val x = Array.sub(mem,i) * Array.sub(mem,j)
  in
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(9,i,j,k) = let
    val x = Array.sub(mem,i) div Array.sub(mem,j)
  in
    if Array.sub(mem,j)=0 then raise divbyzero
    else
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(10,i,j,k) = let
    val x = Array.sub(mem,i) mod Array.sub(mem,j)
  in
    if Array.sub(mem,j) =0 then raise modbyzero
    else
    Array.update(mem,k,x);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(11,i,j,k) = let
    val x = (Array.sub(mem,i) = Array.sub(mem,j))
    val y = boolToint(x)
  in
    Array.update(mem,k,y);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(12,i,j,k) = let
    val x = (Array.sub(mem,i) > Array.sub(mem,j))
    val y = boolToint(x)
  in
    Array.update(mem,k,y);
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(13,i,_,k) = if intTobool(Array.sub(mem,i)) then interpretCode(vec,k) 
  else interpretCode(vec,t+1) |
  evaluateOPcode(14,_,_,k) = interpretCode(vec,k) |
  evaluateOPcode(15,i,_,_) = let
    val x = Array.sub(mem,i)
  in
    print(Int.toString(x)^"\n"); 
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(16,i,_,k) = let
    val x = i
  in
    Array.update(mem,k,x); 
    interpretCode(vec,t+1)
  end |
  evaluateOPcode(m,i,j,k) = raise wrongOPcode
in
  if(t>=0 andalso t<Vector.length vec) then evaluateOPcode(a,b,c,d) else raise indexOutofBound
end;


fun interpret(filename:string) = let
  val f = TextIO.openIn filename;
  
  (*Function to read file line by line into a string list*)
  fun readFile(file) = let
  val x = TextIO.inputLine file
  in
  if x = NONE then [] else valOf(x)::readFile(file)
  end ;

  val str = readFile(f);

  (*Function to convert string list into list containing list of opcode*)
  fun toVector(ls:string list) = let
    fun toListInt s =
    let
    val s' = String.substring(s, 1, size s-2)
    in
    List.mapPartial Int.fromString (String.tokens (fn c => c = #",") s')
    end
  (*int list to quadruple*)
  fun toQuadruple (a::b::c::d::[])= (a,b,c,d) | toQuadruple l = raise wrongOPcode;

  val toQuadList = map toListInt ls;

  val toQuadrupleList = map toQuadruple toQuadList;
  in
  Vector.fromList toQuadrupleList
  end;
  (*Creating Vector of quadruples*)
  val code = toVector(str);
in
  interpretCode(code,0)
end;