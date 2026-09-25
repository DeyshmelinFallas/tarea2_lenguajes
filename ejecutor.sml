(* ===================================================================
   analizador.sml  -  Análisis del catálogo de la biblioteca (SIGB)

   Lee un archivo CSV con el formato:
     codigo,autor,genero,fecha_publicacion,copias_disponibles

   y ofrece un menú con:
     a) Libros más populares en un rango de copias
     b) Autores con 5 o más libros
     c) Búsqueda por código o autor
     d) Cantidad de libros por género
     e) Resumen general

   Uso:   - main ();
   =================================================================== *)

type book = { codigo : string, autor : string, genero : string,
              fecha : string, copias : int }

(* ---------- Utilidades de texto ---------- *)

fun trim s =
  Substring.string (Substring.dropr Char.isSpace
                      (Substring.dropl Char.isSpace (Substring.full s)))

fun lower s = String.map Char.toLower s

fun allDigits s = s <> "" andalso CharVector.all Char.isDigit s

fun spaces n = if n <= 0 then "" else CharVector.tabulate (n, fn _ => #" ")
fun pad (s, n) = s ^ spaces (n - size s)

fun plural (n, sing, plur) =
  Int.toString n ^ " " ^ (if n = 1 then sing else plur)

(* ---------- Entrada por consola ---------- *)

(* Muestra un mensaje y lee una línea. Si se acaba la entrada (EOF), sale. *)
fun prompt msg =
  ( print msg
  ; TextIO.flushOut TextIO.stdOut
  ; case TextIO.inputLine TextIO.stdIn of
        NONE => (print "\n"; OS.Process.exit OS.Process.success)
      | SOME s => trim s )

fun promptInt msg =
  let val s = prompt msg
  in
    if allDigits s then valOf (Int.fromString s)
    else (print "  Debe ingresar un numero entero mayor o igual a 0.\n";
          promptInt msg)
  end

(* ---------- Ordenamiento (merge sort estable) ---------- *)

(* le (a, b) = true si a puede ir antes (o igual) que b *)
fun msort le [] = []
  | msort le [x] = [x]
  | msort le xs =
      let
        val half = length xs div 2
        val left = List.take (xs, half)
        val right = List.drop (xs, half)
        fun merge ([], ys) = ys
          | merge (xs, []) = xs
          | merge (x :: xs, y :: ys) =
              if le (x, y) then x :: merge (xs, y :: ys)
              else y :: merge (x :: xs, ys)
      in
        merge (msort le left, msort le right)
      end

(* ---------- Lectura y parseo del CSV ---------- *)

fun readLines path =
  let
    val ins = TextIO.openIn path
    fun loop acc =
      case TextIO.inputLine ins of
          NONE => List.rev acc
        | SOME l => loop (l :: acc)
    val ls = loop []
  in
    TextIO.closeIn ins; ls
  end

(* Fecha AAAA-MM-DD con mes entre 01 y 12 *)
fun validFecha s =
  size s = 10
  andalso String.sub (s, 4) = #"-"
  andalso String.sub (s, 7) = #"-"
  andalso allDigits (String.substring (s, 0, 4))
  andalso allDigits (String.substring (s, 5, 2))
  andalso allDigits (String.substring (s, 8, 2))
  andalso (let val m = valOf (Int.fromString (String.substring (s, 5, 2)))
           in m >= 1 andalso m <= 12 end)

(* Convierte una línea en un libro; NONE si está mal formada *)
fun parseLine line : book option =
  case String.fields (fn c => c = #",") (trim line) of
      [cod, aut, gen, fec, cop] =>
        let
          val cod = trim cod  val aut = trim aut  val gen = trim gen
          val fec = trim fec  val cop = trim cop
        in
          if cod <> "" andalso aut <> "" andalso gen <> ""
             andalso validFecha fec andalso allDigits cop
          then SOME { codigo = cod, autor = aut, genero = gen, fecha = fec,
                      copias = valOf (Int.fromString cop) }
          else NONE
        end
    | _ => NONE

fun isHeader line = String.isPrefix "codigo," (lower (trim line))
fun isBlank line = trim line = ""

(* Carga el archivo; devuelve (libros, líneas ignoradas) *)
fun loadBooks path : book list * int =
  let
    val lines = List.filter (fn l => not (isBlank l) andalso not (isHeader l))
                            (readLines path)
    val parsed = List.map parseLine lines
    val books = List.mapPartial (fn x => x) parsed
  in
    (books, length lines - length books)
  end

(* ---------- Agrupamiento y conteo ---------- *)

(* Agrupa por clave sin distinguir mayúsculas; conserva el primer nombre
   visto y el orden de aparición. *)
fun groupBy (key : book -> string) (books : book list) : (string * book list) list =
  let
    fun ins (b, []) = [(key b, [b])]
      | ins (b, (k, bs) :: rest) =
          if lower k = lower (key b) then (k, b :: bs) :: rest
          else (k, bs) :: ins (b, rest)
  in
    List.foldl ins [] books
  end

(* Cantidad de libros distintos (por código) en una lista *)
fun numLibros (bs : book list) =
  let
    fun add (b : book, seen) =
      if List.exists (fn c => lower c = lower (#codigo b)) seen then seen
      else #codigo b :: seen
  in
    length (List.foldl add [] bs)
  end

(* Elementos con puntaje máximo (todos los empatados) *)
fun maxTies score [] = []
  | maxTies score (x :: xs) =
      let
        fun go ([], best, acc) = List.rev acc
          | go (y :: ys, best, acc) =
              let val s = score y
              in
                if s > best then go (ys, s, [y])
                else if s = best then go (ys, best, y :: acc)
                else go (ys, best, acc)
              end
      in
        go (xs, score x, [x])
      end

fun groupCount (_, bs : book list) = numLibros bs

(* Grupos ordenados de mayor a menor cantidad de libros *)
fun sortedGroups key books =
  msort (fn (a, b) => groupCount a >= groupCount b) (groupBy key books)

(* "2006-04-07" -> "2006-04" *)
fun anioMes (b : book) = String.substring (#fecha b, 0, 7)

(* "2006-04" -> "04-2006" *)
fun mesAnio s = String.substring (s, 5, 2) ^ "-" ^ String.substring (s, 0, 4)

(* ---------- Impresión de tablas ---------- *)

fun colWidth (f : book -> string) title (bs : book list) =
  List.foldl (fn (b, m) => Int.max (m, size (f b))) (size title) bs

fun printBooks (bs : book list) =
  let
    val wc = colWidth #codigo "Codigo" bs
    val wf = colWidth #fecha "Publicacion" bs
    val wa = colWidth #autor "Autor" bs
    val wg = colWidth #genero "Genero" bs
    val wn = size (Int.toString (length bs))
    fun row (n, c, f, a, g, k) =
      print ("  " ^ pad (n, Int.max (wn, 1)) ^ "  " ^ pad (c, wc) ^ "  "
             ^ pad (f, wf) ^ "  " ^ pad (a, wa) ^ "  " ^ pad (g, wg)
             ^ "  " ^ k ^ "\n")
    fun loop (_, []) = ()
      | loop (i, (b : book) :: rest) =
          ( row (Int.toString i, #codigo b, #fecha b, #autor b, #genero b,
                 Int.toString (#copias b))
          ; loop (i + 1, rest) )
  in
    row ("#", "Codigo", "Publicacion", "Autor", "Genero", "Copias");
    loop (1, bs)
  end

(* ---------- Opciones del menú ---------- *)

(* a) Libros más populares dentro de un rango de copias *)
fun opcionPopulares (books : book list) =
  let
    val a = promptInt "Minimo de copias disponibles: "
    val b = promptInt "Maximo de copias disponibles: "
    val (lo, hi) = if a <= b then (a, b) else (b, a)
    val _ = if a > b then print "  (El minimo era mayor que el maximo; se intercambiaron.)\n"
            else ()
    val found = List.filter (fn (x : book) => #copias x >= lo andalso #copias x <= hi) books
    val ranking = msort (fn (x : book, y : book) => #copias x >= #copias y) found
  in
    if null ranking then
      print ("\nNo hay libros con entre " ^ Int.toString lo ^ " y "
             ^ Int.toString hi ^ " copias disponibles.\n")
    else
      ( print ("\nLibros mas populares con entre " ^ Int.toString lo ^ " y "
               ^ Int.toString hi ^ " copias (" ^ Int.toString (length ranking)
               ^ " en total):\n")
      ; printBooks ranking )
  end

(* b) Autores con 5 o más libros publicados *)
fun opcionAutores (books : book list) =
  let
    val autores = List.filter (fn g => groupCount g >= 5)
                              (sortedGroups (fn (b : book) => #autor b) books)
  in
    if null autores then
      print "\nNingun autor tiene 5 o mas libros en la biblioteca.\n"
    else
      ( print "\nAutores con 5 o mas libros publicados:\n"
      ; List.app (fn (a, bs) =>
                    print ("  " ^ a ^ ": " ^ plural (numLibros bs, "libro", "libros") ^ "\n"))
                 autores )
  end

(* c) Buscar por código (exacto) o autor (coincidencia parcial) *)
fun opcionBuscar (books : book list) =
  let
    val q = prompt "Ingrese un codigo de libro o el nombre de un autor: "
    val ql = lower q
    fun matches (b : book) =
      lower (#codigo b) = ql orelse String.isSubstring ql (lower (#autor b))
    val found = if q = "" then [] else List.filter matches books
  in
    if null found then
      print ("\nNo se encontraron libros para '" ^ q ^ "'.\n")
    else
      ( print ("\nSe encontraron " ^ plural (length found, "libro", "libros") ^ ":\n")
      ; printBooks found )
  end

(* d) Cantidad de libros de un género *)
fun opcionGenero (books : book list) =
  let
    val g = prompt "Ingrese el genero: "
    val found = List.filter (fn (b : book) => lower (#genero b) = lower g) books
  in
    print ("\nGenero '" ^ g ^ "': "
           ^ plural (numLibros found, "libro registrado", "libros registrados") ^ ".\n")
  end

(* e) Resumen general *)
fun opcionResumen (books : book list) =
  if null books then print "\nNo hay libros para analizar.\n"
  else
    let
      val generos = sortedGroups (fn (b : book) => #genero b) books
      val autores = sortedGroups (fn (b : book) => #autor b) books
      val meses   = sortedGroups anioMes books
      val masCopias = maxTies (fn (b : book) => #copias b) books
      val topAutores = maxTies groupCount autores
      val topGeneros = maxTies groupCount generos
      val topMeses = maxTies groupCount meses
      fun libroStr (b : book) =
        #codigo b ^ " - " ^ #autor b ^ " (" ^ #genero b ^ ", " ^ #fecha b ^ ") con "
        ^ plural (#copias b, "copia", "copias")
      fun grupoStr fmt (k, bs) =
        fmt k ^ " con " ^ plural (numLibros bs, "libro", "libros")
    in
      print "\n========== RESUMEN GENERAL ==========\n";
      print "1. Cantidad de libros por genero:\n";
      List.app (fn (g, bs) => print ("     " ^ g ^ ": " ^ Int.toString (numLibros bs) ^ "\n"))
               generos;
      print "2. Libro con mas copias disponibles:\n";
      List.app (fn b => print ("     " ^ libroStr b ^ "\n")) masCopias;
      print "3. Autor con mas libros en la biblioteca:\n";
      List.app (fn g => print ("     " ^ grupoStr (fn k => k) g ^ "\n")) topAutores;
      print "4. Genero con mas libros registrados:\n";
      List.app (fn g => print ("     " ^ grupoStr (fn k => k) g ^ "\n")) topGeneros;
      print "5. Mes-ano con mas publicaciones:\n";
      List.app (fn (k, bs) =>
                  print ("     " ^ mesAnio k ^ " con "
                         ^ plural (numLibros bs, "publicacion", "publicaciones") ^ "\n"))
               topMeses
    end

(* ---------- Programa principal ---------- *)

(* Pide la ruta hasta lograr abrir el archivo *)
fun cargar () : book list =
  let
    val path = prompt "Ingrese la ruta del archivo a analizar (ej. /tmp/datalibros.csv): "
  in
    case ((SOME (loadBooks path)) handle IO.Io _ => NONE) of
        NONE => (print "  No se pudo abrir ese archivo. Intente de nuevo.\n"; cargar ())
      | SOME (books, ignoradas) =>
          ( print ("Se cargaron " ^ plural (length books, "libro", "libros") ^ ".\n")
          ; if ignoradas > 0 then
              print ("Aviso: se ignoraron " ^ plural (ignoradas, "linea", "lineas")
                     ^ " con formato invalido.\n")
            else ()
          ; books )
  end

fun menu (books : book list) =
  ( print "\n===== ANALIZADOR - Biblioteca =====\n"
  ; print "a. Libros mas populares en un rango de copias\n"
  ; print "b. Autores con 5 o mas libros publicados\n"
  ; print "c. Buscar libros por codigo o autor\n"
  ; print "d. Cantidad de libros por genero\n"
  ; print "e. Resumen general de la biblioteca\n"
  ; print "s. Salir\n"
  ; case lower (prompt "Opcion: ") of
        "a" => (opcionPopulares books; menu books)
      | "b" => (opcionAutores books; menu books)
      | "c" => (opcionBuscar books; menu books)
      | "d" => (opcionGenero books; menu books)
      | "e" => (opcionResumen books; menu books)
      | "s" => print "Hasta luego!\n"
      | _   => (print "Opcion no valida.\n"; menu books) )

fun main () = menu (cargar ())