(*funciones auxiliares*)

(* inputLine deja un salto de linea "\n" al final, esto lo quita *)
fun quitarSalto cadena =
  if String.isSuffix "\n" cadena then String.substring(cadena, 0, size cadena - 1)
  else cadena

(* lee una linea del teclado y la devuelve limpia *)
fun leerLinea () =
  case TextIO.inputLine TextIO.stdIn of
      SOME cadena => quitarSalto cadena
    | NONE => ""

(* pide los datos uno por uno y arma una linea de texto
el orden de la linea final DEBE ser el del archivo:
codigo,autor,genero,fecha_publicacion,copias_disponibles*)
fun agregarLibro ruta =
  let
    val _ = print "Codigo: "
    val codigo = leerLinea ()

    val _ = print "Autor: "
    val autor = leerLinea ()

    val _ = print "Genero: "
    val genero = leerLinea ()

    val _ = print "Fecha de publicacion (AAAA-MM-DD): "
    val fecha = leerLinea ()

    val _ = print "Copias disponibles: "
    val copias = leerLinea ()

    (* se unen todos los campos separados por coma*)
    val linea = codigo ^ "," ^ autor ^ "," ^ genero ^ "," ^ fecha ^ "," ^ copias

    (* openAppend = abre el archivo y escribe AL FINAL sin borrar lo anterior *)
    val out = TextIO.openAppend ruta
  in
    TextIO.output (out, linea ^ "\n");
    TextIO.closeOut out;
    print "Libro agregado.\n"
  end


(* openOut = abre el archivo y BORRA todo lo que tenia antes *)
fun limpiarCatalogo ruta =
  let
    val out = TextIO.openOut ruta
  in
    (* se mantiene el encabezado para que el archivo mantenga la estructura *)
    TextIO.output (out, "codigo,autor,genero,fecha_publicacion,copias_disponibles\n");
    TextIO.closeOut out;
    print "Catalogo limpiado.\n"
  end


(* --- menu que se repite hasta que el usuario elija salir --- *)
fun menu ruta =
  (
    print "\n--- CREADOR ---\n";
    print "1) Agregar libro\n";
    print "2) Limpiar catalogo\n";
    print "0) Salir\n";
    print "Opcion: ";
    case leerLinea () of
        "1" => (agregarLibro ruta; menu ruta)
      | "2" => (limpiarCatalogo ruta; menu ruta)
      | "0" => print "Gracias por usar nuestra gestion bibliotecaria.\n"
      | _   => (print "Opcion invalida.\n"; menu ruta)
  )


(*el main ejeuta el programa principal *)
fun main () =
  (
    print "Ruta del archivo (ejemplo /tmp/datalibros.csv): ";
    let
      val ruta = leerLinea ()
    in
      menu ruta
    end
  )