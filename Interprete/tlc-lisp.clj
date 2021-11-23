(declare evaluar)
(declare aplicar)
(declare controlar-aridad)
(declare igual?)
(declare cargar-arch)
(declare imprimir)
(declare actualizar-amb)
(declare revisar-f)
(declare revisar-lae)
(declare buscar)
(declare evaluar-cond)
(declare evaluar-secuencia-en-cond)

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua.
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
   ([]
      (println "Interprete de TLC-LISP en Clojure")
	  (println "Trabajo Practico de Programacion III")
	  (println "Inspirado en:")
      (println "TLC-LISP Version 1.51 for the IBM Personal Computer")
      (println "Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
      (repl '(add add append append cond cond cons cons de de env env equal equal eval eval exit exit
 			  first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
 			  null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
 			  t t terpri terpri + add - sub)))
   ([amb]  
      (print ">>> ") (flush)
      (try (let [res (evaluar (read) amb nil)]
	            (if (nil? (fnext res))
				    true
					(do (imprimir (first res)) (repl (fnext res)))))
           (catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb))))
)

; Evalua una expresion usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local. 
(defn evaluar [expre amb-global amb-local]
	(if (not (seq? expre))
		(if (or (number? expre) (string? expre)) (list expre amb-global) (list (buscar expre (concat amb-local amb-global)) amb-global))
		(cond (igual? expre nil) (list nil amb-global)
		      (igual? (first expre) '*error*) (list expre amb-global)
		      (igual? (first expre) 'exit) (if (< (count (next expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) amb-global))
	          (igual? (first expre) 'setq) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
			                                     (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
			                                     (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
												 (= (count (next expre)) 2) (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
				                                                                 (list (first res) (actualizar-amb amb-global (fnext expre) (first res))))
												 true (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                           (evaluar (cons 'setq (next (nnext expre))) (actualizar-amb amb-global (fnext expre) (first res)) amb-local)))
			  (igual? (first expre) 'de) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
											   (and (not (igual? (first (nnext expre)) nil)) (not (seq? (first (nnext expre))))) (list (list '*error* 'list 'expected (first (nnext expre))) amb-global)
			                                   (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
			                                   (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
											   true (list (fnext expre) (actualizar-amb amb-global (fnext expre) (cons 'lambda (nnext expre)))))
 			  (igual? (first expre) 'load) (cond (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
												 (> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
			                                     true (list \space (cargar-arch amb-global amb-local (fnext expre))))
			  (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) amb-global)
			  (igual? (first expre) 'lambda) (cond (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
											       (and (not (igual? (fnext expre) nil)) (not (seq? (fnext expre)))) (list (list '*error* 'list 'expected (fnext expre)) amb-global)
											       true (list expre amb-global))
			  (igual? (first expre) 'or) (if (nil? (next expre)) (list nil amb-global)
											(let [res1 (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre))
												  res2 (filter (fn [x] (or (and (seq? x) (igual? (first x) '*error*)) (igual? x nil))) res1)
												  res3 (filter (fn [x] (not (or (and (seq? x) (igual? (first x) '*error*)) (igual? x nil)))) res1)]
												 (if (empty? res3)
													 (list (if (empty? (first res2)) nil (first res2)) amb-global)
													 (list (first res3) amb-global))))
			  (igual? (first expre) 'if) (if (nil? (next expre))
											(list (list '*error* 'list 'expected (next expre)) amb-global)  
											(if (not (igual? (first (evaluar (fnext expre) amb-global amb-local)) nil))
												(evaluar (first (nnext expre)) amb-global amb-local)
												(evaluar (fnext (nnext expre)) amb-global amb-local)))
   			  (igual? (first expre) 'cond) (evaluar-cond (next expre) amb-global amb-local)
			  true (aplicar (first (evaluar (first expre) amb-global amb-local)) (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre)) amb-global amb-local)))
)

; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,  
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto. 
(defn aplicar
   ([f lae amb-global amb-local]
      (aplicar (revisar-f f) (revisar-lae lae) f lae amb-global amb-local))
   ([resu1 resu2 f lae amb-global amb-local]
      (cond resu1 (list resu1 amb-global)
		    resu2 (list resu2 amb-global)
		    true  (if (not (seq? f))
		              (list (cond
							(igual? f 'eval) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (and (not (igual? (first lae) nil)) (not (seq? (first lae)))) (first (evaluar (first lae) amb-global amb-local))
			                                             true (if (igual? (first (first lae)) 'quote) (first (evaluar (fnext (first lae)) amb-global amb-local)) (first (evaluar (first lae) amb-global amb-local)))))
							(igual? f 'cons) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (and (not (igual? (fnext lae) nil)) (not (seq? (fnext lae)))) (list '*error* 'not-implemented)
												         true (cons (first lae)(fnext lae))))
							(igual? f 'append) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (and (not (igual? (first lae) nil)) (not (seq? (first lae)))) (list '*error* 'list 'expected (first lae))
													    (and (not (igual? (fnext lae) nil)) (not (seq? (fnext lae)))) (list '*error* 'list 'expected (fnext lae))
							                            true (let [res (concat (first lae) (fnext lae))] (if (empty? res) nil res))))
							(igual? f 'equal) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (igual? (first lae)(fnext lae)) 't 
												    	true nil))
							(igual? f 'lt) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (not (number? (first lae))) (list '*error* 'number-expected (first lae)) 
												        (not (number? (fnext lae))) (list '*error* 'number-expected (fnext lae)) 
										                (< (first lae)(fnext lae)) 't
												        true nil))
							(igual? f 'gt) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (not (number? (first lae))) (list '*error* 'number-expected (first lae)) 
												        (not (number? (fnext lae))) (list '*error* 'number-expected (fnext lae)) 
										                (> (first lae)(fnext lae)) 't
												        true nil))
							(igual? f 'ge) (let [ari (controlar-aridad lae 2)]
							                      (cond (seq? ari) ari
												        (not (number? (first lae))) (list '*error* 'number-expected (first lae)) 
												        (not (number? (fnext lae))) (list '*error* 'number-expected (fnext lae)) 
										                (>= (first lae)(fnext lae)) 't
												        true nil))
							(igual? f 'not) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (igual? (first lae) nil) 't
												        true nil))
							(igual? f 'null) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (igual? (first lae) nil) 't
												        true nil))
							(igual? f 'length) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (and (not (seq? (first lae))) (not (igual? (first lae) nil))) (list '*error* 'arg-wrong-type (first lae))
										                true (count (first lae))))
							(igual? f 'reverse) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (and (not (seq? (first lae))) (not (igual? (first lae) nil))) (list '*error* 'list 'expected (first lae))
										                true (let [res (reverse (first lae))] (if (empty? res) nil res)))) 
							(igual? f 'list) (if (< (count lae) 1) nil lae)
							(igual? f 'prin3) (cond (< (count lae) 1) (list '*error* 'too-few-args)
										            (> (count lae) 1) (list '*error* 'not-implemented)
										            true (do (print (first lae)) (flush) (first lae)))
			                (igual? f 'terpri) (if (> (count lae) 0)
							                       (list '*error* 'not-implemented)
							                       (do (println) (flush) nil)) 
			                (igual? f 'env) (if (> (count lae) 0)
							                    (list '*error* 'too-many-args)
												(concat amb-global amb-local))
							(igual? f 'first) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (igual? (first lae) nil) nil
												        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
										                true (ffirst lae)))
							(igual? f 'rest) (let [ari (controlar-aridad lae 1)]
							                      (cond (seq? ari) ari
												        (igual? (first lae) nil) nil
												        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
										                true (nfirst lae)))
							(igual? f 'add) (if (< (count lae) 2)
							                    (list '*error* 'too-few-args)
							                    (try (reduce + lae) 
												     (catch Exception e (list '*error* 'number-expected))))
							(igual? f 'sub) (if (< (count lae) 2)
							                    (list '*error* 'too-few-args)
							                    (try (reduce - lae)
												     (catch Exception e (list '*error* 'number-expected))))
		 			        (igual? f 'read) (if (> (count lae) 0)
			                                    (list '*error* 'not-implemented)
			                                    (read)) 
							true (let [lamb (buscar f (concat amb-local amb-global))]
								    (cond (or (number? lamb) (igual? lamb 't) (igual? lamb nil)) (list '*error* 'non-applicable-type lamb)
		                                  (or (number? f) (igual? f 't) (igual? f nil)) (list '*error* 'non-applicable-type f)
		                                  (igual? (first lamb) '*error*) lamb
	 	                                  true (aplicar lamb lae amb-global amb-local)))) amb-global)
					(cond (< (count lae) (count (fnext f))) (list (list '*error* 'too-few-args) amb-global)
					      (> (count lae) (count (fnext f))) (list (list '*error* 'too-many-args) amb-global)
					      true (if (nil? (next (nnext f)))
					               (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))
						           (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))) amb-local))))))
)

; Controla la aridad (cantidad de argumentos de una funcion).
; Recibe una lista y un numero. Si la longitud de la lista coincide con el numero, retorna el numero.
; Si es menor, retorna (list '*error* 'too-few-args).
; Si es mayor, retorna (list '*error* 'too-many-args).
(defn controlar-aridad [lis val-esperado] (
    if (= (count lis) val-esperado) 
        val-esperado
        (if (< (count lis) val-esperado) (list '*error* 'too-few-args) (list '*error* 'too-many-args))
))

; Verifica la igualdad de dos simbolos.
; Recibe dos simbolos a y b. Retorna true si se deben considerar iguales; si no, false.
; Se utiliza porque TLC-LISP no es case-sensitive y ademas no distingue entre nil y la lista vacia.
(defn igual? [a b] 
	(if (or (seq? a) (seq? b))
		(if	
			(or
				(and (nil? a) (seq? b) (empty? b)) (and (nil? b) (seq? a) (empty? a))
				(and (nil? a) (nil? b)) (and (seq? a) (empty? a) (seq? b) (empty? b))
			) true (= a b)
		)
		(if
			(or 
				(and (= 'NIL a) (nil? b))
				(and (nil? a) (= 'NIL b))
			) true (= (clojure.string/lower-case (str a)) (clojure.string/lower-case (str b)))
		)
	)
)

; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo
; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo 
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (and (seq? nomb) (igual? (first nomb) '*error*))
	    (do (imprimir nomb) amb-global) 
        (let [nm (clojure.string/lower-case (str nomb)),
              nom (if (and (> (count nm) 4)(clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
              ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                             (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
							                                        (cargar-arch (fnext res) nil in res))
	                                                           (catch Exception e (imprimir nil) amb-global))))
			  	       (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
  		     ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
         (catch Exception e (imprimir (first res)) amb-global)))
)

; Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas con comillas) y devuelve su valor. Muestra errores sin parentesis.
; Aridad 1: Si recibe un escalar, lo imprime con salto de linea en formato estandar (pero si es \space no lo imprime), purga la salida y devuelve el escalar.
; Si recibe una secuencia cuyo primer elemento es '*error*, se llama recursivamente con dos argumentos iguales: la secuencia recibida.
; Si no, imprime lo recibido con salto de linea en formato estandar, purga la salida y devuelve la cadena.
; Aridad 2: Si el primer parametro es nil, imprime un salto de linea, purga la salida y devuelve el segundo parametro.
; Si no, imprime su primer elemento en formato estandar, imprime un espacio y se llama recursivamente con la cola del primer parametro y el segundo intacto.
(defn imprimir
    (
        [elem]
        (cond
            (or (and (not (seq? elem)) (not= elem \space))
                (and (seq? elem) (not= (first elem) '*error*))
            ) 
                (do 
                    (prn elem)
                    (flush)
                    elem
                )
            (and (not (seq? elem)) (= elem \space))
                (do
                    (flush)
                    elem
                )
            (and (seq? elem) (= (first elem) '*error*)) (imprimir elem elem)
        )
    )
    (
        [lis orig] 
        (cond
            (nil? lis) 
                (do 
                    (newline)
                    (flush)
                    orig
                )
            (not (nil? lis))
                (do 
                    (cond
                        (not (empty? lis))
                        (do
                            (pr (first lis))
                            (printf "%s" " ")
                        )
                    )
                    (imprimir (if (empty? lis) nil (pop lis)) orig)
                )
        )
    )
)

; Actualiza un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
; Recibe el ambiente, la clave y el valor.
; Si el valor no es escalar y en su primera posicion contiene '*error*, retorna el ambiente intacto.
; Si no, coloca la clave y el valor en el ambiente (puede ser un alta o una actualizacion) y lo retorna.
(defn actualizar-amb [amb-global clave valor] (
    if(and (seq? valor) (= (first valor) '*error*)) 
        amb-global
        (
            try (
                into '() (
                    reverse (
                        assoc  
                        (vec amb-global)
                        (nth (
                            for [x (range 0 (count amb-global))
                                :when (and (even? x) (= (nth amb-global x) clave))
                            ] (+ x 1)
                        ) 0)
                        valor
                    )
                )
            )
            (catch Exception e (lazy-seq (conj (conj (vec amb-global) clave) valor))) 
        )
))

; Revisa una lista que representa una funcion.
; Recibe la lista y, si esta comienza con '*error*, la retorna. Si no, retorna nil.
(defn revisar-f [lis] (if (and (seq? lis) (= (first lis) '*error*)) lis nil))

; Revisa una lista de argumentos evaluados.
; Recibe la lista y, si esta contiene alguna sublista que comienza con '*error*, retorna esa sublista. Si no, retorna nil.
(defn revisar-lae [lis] (
    try (
        nth (
            for [x lis
                :when (and (seq? x) (= (first x) '*error*))
            ] x
        ) 0
    )
    (catch Exception e nil) 
))

; Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...] y retorna el valor asociado.
; Si no la encuentra, retorna una lista con '*error* en la 1ra. pos., 'unbound-symbol en la 2da. y el elemento en la 3ra.
(defn buscar [elem lis] (
	cond 
        (empty? lis) (list '*error* 'unbound-symbol elem)
        (not (empty? lis)) 
            (if (= elem (first lis))
                (nth lis 1)
                (buscar elem (drop 2 lis))
            )
))

; Evalua el cuerpo de una macro COND. Siempre retorna una lista con un resultado y un ambiente.
; Recibe una lista de sublistas (cada una de las cuales tiene una condicion en su 1ra. posicion) y los ambientes global y local.
; Si la lista es nil, el resultado es nil y el ambiente retornado es el global.
; Si no, evalua (con evaluar) la cabeza de la 1ra. sublista y, si el resultado no es nil, retorna el res. de invocar a evaluar-secuencia-en-cond con la cola de esa sublista.
; En caso contrario, sigue con las demas sublistas. 
(defn evaluar-cond [lis amb-global amb-local] 
	(cond 
		(nil? lis) (list nil amb-global)
		(not (igual? (first (evaluar (ffirst lis) amb-global amb-local)) nil)) (evaluar-secuencia-en-cond (nfirst lis) amb-global amb-local)
		:else (evaluar-cond (pop lis) amb-global amb-local)
	)
)

; Evalua (con evaluar) secuencialmente las sublistas de una lista y retorna el valor de la ultima evaluacion.
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
	(if (nil? (next lis))
			(evaluar (first lis) amb-global amb-local)
			(let [result (evaluar (first lis) amb-global amb-local)]
				(if (and (seq? (first result)) (igual? (ffirst result) '*error*))
						result
						(evaluar-secuencia-en-cond (next lis) (second result) amb-local)
				)
			)
	)
)

; Al terminar de cargar el archivo, se retorna true.
true
