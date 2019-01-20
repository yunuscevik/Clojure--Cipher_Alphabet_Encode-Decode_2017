; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yunus ÇEVİK                      *
; *********************************************

;; utility functions
(load-file "include.clj") ;; "c2i and "i2c"

(use 'clojure.java.io)
(use '[clojure.string :only (split)])
(require '[clojure.set :as set]) ;; random sayılar için
(use 'clojure.set)

(defn read-as-list
	"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	[filename]
  (def readInList '())
  (try
    (with-open [lnRdr (reader filename)]
      (doseq [word (line-seq lnRdr)]
        ;; Dosyadan satır satır okunan kelimeler lislere çevrilerek yeni bir liste oluşturulur.
        (def readInList (conj readInList (list* (split word #""))))
      )
    )

    (catch java.io.FileNotFoundException e (println (str "Caught Exception: " (.getMessage e)) "\n\n"))
    (catch Exception e (println (str "Caught Exception: " (.getMessage e)) "\n\n"))
  )
  (reverse readInList)
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defn spell-checker-0   ;; Alınan string tipindeki kelimeyi dictionary2.txt içindeki tüm kelimerle aynı mı diye tek tek bakarak kontrol eder.
	[word]
  (def readList (read-as-list "dictionary2.txt"))
  (def trOrFls false)
  (def wordList (list* (split  word #"")))
  (doseq [dcr readList :while (= trOrFls false)]
    (when (= (count dcr) (count wordList)) (when (= (first dcr) (first wordList)) (when (= (last dcr) (last wordList))
      (when (= dcr wordList) (def trOrFls (= dcr wordList)))
    )))
  )
  trOrFls
)


(defn spell-checker-1  ;; Alınan string tipindeki kelimeyi dictionary2.txt içindeki tüm kelimerle aynı mı diye binary-search yaparak kontrol eder.
	[word]

  (def stringList-of-word '())
  (def readList (read-as-list "dictionary2.txt"))
  (doseq [n readList]
    (def stringList-of-word (conj stringList-of-word (apply str n)))
  )
  (def stringList-of-word (reverse stringList-of-word))

  (def bottom 0)
  (def top (- (count stringList-of-word) 1))
  (def midl 0)
  (def trOrFls false)
  (while (and (<= bottom top ) (= trOrFls false))
    (def midl (int (/ (+ bottom top) 2.0)))
    (if (= (compare (nth stringList-of-word midl) word) 0)
        (def trOrFls true)
    )
    (if (> (compare (nth stringList-of-word midl) word) 0)
        (def top (- midl 1))
    )
    (if (< (compare (nth stringList-of-word midl) word) 0)
        (def bottom (+ midl 1))
    )
  )
  trOrFls
)
;;-----------------------------------------------------------
;; n factoriyellik random cipher-alphabet oluşturur.
(def factorial (reductions * 1N (drop 1 (range))))

(defn factoradic [n] {:pre [(>= n 0)]}
   (loop [a (list 0) n n p 2]
      (if (zero? n) a (recur (conj a (mod n p)) (quot n p) (inc p)))))
;; n factiyellik pemütayson ile random cipher-alphabet oluşturur.
(defn nth-permutation [s n] {:pre [(< n (nth factorial (count s)))]}
  (let [d (factoradic n)
        choices (concat (repeat (- (count s) (count d)) 0) d)]
    ((reduce
        (fn [m i]
          (let [[left [item & right]] (split-at i (m :rem))]
            (assoc m :rem (concat left right)
                     :acc (conj (m :acc) item))))
      {:rem s :acc []} choices) :acc)))
;;-----------------------------------------------------------
;; Encode edilmiş dosyada sıklıkla tekrar eden harflerin frekans analizlerini yapıp bir liste şeklinde dönderir.
(defn frequence [paragraph]
  (def frCount 0)
  (def prgCount 0)
  (def frequencyAnalysis '())
  (dotimes [i 26]
    (doseq [n paragraph]
      (dotimes [j (count n)]
        (if (= (compare (str (i2c i)) (nth n j)) 0)
          (def frCount (+ frCount 1)))
        (if (< i 1)
          (def prgCount (+ prgCount 1)))
      )
    )
    (def frequencyAnalysis (conj frequencyAnalysis (float (/ (* frCount 100) prgCount))))
    (def frCount 0)
  )
  (reverse frequencyAnalysis)
)
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A  ;; Encode edilmiş paragraf Gen-Decoder-A ile Burute Forece işlemine tabi tutulur.
	[paragraph]
  (def empty-list '())
  (def trOrFls1 false)
  (def orj-alph [])
  (dotimes [n 26]
    (def orj-alph (conj orj-alph (str (i2c n))))
  )
  (def randMapList (map (partial nth-permutation orj-alph) (range (nth factorial (count orj-alph))))) ;; 26! lik random map oluşturulur.
  (doseq [n randMapList :while (= trOrFls1 false)]
    (def decode-paragrap '())
    (def return-dec-parag '())
    (def word '())
    (def trOrFls2 true)
    (def cipher-alph (apply list n))
    (println "\ncipher-alph  : " cipher-alph)
    (println "orjinal-alph : " (apply list orj-alph))
    (println "encode-paragraph:" paragraph)
    (doseq [i paragraph]
      (doseq [k i]
        (dotimes [j (count cipher-alph)]  ;; encode edilmiş word ü mapping yaparak decode edilmiş paragraf oluşturulur.
          (if (= (compare (str k) (str (nth cipher-alph j))) 0) (def word (conj word (i2c j))))
        )
      )
      (def decode-paragrap (conj decode-paragrap (apply str (reverse word))))
      (def word empty-list)
    )
    (doseq [i (reverse decode-paragrap)]
       (def return-dec-parag (conj return-dec-parag (list* (split (str i) #""))))
    )
    (def decode-paragrap (reverse decode-paragrap))
    (println "decode-paragrap :  " decode-paragrap)
    (doseq[i decode-paragrap :when (= trOrFls2 true)]  ;; Decode edilmiş paragrafın her word ü spell-checker ile dictionary2.txt de olup olmadığı kontrol edilir.
      (if(= (def trOrFls2 (spell-checker-0 i)) false)          ;; spell-checker-0
      ;;(if(= (def trOrFls2 (spell-checker-1 i)) false)        ;; spell-checker-1
        (do
          (def decode-paragrap empty-list)
          (def return-dec-parag empty-list)
        )
      )
      (println i "-->" trOrFls2)
      (def trOrFls1 trOrFls2) ;; Eğer decode edilen paragraf içindeki word ler dictionary2.txt içinde bulunmuyorsa yeni cipher-alphabet (map) oluşturulur.
    )
  )
  (reverse return-dec-parag)
)

(defn Gen-Decoder-B-0  ;; Encode edilmiş paragraf Gen-Decoder-B-0 ile Frequence Analysis işlemine tabi tutulur.
	[paragraph]
  (def frequencyAlph [\e \t \a \o \i \n]) ;; English Alphabet en çok sıklıkla tekrar eden kelimeler
  (def orj-alph [])
  (def empty-list '())
  (def trOrFls1 false)

  (def frequence-list (frequence paragraph))
  (def frequencyAnalysis (sort > (list* (set frequence-list)))) ;; en sıktan aza doğru frekanslar
  (if (< 6 (count frequencyAnalysis))
    (def frequencyAnalysis (take 6 frequencyAnalysis))
  )

  (def frequenceIndex '())
  (def frequenceChar '())
  (doseq [i frequencyAnalysis :when (not= i 0.0)]
    (dotimes [j (count frequence-list)]
      (if (= i (nth frequence-list j)) (do (def frequenceIndex (conj frequenceIndex j))))
    )
  )
  (def frequenceIndex (reverse frequenceIndex)) ;; frekansların English Alphabet' indeki index numaralarının listesi

  (if (< 6 (count frequenceIndex))
    (def frequenceIndex (take 6 frequenceIndex))
  )
  (doseq [i frequenceIndex]
    (def frequenceChar (conj frequenceChar (i2c i)))
  )
  (def frequenceChar (reverse frequenceChar)) ;; frekansların indexlerinin karşılık geldiği harflerlin listesi

  (dotimes [n 26]
    (def orj-alph (conj orj-alph  (i2c n)))
  )

  (def notFrequenceChar (apply list (difference (set orj-alph) (set (apply vector frequenceChar))))) ;; frekansların indexlerine karşılık gelmeyen harflerin listesi
  (def notFrequencyAlph (apply list (difference (set orj-alph) (set frequencyAlph)))) ;; English Alphabet' indeki az sıklıkla tekrar eden harflerin listesi

  (def randMapList (map (partial nth-permutation notFrequenceChar) (range (nth factorial (count notFrequenceChar))))) ;; sık tekrar edenler hariç 20! lik random cipher-alphabet

  (doseq [n randMapList :while (= trOrFls1 false)]
    (def decode-paragrap '())
    (def return-dec-parag '())
    (def word '())
    (def trOrFls2 true)
    (def cipher-alph (apply list (concat frequenceChar n)))
    (def orj-alp2 (apply list (concat frequencyAlph notFrequencyAlph)))
    (println "\nDocument-Frequency Characters: " frequenceChar)
    (println "cipher-alph  : " cipher-alph)
    (println "orjinal-alph : " orj-alp2)
    (println "encode-paragraph:" paragraph)
    (doseq [i paragraph]
      (doseq [k i]
        (dotimes [j (count cipher-alph)] ;; encode edilmiş word ü mapping yaparak decode edilmiş paragraf oluşturulur.
          (if (= (compare (str k) (str (nth cipher-alph j))) 0)
            (def word (conj word (nth orj-alp2 j)))
          )
        )
      )
      (def decode-paragrap (conj decode-paragrap (apply str (reverse word))))
      (def word empty-list)
    )
    (doseq [i (reverse decode-paragrap)]
       (def return-dec-parag (conj return-dec-parag (list* (split (str i) #""))))
    )
    (def decode-paragrap (reverse decode-paragrap))
    (println "decode-paragrap :  " decode-paragrap)
    (doseq[i decode-paragrap :when (= trOrFls2 true)]  ;; Decode edilmiş paragrafın her word ü spell-checker ile dictionary2.txt de olup olmadığı kontrol edilir.
      ;;(if(= (def trOrFls2 (spell-checker-0 i)) false)  ;; spell-checker-0 Burute Forece
      (if(= (def trOrFls2 (spell-checker-1 i)) false)    ;; spell-checker-1 Binary-Search
        (do
          (def decode-paragrap empty-list)
          (def return-dec-parag empty-list)
        )
      )
      (println i "-->" trOrFls2)
      (def trOrFls1 trOrFls2)
    )
  )
  (reverse return-dec-parag)

)

(defn Gen-Decoder-B-1
	[paragraph]
  	;you should implement this function
)

(defn Code-Breaker
	[document decoder] ;; documet string tipinde encode edilmiş dosyanın ismini alır. Example; "documet.txt" , decoder hangi decodurun belirleneceğini gösterir. Example; Gen-Decoder-A
   (decoder (read-as-list document))
)

;;Test
;;(println (Code-Breaker "documet.txt" Gen-Decoder-A))
;;(println (Code-Breaker "documet.txt" Gen-Decoder-B-0))
