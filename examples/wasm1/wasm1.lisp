(loadfile "lisp/lisp.lisp")


(defmacro static (f)
  (eval f))

(defvar wasm1 "0061736d0100000001070160027f7f017f03020100070c01086d756c7469
706c7900000a09010700200020016c0b")
(defvar wasm1 "0061736d01000000010b0260027f7f017f6000017f03030200010608017f0141d0ce040b070c01086d756c7469706c7900000a15020700200020016c0b0b00230041016a240023000b")
(defvar wasm1 "0061736d01000000010b0260027f7f017f6000017f03030200010614037f0141d0ce040b7f0141400b7f0141b0b17b0b071302086d756c7469706c79000004696e636600010a15020700200020016c0b0b00230041016a240023000b")
(defvar wasm1 "0061736d01000000010b0260027f7f017f6000017f03030200010614037f0141d0ce040b7f0141400b7f0141b0b17b0b071302086d756c7469706c79000004696e636600010a28020700200020016c0b1e002300420143000000404400000000000008401a1a1a41016a240023000b")
(defvar glyph:0 (car "0"))
(defvar glyph:9 (car "9"))
(defvar glyph:dot (car "."))
(defvar glyph:space (car " "))
(defvar glyph:newline (car "
"))

(defvar hexchars (list "0123456789abcdefABCDEF"
							  (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 10 11 12 13 14 15)
							  ))
(defvar hex::lookup (lisp::make-int32-array 128))

(defun whitespace? (chr)
  (or (eq chr glyph:space) (eq chr glyph:newline))
  )

(defun glyph:char-code(c)
  (c.charCodeAt 0))

(defun glyph:from-char-code (c)
  (String.fromCharCode c))

(dotimes (i (length (car hexchars)))
  (setnth hex::lookup (glyph:char-code (nth (car hexchars) i))
			 (nth (cadr hexchars) i)))

(defun hex->bytes (str)
  (let ((bytes (list)))
	 (dotimes (i (length str))
		(unless (whitespace? (nth str i))
		  (let ((a (nth hex::lookup (glyph:char-code (nth str i))))
				  (b (nth hex::lookup (glyph:char-code (nth str (+ i 1))))))
			 (push bytes (+ (* 16 a) b))
			 )
		  (incf i 1)
		  ))
	 bytes
	 ))

(defvar hex::str (car hexchars))
(defun bytes->hex (bytes)
  (let ((str ""))
	 (foreach b bytes
				 (set str (concat str
										(th hex::str (logand #xF (>> b 4)))
										(th hex::str (logand #xF b))
										)))
	 str))
										

(defun read:from-bytes(bytes)
  (list bytes 0))

(defun read:byte (r)
  (let ((offset (cadr r))
		  (b (th (car r) offset)))
	 (setnth r 1 (+ offset 1))
	 b))
(defun read:skip (r n)
  (set (th r 1) (+ (th r 1) n)))

(defun read:finished?(r)
  (>= (cadr r) (length (car r))))

(defun read:bytes(r n)
  (let ((o (list))
		  (bytes (car r))
		  (offset (cadr r)))
	 (dotimes (i n)
		(push o (th bytes (+ i offset))))
	 
	 (setnth r 1 (+ offset n))
	 o))

(defun read:uleb (r)
  (block a
	 (let ((result 0)
			 (offset 0))
		(loop 1
				(let ((chunk (read:byte r)))
				  (unless chunk
					 (return-from a result))
				
				  (set result (logor result (<< (logand #b01111111 chunk) offset)))
				  (set offset (+ offset 7))
				  (when (eq 0 (logand #b10000000 chunk))
					 (return-from a result))
		
				  )))))

(defun read:ileb (r)
  
  (let ((value 0)
		  (read t)
		  (offset 0)
		  (chunk 0)
		  (read-chunks (list)))
	 (loop read
			 (set chunk (read:byte r))
			 (set value (+ value (<< (logand chunk #x7f) offset)))
			 (set offset (+ offset 7))
			 (set read (> chunk 128)))
	 (when (and (< offset 64) (logand chunk #x40))
		
		(set value (logor value (<< -1 offset))))
	 value))

(defun read:f32 (r)
  (let ((bytes (read:bytes r 4)))
	 
	 (%js "new DataView(new Uint8Array(bytes).buffer).getFloat32(0, true)")))

(defun read:f64 (r)
  (let ((bytes (read:bytes r 8)))
	 
	 (%js "new DataView(new Uint8Array(bytes).buffer).getFloat64(0, true)")))


(defun read:get-offset (r)
  (th r 1))

(defun read:set-offset (r n)
  (set (th r 1) n))

(defun write:new ()
  (list 'writer (list)))

(defun write:byte (w b)
  (push (th w 1) b))

(defun write:bytes (w bytes)
  (foreach b bytes
			  (write:byte w b)))

(defun write:to-array(w)
  (let ((bytes (cadr w)))
	 (%js "new Uint8Array(bytes)")))

(defun write:uleb (w integer)
  (cond
	 ((< integer 128) (write:byte w integer))
	 ((< integer (static (<< 1 14)))
	  (write:byte w (logor #b10000000 (logand #b11111111 integer)))
	  (write:byte w (>> integer 7)))
	 ((< integer (static (<< 1 21)))
	  (write:byte w (logor #b10000000 (logand #b11111111 integer)))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 7))))
	  (write:byte w (>> integer 14))
	  )
	 ((< integer (static (<< 1 28)))
	  (write:byte w (logor #b10000000 (logand #b11111111 integer)))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 7))))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 14))))
	  (write:byte w (>> integer 21))
	  )
	 ((< integer (static (<< 1 30)))
	  (write:byte w (logor #b10000000 (logand #b11111111 integer)))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 7))))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 14))))
	  (write:byte w (logor #b10000000 (logand #b11111111 (>> integer 21))))
	  (write:byte w (>> integer 28))
	  )
	 (1 (raise (list 'unsupported-integer integer)))
	 ))

(defun write:ileb(w value)
  ($ let ((continue t)))
  (loop continue
		  ($ let ((bits (logand #b01111111 value))
					 (sign (logand #b01000000 value))
					 (next (>> value 7))))
		  (if (or (and (eq next 0) (eq sign 0))
					 (and (> sign 0) (eq next -1)))
				(progn
				  (write:byte w bits)
				  (set continue nil))
				(progn
				  (write:byte w (logor #b10000000 bits))
				  (set value next)))))

(defun write:f32(w value)
  (let ((buf (%js "new Uint8Array(4)"))
		  (view (%js "new DataView(buf.buffer)")))
	 (view.setFloat32 0 value t)
	 (write:bytes w buf)))

(defun write:f64(w value)
  (let ((buf (%js "new Uint8Array(8)"))
		  (view (%js "new DataView(buf.buffer)")))
	 (view.setFloat64 0 value t)
	 (write:bytes w buf)))

(defvar wasm:section-lookup
 '(custom type ímport function table memory global
	export start element code data data-count))

(defvar wasm:section-lookup-inverse (make-hash-map))
(let ((iter 0))
  (foreach item wasm:section-lookup
			  (hash-map-set wasm:section-lookup-inverse item iter)
			  (incf iter)))

(defun wasm:section-id-by-name (name)
  (hash-map-get wasm:section-lookup-inverse name))

(assert-eq 7 (wasm:section-id-by-name 'export))

(defvar wasm:instructions
  '((#x00 unreachable)
	 (#x01 nop)
	 (#x02 block)
(#x03 loop)
(#x04 if)
(#x05 else)
(#x0B end)
(#x0C br)
(#x0D br_if)
(#x0E br_table)
(#x0F return)
(#x10 call)
(#x11 call_indirect)
(#x1A drop)
(#x1B select)
(#x20 local.get)
(#x21 local.set)
(#x22 local.tee)
(#x23 global.get)
(#x24 global.set)
(#x28 i32.load)
(#x29 i64.load)
(#x2A f32.load)
(#x2B f64.load)
(#x2C i32.load8_s)
(#x2D i32.load8_u)
(#x2E i32.load16_s)
(#x2F i32.load16_u)
(#x30 i64.load8_s)
(#x31 i64.load8_u)
(#x32 i64.load16_s)
(#x33 i64.load16_u)
(#x34 i64.load32_s)
(#x35 i64.load32_u)
(#x36 i32.store)
(#x37 i64.store)
(#x38 f32.store)
(#x39 f64.store)
(#x3A i32.store8)
(#x3B i32.store16)
(#x3C i64.store8)
(#x3D i64.store16)
(#x3E i64.store32)
(#x3F memory.size)
(#x40 memory.grow)
(#x41 i32.const)
(#x42 i64.const)
(#x43 f32.const)
(#x44 f64.const)
(#x45 i32.eqz)
(#x46 i32.eq)
(#x47 i32.ne)
(#x48 i32.lt_s)
(#x49 i32.lt_u)
(#x4A i32.gt_s)
(#x4B i32.gt_u)
(#x4C i32.le_s)
(#x4D i32.le_u)
(#x4E i32.ge_s)
(#x4F i32.ge_u)
(#x50 i64.eqz)
(#x51 i64.eq)
(#x52 i64.ne)
(#x53 i64.lt_s)
(#x54 i64.lt_u)
(#x55 i64.gt_s)
(#x56 i64.gt_u)
(#x57 i64.le_s)
(#x58 i64.le_u)
(#x59 i64.ge_s)
(#x5A i64.ge_u)
(#x5B f32.eq)
(#x5C f32.ne)
(#x5D f32.lt)
(#x5E f32.gt)
(#x5F f32.le)
(#x60 f32.ge)
(#x61 f64.eq)
(#x62 f64.ne)
(#x63 f64.lt)
(#x64 f64.gt)
(#x65 f64.le)
(#x66 f64.ge)
(#x67 i32.clz)
(#x68 i32.ctz)
(#x69 i32.popcnt)
(#x6A i32.add)
(#x6B i32.sub)
(#x6C i32.mul)
(#x6D i32.div_s)
(#x6E i32.div_u)
(#x6F i32.rem_s)
(#x70 i32.rem_u)
(#x71 i32.and)
(#x72 i32.or)
(#x73 i32.xor)
(#x74 i32.shl)
(#x75 i32.shr_s)
(#x76 i32.shr_u)
(#x77 i32.rotl)
(#x78 i32.rotr)
(#x79 i64.clz)
(#x7A i64.ctz)
(#x7B i64.popcnt)
(#x7C i64.add)
(#x7D i64.sub)
(#x7E i64.mul)
(#x7F i64.div_s)
(#x80 i64.div_u)
(#x81 i64.rem_s)
(#x82 i64.rem_u)
(#x83 i64.and)
(#x84 i64.or)
(#x85 i64.xor)
(#x86 i64.shl)
(#x87 i64.shr_s)
(#x88 i64.shr_u)
(#x89 i64.rotl)
(#x8A i64.rotr)
(#x8B f32.abs)
(#x8C f32.neg)
(#x8D f32.ceil)
(#x8E f32.floor)
(#x8F f32.trunc)
(#x90 f32.nearest)
(#x91 f32.sqrt)
(#x92 f32.add)
(#x93 f32.sub)
(#x94 f32.mul)
(#x95 f32.div)
(#x96 f32.min)
(#x97 f32.max)
(#x98 f32.copysign)
(#x99 f64.abs)
(#x9A f64.neg)
(#x9B f64.ceil)
(#x9C f64.floor)
(#x9D f64.trunc)
(#x9E f64.nearest)
(#x9F f64.sqrt)
(#xA0 f64.add)
(#xA1 f64.sub)
(#xA2 f64.mul)
(#xA3 f64.div)
(#xA4 f64.min)
(#xA5 f64.max)
(#xA6 f64.copysign)
(#xA7 i32.wrap_i64)
(#xA8 i32.trunc_f32_s)
(#xA9 i32.trunc_f32_u)
(#xAA i32.trunc_f64_s)
(#xAB i32.trunc_f64_u)
(#xAC i64.extend_i32_s)
(#xAD i64.extend_i32_u)
(#xAE i64.trunc_f32_s)
(#xAF i64.trunc_f32_u)
(#xB0 i64.trunc_f64_s)
(#xB1 i64.trunc_f64_u)
(#xB2 f32.convert_i32_s)
(#xB3 f32.convert_i32_u)
(#xB4 f32.convert_i64_s)
(#xB5 f32.convert_i64_u)
(#xB6 f32.demote_f64)
(#xB7 f64.convert_i32_s)
(#xB8 f64.convert_i32_u)
(#xB9 f64.convert_i64_s)
(#xBA f64.convert_i64_u)
(#xBB f64.promote_f32)
(#xBC i32.reinterpret_f32)
(#xBD i64.reinterpret_f64)
(#xBE f32.reinterpret_i32)
(#xBF f64.reinterpret_i64)
(#xC0 i32.extend8_s)
(#xC1 i32.extend16_s)
(#xC2 i64.extend8_s)
(#xC3 i64.extend16_s)
(#xC4 i64.extend32_s)))

(defvar wasm:instr-lookup (list))
(defvar wasm:instr-lookup-inverse (make-hash-map))
(dotimes (i #xFF)
  (push wasm:instr-lookup i)
  )
(foreach pair wasm:instructions
			 (set (th wasm:instr-lookup (car pair)) (cadr pair))
			 (hash-map-set wasm:instr-lookup-inverse (cadr pair) (car pair)) 
			 )

(defun wasm:instruction-to-opcode (opcode)
  (hash-map-get wasm:instr-lookup-inverse opcode))

(defvar wasm:types
  '((i32 #x7f)
	 (i64 #x7e)
	 (f32 #x7d)
	 (f64 #x7c)
	 (v128 #x7b)
	 (funcref #x70)
	 (externref #x6F)))

(defun wasm:type-from-id(typeid)
  (block type-lookup
	 (foreach elem wasm:types
				 (when (eq typeid (cadr elem))
					(return-from type-lookup (car elem))))
	 (raise (list "unknown type" typeid))
	 nil))


(defun wasm:type-to-id(type)
  (block type-lookup
	 (foreach elem wasm:types
				 (when (eq type (car elem))
					(return-from type-lookup (th elem 1))))
	 (raise (list "unknown type" type ))
	 nil))


(defun parse-value-type (r)
  (wasm:type-from-id (read:byte r)))

(defun parse-result-type (r)
  ($ let ((count (read:uleb r))
			 (out-list (list))))
  (dotimes (i count)
	 (push out-list (parse-value-type r)))
	
  out-list)


(defun parse-type-section (r)
  ($ let ((types (list))
			 (n (read:uleb r))))
  (dotimes (i n)
	 ($ let ((magic (read:byte r))
				(t-args (parse-result-type r))
				(t-returns (parse-result-type r))
				))
	 (assert-eq #x60 magic)
	 (push types (list t-args t-returns))
	 )
  (list 'types types)
  )

(defun parse-function-section (r type-section)
  ($ let ((fcn-types (list))
			 (n (read:uleb r))))
  (dotimes (i n)
	 ($ let ((index (read:uleb r))))
	 (push fcn-types (nth (cadr type-section) index)))
  (list 'function fcn-types)
  )

(defun utf8:decode(bytes)
  ($ let ((decoder (%js "new TextDecoder()"))))
  (decoder.decode (Uint8Array.from bytes)))

(defun utf8:encode(string)
  ($ let ((encoder (%js "new TextEncoder()"))))
  (encoder.encode string))

(defun parse-name (r)
  ($ let ((n (read:uleb r))
			 (bytes (read:bytes r n))))
  (utf8:decode bytes))
(defun parse-global-section (r)
  ($ let ((n (read:uleb r))
			 (globals (list))))
  (dotimes (i n)
	 (println 'global$ i)
	 ($ let ((type (parse-value-type r))
				(mutable (if (read:byte r) 'mutable 'const))
				(expr (parse-expr r)))
		 ;; read end marker
		 (read:byte r)

		 (push globals (println (list type mutable expr)))
		 ))
  (list 'global globals))

(defun parse-export-section (r)
  ($ let ((n (read:uleb r))
			 (exports (list))))
  (dotimes (i n)
	 ($ let ((name (parse-name r))
				(type (read:byte r))
				(index (read:uleb r))))
	 (push exports
			 (list name (case type
						(0 'funcidx)
						(1 'tableidx)
						(2 'memidx)
						(3 'globalidx))
					 index)))
	 (list 'export exports)
	 )

(defun parse-expr (r)
  
  
  (let ((op (read:byte r))
							 (op-id (th wasm:instr-lookup op))
							 (variable nil)
		  )
						(case op-id

						  ('end (progn))
						  ('local.get (set variable (read:uleb r)))
						  ('local.set (set variable (read:uleb r)))
						  ('global.get (set variable (read:uleb r)))
						  ('global.set (set variable (read:uleb r)))
						  ('i32.const (set variable (read:ileb r)))
						  ('i64.const (set variable (read:ileb r)))
						  ('f32.const (set variable (read:f32 r)))
						  ('f64.const (set variable (read:f64 r)))
						  ('i32.mul (progn))
						  ('i32.add (progn))
						  ('drop (progn))
						  
						  (otherwise (raise (list "error: unsupported opcode" op-id)))
						  )
						(if (null? variable)
							 op-id
							 (list op-id variable))))

(defun parse-code-section (r)
  (let ((nf (read:uleb r))
		  (codes (list)))
	 (dotimes (j nf)
		(let ((size (read:uleb r))
				(code (list))
				(locals (list)))
		  (let ((nlocals (read:uleb r)))
			 (dotimes (k nlocals)
				(let ((n (read:uleb r))
						(type (wasm:type-from-id (read:byte r))))
				(push locals (list type n))
				
				)))

		  (block next
			 (loop 1
					 (let ((expr (parse-expr r)))
						(push code expr)
						(when (eq expr 'end)
						  (return-from next nil))
					 )))
		
		(push codes (list code locals))))

	 (list 'code codes)
	 )
  )

(defun parse-section(r ctx)
  (let ((section-id (read:byte r))
		  (section-size (read:byte r))
		  (section-type (th wasm:section-lookup section-id))
		  (next-section (+ section-size (read:get-offset r)))
		  (section nil)
		  )
	 
	 (println 'parse-section section-type)

	 (set section
			(case section-type
			  ('code (parse-code-section r))
			  ('type (let ((type-section (parse-type-section r)))
						  (set ctx.type type-section)
						  type-section))
			  ('function (parse-function-section r ctx.type))
			  ('global (parse-global-section r))
			  ('export (parse-export-section r))
			  (otherwise (list 'unsupported-section-type section-type))))
	 (read:set-offset r next-section)
	 section
	 
  ))
(defun parse-module(r)
  (let ((magic (read:bytes r 4))
		  (version (read:bytes r 4))
		  (sections (list)))
	 (loop (not (read:finished? r))
	  (push sections (parse-section r sections)))
	 (list 'wasm magic version sections)
	 ))

(defun find-car (lst car-value)
  
  (block iter
	 (foreach item lst
				 (when (eq (car item) car-value)
					(return-from iter item)))))

(defun write-name (writer name)
  ($ let ((bytes (utf8:encode name))))
  (write:uleb writer (length bytes))
  (write:bytes writer bytes))

(defun write-type (writer type)
  (write:byte writer (wasm:type-to-id type)))

(defun write-type-array (writer array)
  (when (null? array)
	 (set array (list)))
  (write:uleb writer (length array))
  (foreach type2 array
			  (write-type writer type2)))

(defun write-types-section (w m)
  
  (let ((section (find-car (cadddr m) 'types))
		  (type-list (cadr section)))
	 (write:uleb w (length type-list))
	 (foreach type1 type-list
				 (let ((t1 (car type1))
						 (t2 (cadr type1)))
					(write:byte w #x60)
					(write-type-array w t1)
					(write-type-array w t2)
					))
  ))

(defun write-function-section (w m)
  (let ((section (find-car (cadddr m) 'function))
		  (type-section (find-car (cadddr m) 'types))
		  (function-list (cadr section)))
	 (write:uleb w (length function-list))
	 (foreach f function-list
				 (let ((idx (index-of f (cadr type-section) 1)))
					(println idx (cadr type-section) f)
					(write:uleb w idx)))))


(defun write-expr (w expr)
  (if (list? expr)
		
		(let ((instr (car expr))
				(opcode-id (wasm:instruction-to-opcode instr))
				(expr2 (cadr expr)))
		  (write:byte w opcode-id)
		  (case instr
				  ('i32.const (write:ileb w expr2))
				  ('i64.const (write:ileb w expr2))
				  ('f32.const (write:f32 w expr2))
				  ('f64.const (write:f64 w expr2))
				  (otherwise
					(when (number? expr2)
					  (write:ileb w expr2))))
		  )
		(write:byte w (wasm:instruction-to-opcode expr))))

(defun write-global-section (w m)
  (let ((section (find-car (cadddr m) 'global))
		  (globals (cadr section)))
	 (write:uleb w (length globals))
	 (foreach global globals
				 (println 'global global)
				 (let ((type (car global))
						 (mutable (eq (cadr global) 'mutable))
						 (expr (caddr global)))
					(write-type w type)
					(write:byte w (if mutable 1 0))
					(write-expr w expr)
					(write-expr w 'end)
					))))

(defun write-export-section (w m)
  (let ((section (find-car (cadddr m) 'export))
		  (exports (cadr section)))
	 (write:uleb w (length exports))
	 (foreach expt exports
				 (println 'export expt)
				 (write-name w (car expt))
				 (write:byte w (cond (cadr expt)
											('funcidx 0)
											('tableidx 1)
											('memidx 2)
											('globalidx 3)))
				 (write:uleb w (caddr expt)))))

(defun write-code-section(w m)
  (let ((section (find-car (cadddr m) 'code))
		  (codes (cadr section))
		  (n (length codes)))
	 (write:uleb w n)
	 (foreach code-entry codes
				 (let ((code (car code-entry))
						 (locals (cadr code-entry)))
					(let ((w2 (write:new)))
					
					  (write:uleb w2 (length locals))
					  (foreach local locals
								  (let ((t (car local))
										  (n (cadr local)))
									 (write:uleb w2 n)
								  (write-type w2 t)))
					  (foreach expr code
								  (write-expr w2 expr))

					  (let ((w2-array (write:to-array w2)))
						 (write:uleb w (length w2-array))
						 (write:bytes w w2-array))
					
				 )))))

(defun write-section (w m f t)
  (let ((sec-id (wasm:section-id-by-name t)))
	 (unless sec-id
		(raise (list 'sec-id-not-found t)))
	 (assert (not (undefined? sec-id)))
	 (write:byte w sec-id))
  (let ((w2 (write:new)))
	 (f w2 m)
	 (let ((bytes (write:to-array w2))
			 (l (length bytes)))
		
		(write:uleb w l)
		(write:bytes w bytes))))

(defun write-module(w m)
  (write:bytes w '(0 97 115 109))
  (write:bytes w '(1 0 0 0))
  (write-section w m write-types-section 'type)
  (write-section w m write-function-section 'function)
  (write-section w m write-global-section 'global)
  (write-section w m write-export-section 'export)
  (write-section w m write-code-section 'code)

  )

(println (nth wasm:instructions 3))

(println (read:uleb (read:from-bytes (list 255 1))))
(let ((uleb:test-values
		 '((#x0 #x0) (#x7f #x7f)
			(#x80 #x80 #x01)
			(#x3FFF #xFF #x7F)
			(#x4000 #x80 #x80 #x01)
			(#x1FFFFF #xFF #xFF #x7F))))
  (foreach item uleb:test-values
			 ;(println 'test: (car item) 'eq (read:uleb (read:from-bytes (cdr item))))
			(assert-eq (car item) (read:uleb (read:from-bytes (cdr item))))))


(let ((ileb:test-values '((0 #x0 #x0) (1 1) (32 32) (-64 64) (75600 208 206 4) (-75600 176 177 123))))
  (foreach item ileb:test-values
			  (assert-eq (car item) (println (read:ileb (read:from-bytes (cdr item)))))

			  (let ((value (read:ileb (read:from-bytes (cdr item))))
					  (w (write:new)))
				 (write:ileb w value)
				 
				 (let ((result (println (write:to-array w) '<<<))
						 (read-back (read:ileb (read:from-bytes result))))
					(println (car item) read-back)
					(assert-eq (car item) read-back)

			  ))))
	 
													;(println hex::lookup)
(let ((test-values '(0 1 100 1000 10000 100000 1000000 10000000 1000000000)))
  (foreach test-value test-values
			  (let ((w (write:new)))
				 (println (<< 1 30))
				 (write:uleb w test-value)
				 (let ((reader (read:from-bytes (write:to-array w))))
					(let ((u (read:uleb reader)))
					  (println u)
					  (assert-eq test-value u)
					  )))))




(let ((strhex "1a4005ff0013ac")
		(bytes (hex->bytes strhex))
		(strhexback (bytes->hex bytes)))
  (assert-eq strhex strhexback)
  )


(defun test-read-write-wasm ()
  (let ((r (read:from-bytes (hex->bytes wasm1))))
	 (println 'bytes (hex->bytes wasm1))
	 (let ((sections (parse-module r)))
		(println 'sections: sections)
		
		  (let ((read-back (bytes->hex (wasm->bytes sections)))) 
			 (println read-back)
			 (println 'sections sections)
			 ;(assert-equals read-back wasm1)
			 
			 ;(println '>>> (parse-module (read:from-bytes (hex->bytes read-back))))
			 (hex->bytes read-back)
			 ))))

(defun then (promise f)
  (promise.then f))
(defvar wasm2 '(wasm (0 97 115 109) (1 0 0 0) ((types (((i32 i32) (i32)) (() (i32)))) (function (((i32 i32) (i32)) (() (i32)))) (global ((i32 mutable (i32.const 75600)) (i32 mutable (i32.const -64)) (i32 mutable (i32.const -75600)))) (export (("multiply" funcidx 0) ("incf" funcidx 1))) (code ((((local.get 0) (local.get 1) i32.mul end) ()) (((global.get 0) (i64.const 1) (f32.const 2) (f64.const 3) drop drop drop (i32.const 1) i32.add (global.set 0) (global.get 0) end) ()))))))

(defun wasm->bytes (wasm)
  (let ((writer (write:new)))
	 (write-module writer wasm)
	 (write:to-array writer)))

;(test-read-write-wasm)
(println 'wasm2 wasm2)


(when 1

(let ((wasm-bin1 (wasm->bytes wasm2)))

		
  (then (WebAssembly.instantiate wasm-bin1.buffer)
		  (lambda (r)
			 (println (r.instance.exports.multiply 2 4)))
		  )))

