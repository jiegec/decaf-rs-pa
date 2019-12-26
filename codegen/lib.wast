    ;; Bump allocator
    ;; Current extent is store in mem[0]
    (func $_Alloc (param i32) (result i32)
        (local i32)
        (set_local 1 (i32.load (i32.const 0)))
        (set_local 0 (i32.shl (i32.shr_s (i32.add (i32.const 3) (get_local 0)) (i32.const 2)) (i32.const 2)))
        (i32.store (i32.const 0) (i32.add (get_local 1)
            (get_local 0)))
        (get_local 1)
        )

    ;; Below print functions use WASI fd_write
    (func $_PrintString (param i32) (result i32)
        (local i32 i32) ;; iov, len
        (set_local 1 (call $_Alloc (i32.const 12))) ;; iov
        (i32.store (get_local 1) (get_local 0)) ;; base
        (loop
            (if
                (i32.ne (i32.load8_u (i32.add (get_local 0) (get_local 2))) (i32.const 0))
                (then
                  (set_local 2 (i32.add (get_local 2) (i32.const 1))) ;; len ++
                  (br 1)))
        )
        (i32.store (i32.add (get_local 1) (i32.const 4)) (get_local 2)) ;; len
        (call $fd_write
              (i32.const 1) ;; stdout
              (get_local 1) ;; iov
              (i32.const 1) ;; only 1 iov
              (i32.add (get_local 1) (i32.const 8)) ;; nwritten
              )
        )

    (func $_PrintInt (param i32) (result i32)
        (if
            (i32.lt_s (get_local 0) (i32.const 0))
            (then
                (drop (call $_PrintChar (i32.const 45)))
                (return (call $_PrintInt (i32.sub (i32.const 0) (get_local 0)))) ;; TODO: handle overflow
            ))
        (if
            (i32.ge_s (get_local 0) (i32.const 10))
            (then
                (drop (call $_PrintInt (i32.div_s (get_local 0) (i32.const 10))))
            ))
        (call $_PrintChar (i32.add (i32.rem_s (get_local 0) (i32.const 10)) (i32.const 48))))

    ;; Use WASI fd_write
    (func $_PrintBool (param i32) (result i32)
        (if (result i32)
            (get_local 0)
            (then
                (drop (call $_PrintChar (i32.const 116))) ;; 't'
                (drop (call $_PrintChar (i32.const 114))) ;; 'r'
                (drop (call $_PrintChar (i32.const 117))) ;; 'u'
                (call $_PrintChar (i32.const 101)) ;; 'e'
            )
            (else
                (drop (call $_PrintChar (i32.const 102))) ;; 'f'
                (drop (call $_PrintChar (i32.const 97))) ;; 'a'
                (drop (call $_PrintChar (i32.const 108))) ;; 'l'
                (drop (call $_PrintChar (i32.const 115))) ;; 's'
                (call $_PrintChar (i32.const 101)) ;; 'e'
            )))

    (func $_PrintChar (param i32) (result i32)
        (local i32 i32) ;; ch, iov
        (set_local 1 (call $_Alloc (i32.const 1))) ;; ch
        (set_local 2 (call $_Alloc (i32.const 12))) ;; iov
        (i32.store (get_local 2) (get_local 1)) ;; base
        (i32.store (i32.add (get_local 2) (i32.const 4)) (i32.const 1)) ;; len
        (i32.store (get_local 1) (get_local 0)) ;; ch
        (call $fd_write
              (i32.const 1) ;; stdout
              (get_local 2) ;; iov
              (i32.const 1) ;; only 1 iov
              (i32.add (get_local 1) (i32.const 8)) ;; nwritten
              )
        )

    (func $_start
        (drop (call $main)))
    (export "_start" (func $_start))
    (export "main" (func $_start))
    )
