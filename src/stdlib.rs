use super::eval::{eval_sequence, Env, EnvPtr, Value};
use super::lex::scan;
use super::parse::parse;

fn define(env: EnvPtr) {
    let stdlib = "
      ; add bindings for operators so they can be used in higher-order functions
      (define (car pair) (car pair))
      (define (cdr pair) (cdr pair))
      (define (cons a b) (cons a b))
      (define (null? v) (null? v))
      (define (bool? v) (bool? v))
      (define (string? v) (string? v))
      (define (number? v) (number? v))
      (define (pair? v) (pair? v))
      (define (+ a b) (+ a b))
      (define (- a b) (- a b))
      (define (* a b) (* a b))
      (define (/ a b) (/ a b))
      (define (= a b) (= a b))
      (define (> a b) (> a b))

      ; some things that maybe should just be operators
      (define (not a) (if a #f #t))
      (define (>= a b) (or (= a b) (> a b)))
      (define (< a b) (> b a))
      (define (<= a b) (>= b a))

      ; list processing
      (define (list . rest) rest)
      (define (list? v)
                (if (null? v)
                  #t
                  (if (not (pair? v))
                    #f
                    (list? (cdr v)))))
      (define (map proc items)
                (if (null? items)
                  ()
                  (cons (proc (car items))
                        (map proc (cdr items)))))
      (define (reduce proc init items)
                (if (null? items)
                  init
                  (reduce proc (proc init (car items)) (cdr items))))
      (define (filter proc items)
                (if (null? items)
                  ()
                  (let ((first (car items))
                        (rest  (cdr items)))
                    (if (proc first)
                      (cons first (filter proc rest))
                      (filter proc rest)))))
      (define (count items)
                (reduce
                  (lambda (memo _) (+ memo 1))
                  0
                  items))
      (define (any? proc items)
                (if (null? items)
                  #f
                  (if (proc (car items))
                    #t
                    (any? proc (cdr items)))))
      (define (all? proc items)
                (not
                  (any?
                    (lambda (v) (not (proc v)))
                    items)))
    ";
    eval_sequence(&parse(&scan(stdlib).unwrap()).unwrap(), env).unwrap();
}

#[test]
fn test() {
    let env = Env::root().into_ptr();
    define(env.clone());

    let cases = &[
        ("(not #t)", Value::Boolean(false)),
        ("(not #f)", Value::Boolean(true)),
        ("(>= 1 0)", Value::Boolean(true)),
        ("(>= 1 1)", Value::Boolean(true)),
        ("(>= 1 2)", Value::Boolean(false)),
        ("(< 1 0)", Value::Boolean(false)),
        ("(< 1 1)", Value::Boolean(false)),
        ("(< 1 2)", Value::Boolean(true)),
        ("(<= 1 0)", Value::Boolean(false)),
        ("(<= 1 1)", Value::Boolean(true)),
        ("(< 1 2)", Value::Boolean(true)),
        ("(list)", Value::Null),
        (
            "(list 1 2 3)",
            Value::make_list(&[
                Value::Number(1.0).into_ptr(),
                Value::Number(2.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
            ]),
        ),
        ("(list? ())", Value::Boolean(true)),
        ("(list? (list 1))", Value::Boolean(true)),
        (r#"(list? (list 1 #t #f "hello" ()))"#, Value::Boolean(true)),
        ("(list? 1234)", Value::Boolean(false)),
        ("(list? (cons 1 2))", Value::Boolean(false)),
        (
            "(map (lambda (v) (+ v 1)) (list 1 2 3))",
            Value::make_list(&[
                Value::Number(2.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
                Value::Number(4.0).into_ptr(),
            ]),
        ),
        ("(reduce + 1 ())", Value::Number(1.0)),
        ("(reduce + 1 (list 1 2 3))", Value::Number(7.0)),
        ("(filter (lambda (v) (> v 0)) ())", Value::Null),
        (
            "(filter (lambda (v) (> v 0)) (list 1 -2 3 -4 5))",
            Value::make_list(&[
                Value::Number(1.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
                Value::Number(5.0).into_ptr(),
            ]),
        ),
        ("(count ())", Value::Number(0.0)),
        ("(count (list 1))", Value::Number(1.0)),
        ("(count (list 1 2 3))", Value::Number(3.0)),
        ("(any? (lambda (v) (> v 0)) ())", Value::Boolean(false)),
        ("(any? (lambda (v) (> v 0)) (list 1))", Value::Boolean(true)),
        (
            "(any? (lambda (v) (> v 0)) (list -1))",
            Value::Boolean(false),
        ),
        (
            "(any? (lambda (v) (> v 0)) (list 1 -2 -3))",
            Value::Boolean(true),
        ),
        (
            "(any? (lambda (v) (> v 0)) (list -1 -2 3))",
            Value::Boolean(true),
        ),
        (
            "(any? (lambda (v) (> v 0)) (list -1 -2 -3))",
            Value::Boolean(false),
        ),
        ("(all? (lambda (v) (> v 0)) ())", Value::Boolean(true)),
        (
            "(all? (lambda (v) (> v 0)) (list 1 2 3))",
            Value::Boolean(true),
        ),
        (
            "(all? (lambda (v) (> v 0)) (list 1 2 -3))",
            Value::Boolean(false),
        ),
    ];

    for c in cases.iter() {
        println!("{}", c.0);
        let got = eval_sequence(&parse(&scan(c.0).unwrap()).unwrap(), env.clone()).unwrap();
        assert_eq!(&*got.borrow(), &c.1, "src: {}", c.0);
    }
}
