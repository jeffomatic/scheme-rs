use super::eval::{eval_sequence, Env, EnvPtr, Value};
use super::lex::scan;
use super::parse::parse;

fn define(env: EnvPtr) {
    let stdlib = "
      (define not (lambda (a) (if a #f #t)))
      (define or (lambda (a b) (if a #t b)))
      (define and (lambda (a b) (if a b #f)))
      (define xor (lambda (a b) (if a (not b) b)))
      (define >= (lambda (a b) (or (= a b) (> a b))))
      (define < (lambda (a b) (> b a)))
      (define <= (lambda (a b) (>= b a)))
      (define list (lambda (. rest) rest))
      (define null? (lambda (v) (= v ())))
      (define list? (lambda (v)
                      (if (null? v)
                        #t
                        (if (not (pair? v))
                          #f
                          (list? (cdr v))))))
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
        ("(and #f #f)", Value::Boolean(false)),
        ("(and #f #t)", Value::Boolean(false)),
        ("(and #t #f)", Value::Boolean(false)),
        ("(and #t #t)", Value::Boolean(true)),
        ("(or #f #f)", Value::Boolean(false)),
        ("(or #f #t)", Value::Boolean(true)),
        ("(or #t #f)", Value::Boolean(true)),
        ("(or #t #t)", Value::Boolean(true)),
        ("(>= 1 0)", Value::Boolean(true)),
        ("(>= 1 1)", Value::Boolean(true)),
        ("(>= 1 2)", Value::Boolean(false)),
        ("(< 1 0)", Value::Boolean(false)),
        ("(< 1 1)", Value::Boolean(false)),
        ("(< 1 2)", Value::Boolean(true)),
        ("(<= 1 0)", Value::Boolean(false)),
        ("(<= 1 1)", Value::Boolean(true)),
        ("(< 1 2)", Value::Boolean(true)),
        (
            "(list 1 2 3)",
            Value::make_list(&[
                Value::Number(1.0).into_ptr(),
                Value::Number(2.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
            ]),
        ),
        ("(null? ())", Value::Boolean(true)),
        ("(null? #f)", Value::Boolean(false)),
        ("(list? ())", Value::Boolean(true)),
        ("(list? (list 1))", Value::Boolean(true)),
        (r#"(list? (list 1 #t #f "hello" ()))"#, Value::Boolean(true)),
        ("(list? 1234)", Value::Boolean(false)),
        ("(list? (cons 1 2))", Value::Boolean(false)),
    ];

    for c in cases.iter() {
        let got = eval_sequence(&parse(&scan(c.0).unwrap()).unwrap(), env.clone()).unwrap();
        assert_eq!(&*got.borrow(), &c.1, "src: {}", c.0);
    }
}
