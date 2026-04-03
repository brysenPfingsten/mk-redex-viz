use crate::language::*;

fn lookup(subst: &[(LogicVar, Term)], v: LogicVar) -> Option<&Term> {
    subst.iter().rev().find_map(|(k, t)| (*k == v).then_some(t))
}

fn walk<'a>(t: &'a Term, subst: &'a [(LogicVar, Term)]) -> &'a Term {
    let mut cur = t;
    while let Term::Var(v) = cur {
        if let Some(next) = lookup(subst, v.clone()) {
            cur = next;
        } else {
            break;
        }
    }
    cur
}

fn occurs(v: &LogicVar, t: &Term, subst: &[(LogicVar, Term)]) -> bool {
    let t = walk(t, subst);
    match t {
        Term::Var(v2) => *v2 == *v,
        Term::Cons(a, d) => occurs(v, a, subst) || occurs(v, d, subst),
        // add more structural cases if you introduce them later
        _ => false,
    }
}

fn extend(mut st: State, v: &LogicVar, t: &Term) -> Option<State> {
    if occurs(&v, t, &st.subst) {
        return None;
    }
    st.subst.push((v.clone(), t.clone()));
    Some(st)
}

fn unify(st: State, t1: Term, t2: Term) -> Option<State> {
    let w1 = walk(&t1, &st.subst).clone();
    let w2 = walk(&t2, &st.subst).clone();

    match (w1, w2) {
        // identical atoms
        (Term::Nat(a), Term::Nat(b)) if a == b => Some(st),
        (Term::Bool(a), Term::Bool(b)) if a == b => Some(st),
        (Term::Symbol(a), Term::Symbol(b)) if a == b => Some(st),
        (Term::Str(a), Term::Str(b)) if a == b => Some(st),
        (Term::Parameter(a), Term::Parameter(b)) if a == b => Some(st),
        (Term::Nil, Term::Nil) => Some(st),

        // var cases
        (Term::Var(v), t) => extend(st, &v, &t),
        (t, Term::Var(v)) => extend(st, &v, &t),

        // structural list/pair
        (Term::Cons(a1, d1), Term::Cons(a2, d2)) => {
            let st1 = unify(st, (*a1).clone(), (*a2).clone())?;
            unify(st1, (*d1).clone(), (*d2).clone())
        }

        // same term shortcut (covers some remaining equalities)
        (x, y) if x == y => Some(st),

        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::language::{LogicVar, Term};

    fn lv(id: u32) -> Term {
        Term::Var(LogicVar(id))
    }

    #[test]
    fn test_basic_term_unification() {
        let mt_state = empty_state();
        assert!(unify(mt_state, Term::Nat(1), Term::Nat(1)).is_some());
    }

    #[test]
    fn test_var_binds_to_atom() {
        let st = empty_state();
        let out = unify(st, lv(0), Term::Bool(true)).expect("unify should succeed");
        assert_eq!(out.subst, vec![(LogicVar(0), Term::Bool(true))]);
    }

    #[test]
    fn test_var_binds_to_var() {
        let st = empty_state();
        let out = unify(st, lv(0), lv(1)).expect("unify should succeed");
        assert_eq!(out.subst, vec![(LogicVar(0), lv(1))]);
    }

    #[test]
    fn test_structural_list_unification() {
        let st = empty_state();
        let t1 = Term::Cons(Box::new(Term::Nat(1)), Box::new(lv(0)));
        let t2 = Term::Cons(Box::new(lv(1)), Box::new(Term::Nat(2)));
        let out = unify(st, t1, t2).expect("unify should succeed");
        assert_eq!(
            out.subst,
            vec![(LogicVar(1), Term::Nat(1)), (LogicVar(0), Term::Nat(2))]
        );
    }

    #[test]
    fn test_occurs_check_rejects_cycle() {
        let st = empty_state();
        let cyc = Term::Cons(Box::new(lv(0)), Box::new(Term::Nil));
        assert!(unify(st, lv(0), cyc).is_none());
    }
}
