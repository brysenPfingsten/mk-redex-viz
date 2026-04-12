use crate::language::Tree;

pub fn ev(t: Tree) -> Tree {
    match t {
        Tree::AnswerStream(_, tree) => ev(*tree),
        _ => t,
    }
}

pub fn es(t: Tree) -> Tree {
    match t {
        Tree::RightDisj(_, tree) => es(*tree),
        Tree::LeftDisj(tree, _) => es(*tree),
        Tree::Conj(tree, _) => es(*tree),
        _ => t,
    }
}

pub fn ex(t: Tree) -> Tree {
    es(ev(t))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::language::*;

    #[test]
    fn depth_1_tree_returns_itself() {
        let t: Tree = Tree::GoalState(Goal::Unify(Term::Nat(1), Term::Nat(1)), empty_state());
        assert_eq!(es(t.clone()), t.clone())
    }

    #[test]
    fn deeply_nested_disjunction() {
        let dummy_tree = Tree::GoalState(Goal::Success, empty_state());
        let unify = Tree::GoalState(Goal::Unify(Term::Nat(1), Term::Nat(1)), empty_state());
        let t = Tree::LeftDisj(
            Box::new(Tree::LeftDisj(
                Box::new(Tree::LeftDisj(
                    Box::new(unify.clone()),
                    Box::new(dummy_tree.clone()),
                )),
                Box::new(dummy_tree.clone()),
            )),
            Box::new(dummy_tree.clone()),
        );
        assert_eq!(ex(t), unify.clone())
    }
}
