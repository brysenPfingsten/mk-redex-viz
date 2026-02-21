use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LexicalVar(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LogicVar(pub u32);

#[derive(Clone, Debug)]
pub struct RelationDef {
    pub name: String,
    pub params: Vec<LexicalVar>,
    pub body: Goal,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Symbol(String),
    Nat(u64),
    Var(LogicVar),
    Bool(bool),
    Str(String),
    Parameter(LexicalVar),
    Nil,
    Cons(Box<Term>, Box<Term>),
}

#[derive(Clone, Debug)]
pub struct State {
    pub subst: Vec<(LogicVar, Term)>,
    pub counter: u32,
}

#[derive(Clone, Debug)]
pub enum Goal {
    Success,
    Unify(Term, Term),
    RelCall(String, Vec<Term>),
    Disj(Box<Goal>, Box<Goal>),
    Conj(Box<Goal>, Box<Goal>),
    Fresh(Vec<LexicalVar>, Box<Goal>),
}

#[derive(Clone, Debug)]
pub enum Tree {
    Fail,
    GoalState(Goal, State),
    RightDisj(Box<Tree>, Box<Tree>),
    LeftDisj(Box<Tree>, Box<Tree>),
    AnswerStream(State, Box<Tree>),
    Conj(Box<Tree>, Goal),
    Go(Box<Tree>),
    Delay(Box<Tree>),
}

pub struct Prog {
    pub relations: HashMap<String, RelationDef>,
}
