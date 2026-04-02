const appendo = `(defrel (appendo l s out)
  (conde
    [(== l '())
    (== s out)]
    [(fresh (a d res)
      (== l (cons a d))
      (== out (cons a res))
      (appendo d s res))]
  ))

(run* (q) (appendo (list 'minikanren) (list 'visualizer) q))`;

const appendoh1 = `(defrel (appendoh l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
      (== l (cons a d))
      (== out (cons a res))
      (appendoh d s out))]))

(run* (q) (appendoh '(dog) q '(dog cat)))`;

const appendoh2 = `(defrel (appendoh l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
      (appendoh d s res)
      (== l (cons a d))
      (== out (cons a res)))]))

(run* (q r s) (appendoh q r s))`;

const same = `(defrel (same x y)
  (== x y))

(run* (q)
  (conde
    [(conde
       [(same q 'turtle)]
       [(same q 'cat)]
       [(== q 'dog)])]
    [(same q 'fish)]))`;

const div3o = `(defrel (same-counto bn)
  (conde
   [(== bn \`(1 1))]
   [(fresh (a ad dd)
      (== \`(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (same-counto dd)]
       [(== \`(,a ,ad) '(1 0)) (mod+1o dd)]
       [(== \`(,a ,ad) '(0 1)) (mod+2o dd)]))]))

(defrel (mod+1o bn)
  (conde
   [(== bn \`(0 1))]
   [(fresh (a ad dd)
      (== \`(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (mod+1o dd)]
       [(== \`(,a ,ad) '(1 0)) (mod+2o dd)]
       [(== \`(,a ,ad) '(0 1)) (same-counto dd)]))]))

(defrel (mod+2o bn)
  (conde
   [(== bn '(1))]
   [(fresh (a ad dd)
      (== \`(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (mod+2o dd)]
       [(== \`(,a ,ad) '(1 0)) (same-counto dd)]
       [(== \`(,a ,ad) '(0 1)) (mod+1o dd)]))]))

(defrel (multiple-of-threeo bn)
  (conde
   [(== bn '())]
   [(same-counto bn)]))

(run* (q) (multiple-of-threeo q))`;

const fivesFours = `(defrel (fives x)
  (conde
    [(fives x)]
    [(== x 'five)]))

(defrel (fours x)
  (conde
    [(fours x)]
    [(== x 'four)]))

(run 8 (q)
  (conde
    [(fives q)]
    [(fours q)]))`;

const coreFreshConjUnify = `(run* (q)
  (fresh (x y)
    (== x (cons 'ok '()))
    (== y (cons 'ok '()))
    (== q x)
    (== q y)))`;

export const semanticExamples = Object.freeze([
  Object.freeze({
    id: "core-fresh-conj-unify",
    label: "core/fresh+conj+unify",
    miniSource: coreFreshConjUnify,
  }),
  Object.freeze({
    id: "appendo",
    label: "appendo",
    miniSource: appendo,
  }),
  Object.freeze({
    id: "appendoh-1",
    label: "appendoh 1",
    miniSource: appendoh1,
  }),
  Object.freeze({
    id: "appendoh-2",
    label: "appendoh 2",
    miniSource: appendoh2,
  }),
  Object.freeze({
    id: "fives-fours",
    label: "fives/fours",
    miniSource: fivesFours,
  }),
  Object.freeze({
    id: "same",
    label: "same",
    miniSource: same,
  }),
  Object.freeze({
    id: "div3o",
    label: "div3o",
    miniSource: div3o,
  }),
]);

export function exampleById(exampleId) {
  return semanticExamples.find(({ id }) => id === exampleId) ?? null;
}

export function exampleOptions() {
  return [
    { value: "", label: "Examples" },
    ...semanticExamples
      .map(({ id, label }) => ({ value: id, label })),
  ];
}
