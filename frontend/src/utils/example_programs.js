import { ALL_MODEL_IDS } from "./model_ids.js";

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

(run* (q) (appendoh '(dog) q '(dog cat)))`

const appendoh2 = `(defrel (appendoh l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
      (appendoh d s res)
      (== l (cons a d))
      (== out (cons a res)))]))

(run* (q r s) (appendoh q r s))`

const same = `(defrel (same x y)
  (== x y))

(run* (q)
  (conde
    [(conde
       [(same q 'turtle)]
       [(same q 'cat)]
       [(== q 'dog)])]
    [(same q 'fish)]))`

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

(run* (q) (multiple-of-threeo q))`

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
    [(fours q)]))`

const coreFreshConjUnify = `(run* (q)
  (fresh (x y)
    (== x (cons 'ok '()))
    (== y (cons 'ok '()))
    (== q x)
    (== q y)))`

export const exampleProgs = [
  { value: "", label: "Examples", models: ALL_MODEL_IDS },
  { value: coreFreshConjUnify, label: "core/fresh+conj+unify", models: ALL_MODEL_IDS },
  { value: appendo, label: "appendo", models: ALL_MODEL_IDS },
  { value: appendoh1, label: "appendoh 1", models: ALL_MODEL_IDS },
  { value: appendoh2, label: "appendoh 2", models: ALL_MODEL_IDS },
  { value: fivesFours, label: "fives/fours", models: ALL_MODEL_IDS },
  { value: same, label: "same", models: ALL_MODEL_IDS },
  { value: div3o, label: "div3o", models: ALL_MODEL_IDS },
];

export function examplesForModel(model) {
  // Compatibility is computed dynamically via backend analysis.
  // Keep all examples selectable so users can choose model-first or example-first.
  return exampleProgs;
}
