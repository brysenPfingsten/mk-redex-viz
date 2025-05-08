import type { languages } from 'monaco-editor';

export const conf: languages.LanguageConfiguration = {
  comments: {
    lineComment: ';',
    blockComment: ['#|', '|#']
  },
  brackets: [['(', ')'], ['{', '}'], ['[', ']']],
  autoClosingPairs: [
    { open: '{', close: '}' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
    { open: '"', close: '"' }
  ],
  surroundingPairs: [
    { open: '{', close: '}' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
    { open: '"', close: '"' }
  ]
};

export const language: languages.IMonarchLanguage = {
  defaultToken: '',
  ignoreCase: false,
  tokenPostfix: '.minikanren',
  brackets: [
    { open: '(', close: ')', token: 'delimiter.parenthesis' },
    { open: '{', close: '}', token: 'delimiter.curly' },
    { open: '[', close: ']', token: 'delimiter.square' }
  ],
  keywords: [
    // mk
    'defrel','fresh','conde',
    // scheme
    'case','do','let','loop','if','else','when',
    'cons','car','cdr','cond','lambda','lambda*',
    'syntax-rules','format','set!','quote','eval',
    'append','list','list?','member?','load'
  ],
  constants: ['#t', '#f'],
  operators: ['==','eq?','eqv?','equal?','and','or','not','null?'],
  tokenizer: {
    root: [
      [/#\|/, 'comment', '@comment'],
      [/;.*/, 'comment'],
      [/[+-]?\d+(\.\d+)?/, 'number'],
      [/"/, { token: 'string.quote', next: '@string' }],
      [/\b(defrel|fresh|conde|case|do|let|loop|if|else|when|cons|car|cdr|cond|lambda|lambda\*|syntax-rules|format|set!|quote|eval|append|list|list\?|member\?|load)\b/, 'keyword'],
      [/#(?:true|false)\b/, 'constant'],
      [/#t|#f/, 'constant'],
      [/\b(eq\?|eqv\?|equal\?|and|or|not|null\?)\b/, 'operator'],
      [/'[a-zA-Z_][\w\-\?\!\*]*/, 'type.symbol'],
      [/[a-zA-Z_#][a-zA-Z0-9_\-\?\!\*]*/, 'identifier'],
      [/[()\[\]{}]/, '@brackets']
    ],
    comment: [
      [/[^#|]+/, 'comment'],
      [/#\|/, 'comment', '@push'],
      [/\|#/, 'comment', '@pop'],
      [/[#|]/, 'comment']
    ],
    string: [
      [/[^\\"]+/, 'string'],
      [/\\./, 'string.escape'],
      [/"/, { token: 'string.quote', next: '@pop' }]
    ]
  }
};
