# Builtin SICSTUS Prolog Predicates

### abolish/[1,2]

    abolish(+Predicates).
    abolish(+Predicates, +Options).

Removes procedures from the Prolog database.
Predicates is a predicate or a list of predicates.

### abort/0

    abort.

Abandons the current execution and returns to the beginning of the current break level, or terminates the enclosing query.

### acyclic_term/1

    acyclic_term(+Term).

Term is currently instantiated to a finite and acyclic term.

### append/3

    append(?List1, ?List2, ?List3).

Appends list List1 and List2, forming List3. Three-way predicate.

### arg/3

    arg(+ArgNum, +Term, -Arg).

Unifies Arg with the ArgNum argument of term Term.
ArgNum  integer, nonvar and non-negative.
Term    compound, nonvar.
Arg     term.

### assert/[1,2]

    assert(+Clause).

Adds the dynamic clause Clause to the Prolog database.
Undefined whether Clause will precede or follow clauses already in the database.

### asserta/[1,2]

    asserta(+Clause).

Adds the dynamic clause Clause to the Prolog database, before any other clause.

### assertz/[1,2]

    assertz(+Clause).

Adds the dynamic clause Clause to the Prolog database, after any other clause.

### atom/1

    atom(+Term).

Succeeds if Term is an atom.

### atom_chars/2

    atom_chars(+Atom, -Chars).
    atom_chars(-Atom, +Chars).

Chars is a list of chars comprising the printed representation of Atom.
Either must be instantiated.

### atom_codes/2

    atom_codes(+Atom, -Codes).
    atom_codes(-Atom, +Codes).

Codes is a list of codes comprising the printed representation of Atom.
Either must be instantiated.

### atom_concat/3

    atom_concat(+Atom1, +Atom2, -Atom12).
    atom_concat(-Atom1, -Atom2, +Atom12).

Like append/3, but for atoms.

### atom_length/2

    atom_length(+Atom, -Length).

Like length/2, but for atoms.

### atomic/1

    atomic(+Term) :- atom(+Term); integer(+Term).

Succeeds if Term is an atom or integer.

### bagof/3

    bagof(+Template, +Generator, -Set).

See also findall/3 and setof/3.
Like setof/3 except that the list or alternative lists returned
will not be ordered and may contain duplicates.

### block/1

    :- block +BlockSpec

:BlockSpec
    callable, ground.
    Goal template or list of goal templates, of the form
    f(Arg1, Arg2, ...)
    where ArgN is one of:
    -   part of a block condition
    ?   otherwise

When a goal for a block declared predicate is to be executed, the block specs are interpreted as conditions for blocking the goal, and if at least one condition evaluates to true, then the goal is blocked.
A block condition evaluates to true iff all arguments specified as - are uninstantiated, in which case the goal is blocked until at least one of those variables is instantiated. If several conditions evaluate to true, the implementation picks one of them.

### break/0

    break.

Causes the current execution to be interrupted; enters next break level.

### call/[1,2,...,255]

    call(+P).
    call(+P,?Q,...).

Executes goal P, optionally obtained by augmenting P with remaining arguments.

### callable/1

    callable(+Term).

Succeeds if Term is instantiated to an atom or compound term.
