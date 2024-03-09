sub my-string-to-int (Str:D $s) returns Int:D {
    return $s.Int;
}

sub my-string-to-double (Str:D $s) returns Num:D {
    return $s.Num;
}

sub my-int-to-string (Int:D $i) returns Str:D {
    return $i.Str;
}

sub my-double-to-string (Num:D $d) returns Str:D {
    return sprintf("%.6f", $d);
}

sub my-bool-to-string (Bool:D $b) returns Str:D {
    return $b ?? "true" !! "false";
}

sub my-int-to-nullable (Int:D $i) returns Int:_ {
    if $i > 0 {
        return $i;
    } elsif $i < 0 {
        return -$i;
    } else {
        return Int:U;
    }
}

sub my-nullable-to-int (Int:_ $i) returns Int:D {
    return $i.defined ?? $i !! -1;
}

sub my-list-sorted (Array:D[Str:D]() $lst) returns Array:D[Str:D]() {
    return $lst.sort;
}

sub my-list-sorted-by-length (Array:D[Str:D]() $lst) returns Array:D[Str:D]() {
    return $lst.sort({$^a.chars <=> $^b.chars});
}

sub my-list-filter (Array:D[Int:D]() $lst) returns Array:D[Int:D]() {
    return $lst.grep({$_ %% 3});
}

sub my-list-map (Array:D[Int:D]() $lst) returns Array:D[Int:D]() {
    return $lst.map({$_ * $_});
}

sub my-list-reduce (Array:D[Int:D]() $lst) returns Int:D {
    return (0, |$lst).reduce({$^a * 10 + $^b});
}

sub my-list-operations (Array:D[Int:D]() $lst) returns Int:D {
    return (0, |($lst.grep(-> $x { $x %% 3 })
        .map(-> $x { $x * $x })))
        .reduce(-> $acc, $x {$acc * 10 + $x});
}

sub my-list-to-dict (Array:D[Int:D]() $lst) returns Hash:D[Int:D, Int:D]() {
    return Hash[Int, Int]($lst.map({$_ => $_ * $_}));
}

sub my-dict-to-list (Hash:D[Int:D, Int:D]() $dict) returns Array:D[Int:D]() {
    return $dict.sort.map({ $_.key.Int + $_.value.Int });
}

sub my-print-string (Str:D $s) {
    say $s;
}

sub my-print-string-list (Array:D[Str:D]() $lst) {
    for $lst -> $x {
        print $x ~ " ";
    }
    say "";
}

sub my-print-int-list (Array:D[Int:D]() $lst) {
    my-print-string-list($lst.map({my-int-to-string($_)}));
}

sub my-print-dict (Hash:D[Int:D, Int:D]() $dict) {
    for $dict.kv -> $k, $v {
        print my-int-to-string($k) ~ "->" ~ my-int-to-string($v) ~ " ";
    }
    say "";
}

my-print-string("Hello, World!");
my-print-string(my-int-to-string(my-string-to-int("123")));
my-print-string(my-double-to-string(my-string-to-double("123.456")));
my-print-string(my-bool-to-string(False));
my-print-string(my-int-to-string(my-nullable-to-int(my-int-to-nullable(18))));
my-print-string(my-int-to-string(my-nullable-to-int(my-int-to-nullable(-15))));
my-print-string(my-int-to-string(my-nullable-to-int(my-int-to-nullable(0))));
my-print-string-list(my-list-sorted(["e", "dddd", "ccccc", "bb", "aaa"]));
my-print-string-list(my-list-sorted-by-length(["e", "dddd", "ccccc", "bb", "aaa"]));
my-print-string(my-int-to-string(my-list-reduce(my-list-map(my-list-filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
my-print-string(my-int-to-string(my-list-operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
my-print-dict(my-list-to-dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
my-print-int-list(my-dict-to-list(:{3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0}));
