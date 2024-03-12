class PolyEvalType {
    has $.type-str;
    has $.type-name;
    has $.value-type;
    has $.key-type;

    method new($type-str, $type-name, $value-type, $key-type) {  
        return self.bless(:$type-str, :$type-name, :$value-type, :$key-type);
    }  
}

sub s-to-type__($type-str) {
    if $type-str.index("<") -> $idx {
        my $type-name = $type-str.substr(0, $idx);
        my $other-str = $type-str.substr($idx + 1, * - 1);
        if $other-str.index(",") -> $idx {
            my $key-type = s-to-type__($other-str.substr(0, $idx));
            my $value-type = s-to-type__($other-str.substr($idx + 1, *));
            PolyEvalType.new($type-str, $type-name, $value-type, $key-type);
        }
        else {
            my $value-type = s-to-type__($other-str);
            PolyEvalType.new($type-str, $type-name, $value-type, Nil);
        }
    }
    else {
        PolyEvalType.new($type-str, $type-str, Nil, Nil);
    }
}

sub escape-string__($s) {
    my @new-s = $s.comb.map: -> $c {
        given $c {
            when "\\" { "\\\\" }
            when "\"" { "\\\"" }
            when "\n" { "\\n" }
            when "\t" { "\\t" }
            when "\r" { "\\r" }
            default { $c }
        }
    }
    @new-s.join;
}

sub by-bool__($value) {
    $value ?? "true" !! "false";
}

sub by-int__($value) {
    $value.Str;
}

sub by-double__($value) {
    my $vs = sprintf("%.6f", $value);
    $vs .=subst(/0+$/, "");
    $vs .=subst(/\.$/, ".0");
    $vs eq "-0.0" ?? "0.0" !! $vs;
}

sub by-string__($value) {
    '"' ~ escape-string__($value) ~ '"';
}

sub by-list__($value, $ty) {
    my @v-strs = $value.map: -> $v { val-to-s__($v, $ty.value-type) };
    '[' ~ @v-strs.join(", ") ~ ']';
}

sub by-ulist__($value, $ty) {
    my @v-strs = $value.map: -> $v { val-to-s__($v, $ty.value-type) };
    '[' ~ @v-strs.sort.join(", ") ~ ']';
}

sub by-dict__($value, $ty) {
    my @v-strs = $value.pairs.map: -> $pair { val-to-s__($pair.key, $ty.key-type) ~ "=>" ~ val-to-s__($pair.value, $ty.value-type) };
    '{' ~ @v-strs.sort.join(", ") ~ '}';
}

sub by-option__($value, $ty) {
    $value.defined ?? val-to-s__($value, $ty.value-type) !! "null";
}

sub val-to-s__($value, $ty) {
    given $ty.type-name {
        when "bool" { by-bool__($value) }
        when "int" { by-int__($value) }
        when "double" { by-double__($value) }
        when "str" { by-string__($value) }
        when "list" { by-list__($value, $ty) }
        when "ulist" { by-ulist__($value, $ty) }
        when "dict" { by-dict__($value, $ty) }
        when "option" { by-option__($value, $ty) }
        default { die "Unknown type" ~ $ty.type-name }
    }
}

sub stringify__($value, $type-str) {
    val-to-s__($value, s-to-type__($type-str)) ~ ":" ~ $type-str;
}

my $tfs = stringify__(True, "bool") ~ "\n" ~
    stringify__(3, "int") ~ "\n" ~
    stringify__(3.141592653, "double") ~ "\n" ~
    stringify__(3.0, "double") ~ "\n" ~
    stringify__("Hello, World!", "str") ~ "\n" ~
    stringify__("!@#\$%^&*()\\\"\n\t", "str") ~ "\n" ~
    stringify__([1, 2, 3], "list<int>") ~ "\n" ~
    stringify__([True, False, True], "list<bool>") ~ "\n" ~
    stringify__([3, 2, 1], "ulist<int>") ~ "\n" ~
    stringify__(%{1 => "one", 2 => "two"}, "dict<int,str>") ~ "\n" ~
    stringify__(%{"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") ~ "\n" ~
    stringify__(Nil, "option<int>") ~ "\n" ~
    stringify__(3, "option<int>") ~ "\n";
"stringify.out".IO.spurt($tfs);