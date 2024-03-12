use builtin qw(true false is_bool);
use feature qw(signatures);

sub new_poly_eval_type__($type_str, $type_name, $value_type, $key_type) {
    return {
        type_str => $type_str,
        type_name => $type_name,
        value_type => $value_type,
        key_type => $key_type
    };
}

sub s_to_type__($type_str) {
    if (index($type_str, "<") == -1) {
        return new_poly_eval_type__($type_str, $type_str, undef, undef);
    } else {
        my $idx = index($type_str, "<");
        my $type_name = substr($type_str, 0, $idx);
        my $other_str = substr($type_str, $idx + 1, -1);
        if (index($other_str, ",") == -1) {
            my $value_type = s_to_type__(substr($other_str, 0));
            return new_poly_eval_type__($type_str, $type_name, $value_type, undef);
        } else {
            my $idx = index($other_str, ",");
            my $key_type = s_to_type__(substr($other_str, 0, $idx));
            my $value_type = s_to_type__(substr($other_str, $idx + 1));
            return new_poly_eval_type__($type_str, $type_name, $value_type, $key_type);
        }
    }
}

sub escape_string__($s) {
    my @new_s = ();
    for my $c (split //, $s) {
        if ($c eq "\\") {
            push @new_s, "\\\\";
        } elsif ($c eq "\"") {
            push @new_s, "\\\"";
        } elsif ($c eq "\n") {
            push @new_s, "\\n";
        } elsif ($c eq "\t") {
            push @new_s, "\\t";
        } elsif ($c eq "\r") {
            push @new_s, "\\r";
        } else {
            push @new_s, $c;
        }
    }
    return join "", @new_s;
}

sub by_bool__($value) {
    return $value ? "true" : "false";
}

sub by_int__($value) {
    return int($value);
}

sub by_double__($value) {
    my $v = $value;
    my $vs = sprintf("%.6f", $v);
    while ($vs =~ /0$/) {
        $vs = substr($vs, 0, -1);
    }
    if ($vs =~ /\.$/) {
        $vs .= "0";
    }
    if ($vs eq "-0.0") {
        $vs = "0.0";
    }
    return $vs;
}

sub by_string__($value) {
    return '"' . escape_string__($value) . '"';
}

sub by_list__($value, $ty) {
    my @v_strs = map { val_to_s__($_, $ty->{value_type}) } @$value;
    return "[" . join(", ", @v_strs) . "]";
}

sub by_ulist__($value, $ty) {
    my @v_strs = map { val_to_s__($_, $ty->{value_type}) } @$value;
    return "[" . join(", ", sort @v_strs) . "]";
}

sub by_dict__($value, $ty) {
    my @v_strs = map { val_to_s__($_, $ty->{key_type}) . "=>" . val_to_s__($value->{$_}, $ty->{value_type}) } keys %$value;
    return "{" . join(", ", sort @v_strs) . "}";
}

sub by_option__($value, $ty) {
    if (!defined $value) {
        return "null";
    } else {
        return val_to_s__($value, $ty->{value_type});
    }
}

sub val_to_s__($value, $ty) {
    my $type_name = $ty->{type_name};
    if ($type_name eq "bool") {
        return by_bool__($value);
    } elsif ($type_name eq "int") {
        return by_int__($value);
    } elsif ($type_name eq "double") {
        return by_double__($value);
    } elsif ($type_name eq "str") {
        return by_string__($value);
    } elsif ($type_name eq "list") {
        return by_list__($value, $ty);
    } elsif ($type_name eq "ulist") {
        return by_ulist__($value, $ty);
    } elsif ($type_name eq "dict") {
        return by_dict__($value, $ty);
    } elsif ($type_name eq "option") {
        return by_option__($value, $ty);
    }
    die "Unknown type $type_name";
}

sub stringify__($value, $type_str) {
    return val_to_s__($value, s_to_type__($type_str)) . ":" . $type_str;
}

my $tfs = stringify__(true, "bool") . "\n" .
    stringify__(3, "int") . "\n" .
    stringify__(3.141592653, "double") . "\n" .
    stringify__(3.0, "double") . "\n" .
    stringify__("Hello, World!", "str") . "\n" .
    stringify__("!\@#\$%^&*()\\\"\n\t", "str") . "\n" .
    stringify__([1, 2, 3], "list<int>") . "\n" .
    stringify__([1, 0, 1], "list<bool>") . "\n" .
    stringify__([3, 2, 1], "ulist<int>") . "\n" .
    stringify__({1 => "one", 2 => "two"}, "dict<int,str>") . "\n" .
    stringify__({"one" => [1, 2, 3], "two" => [4, 5, 6]}, "dict<str,list<int>>") . "\n" .
    stringify__(undef, "option<int>") . "\n" .
    stringify__(3, "option<int>") . "\n";
open(my $fh, ">", "stringify.out") or die "Could not open file. $!";
print $fh $tfs;