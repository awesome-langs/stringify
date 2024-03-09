use List::Util qw(reduce);
use feature qw(signatures);

sub my_string_to_int ($s) {
    return int($s);
}

sub my_string_to_double ($s) {
    return $s + 0;
}

sub my_int_to_string ($i) {
    return "$i";
}

sub my_double_to_string ($d) {
    return sprintf("%.6f", $d);
}

sub my_bool_to_string ($b) {
    return $b ? "true" : "false";
}

sub my_int_to_nullable ($i) {
    if ($i > 0) {
        return $i;
    } elsif ($i < 0) {
        return -$i;
    } else {
        return undef;
    }
}

sub my_nullable_to_int ($i) {
    return defined($i) ? $i : -1;
}

sub my_list_sorted ($lst) {
    return [sort @$lst];
}

sub my_list_sorted_by_length ($lst) {
    return [sort { length($a) <=> length($b) } @$lst];
}

sub my_list_filter ($lst) {
    return [grep { $_ % 3 == 0 } @$lst];
}

sub my_list_map ($lst) {
    return [map { $_ * $_ } @$lst];
}

sub my_list_reduce ($lst) {
    return reduce { $a * 10 + $b } 0, @$lst;
}

sub my_list_operations ($lst) {
    return reduce { $a * 10 + $b } 0, 
        map { $_ * $_ } 
            grep { $_ % 3 == 0 } @$lst;
}

sub my_list_to_dict ($lst) {
    return {map { $_ => $_ * $_ } @$lst};
}

sub my_dict_to_list ($dict) {
    return [map { $_ + $dict->{$_} } sort keys %$dict];
}

sub my_print_string ($s) {
    print $s . "\n";
}

sub my_print_string_list ($lst) {
    for my $x (@$lst) {
        print $x . " ";
    }
    print "\n";
}

sub my_print_int_list ($lst) {
    my_print_string_list([map { my_int_to_string($_) } @$lst]);
}

sub my_print_dict ($dict) {
    while (my ($k, $v) = each %$dict) {
        print my_int_to_string($k) . "->" . my_int_to_string($v) . " ";
    }
    print "\n";
}

my_print_string("Hello, World!");
my_print_string(my_int_to_string(my_string_to_int("123")));
my_print_string(my_double_to_string(my_string_to_double("123.456")));
my_print_string(my_bool_to_string(0));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))));
my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))));
my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"]));
my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"]));
my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))));
my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])));
my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]));
my_print_int_list(my_dict_to_list({3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0}));