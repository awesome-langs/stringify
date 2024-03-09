defmodule Example do
    def my_string_to_int(s) do
        String.to_integer(s)
    end

    def my_string_to_double(s) do
        String.to_float(s)
    end

    def my_int_to_string(i) do
        Integer.to_string(i)
    end

    def my_double_to_string(d) do
        :erlang.float_to_binary(d, decimals: 6)
    end

    def my_bool_to_string(b) do
        if b, do: "true", else: "false"
    end

    def my_int_to_nullable(i) do
        cond do
            i > 0 -> i
            i < 0 -> -i
            true -> nil
        end
    end

    def my_nullable_to_int(i) do
        case i do
            nil -> -1
            x -> x
        end
    end

    def my_list_sorted(lst) do
        Enum.sort(lst)
    end

    def my_list_sorted_by_length(lst) do
        Enum.sort_by(lst, &String.length/1)
    end

    def my_list_filter(lst) do
        Enum.filter(lst, &(rem(&1, 3) == 0))
    end

    def my_list_map(lst) do
        Enum.map(lst, &(&1 * &1))
    end

    def my_list_reduce(lst) do
        Enum.reduce(lst, 0, &(&2 * 10 + &1))
    end

    def my_list_operations(lst) do
        lst |> Enum.filter(fn x -> rem(x, 3) == 0 end)
        |> Enum.map(fn x -> x * x end)
        |> Enum.reduce(0, fn x, acc -> acc * 10 + x end)
    end

    def my_list_to_dict(lst) do
        Map.new(lst, fn x -> {x, x * x} end)
    end

    def my_dict_to_list(dict) do
        dict |> Enum.to_list()
        |> Enum.sort()
        |> Enum.map(fn {k, v} -> k + v end)
    end

    def my_print_string(s) do
        IO.puts(s)
    end

    def my_print_string_list(lst) do
        for x <- lst do
            IO.write(x <> " ")
        end
        IO.puts("")
    end

    def my_print_int_list(lst) do
        lst |> Enum.map(&my_int_to_string/1) |> my_print_string_list()
    end

    def my_print_dict(dict) do
        for {k, v} <- dict do
            IO.write(my_int_to_string(k) <> "->" <> my_int_to_string(v) <> " ")
        end
        IO.puts("")
    end

    def main() do
        my_print_string("Hello, World!")
        my_print_string(my_int_to_string(my_string_to_int("123")))
        my_print_string(my_double_to_string(my_string_to_double("123.456")))
        my_print_string(my_bool_to_string(false))
        my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(18))))
        my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(-15))))
        my_print_string(my_int_to_string(my_nullable_to_int(my_int_to_nullable(0))))
        my_print_string_list(my_list_sorted(["e", "dddd", "ccccc", "bb", "aaa"]))
        my_print_string_list(my_list_sorted_by_length(["e", "dddd", "ccccc", "bb", "aaa"]))
        my_print_string(my_int_to_string(my_list_reduce(my_list_map(my_list_filter([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))))
        my_print_string(my_int_to_string(my_list_operations([3, 12, 5, 8, 9, 15, 7, 17, 21, 11])))
        my_print_dict(my_list_to_dict([3, 1, 4, 2, 5, 9, 8, 6, 7, 0]))
        my_print_int_list(my_dict_to_list(%{3 => 9, 1 => 1, 4 => 16, 2 => 4, 5 => 25, 9 => 81, 8 => 64, 6 => 36, 7 => 49, 0 => 0}))
    end
end

Example.main()