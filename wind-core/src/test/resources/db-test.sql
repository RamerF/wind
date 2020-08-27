INSERT INTO foo (name, text_string, long_list, long_arr, string_list, string_arr, bit_set, type, big_decimal,
                 non_match_column, int_list, int_arr, has_deleted, id)
VALUES ('test', 'text', '{2, 4, 6}', '{1, 3, 5}', null, '{2, a, b}', E'\\x11000101', 1, 100.00,
        'non_match_column', '{1, 3, 5}', '{1, 4, 7}', false, 10000);