INSERT INTO foo (name, age, text_string, long_list, long_arr, string_list, string_arr, bit_set, type, big_decimal,
                 non_match_column, int_list, int_arr, has_deleted, id, large_text,
                 is_null, is_number, non_null, string)
VALUES ('test', 20, 'text', '{2, 4, 6}', '{1, 3, 5}', null, '{2, a, b}', E '\\x11000101', 1, 100.00,
        'non_match_column', '{1, 3, 5}', '{1, 4, 7}', false, 10000,
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.' ||
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.', false, false, true, true);