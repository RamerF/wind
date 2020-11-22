INSERT INTO foo (name, age, text_string, type, big_decimal, non_match_column, has_deleted, id, bit_set, large_text,
                 is_null, is_number, non_null, string)
VALUES ('test', 20, 'text', 1, 100.00, 'non_match_column', false, 10000, 0x11000101,
        '佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.佛祖保佑莫得BUG.', false, false, true, true);