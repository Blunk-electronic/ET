-- Standalone testbench for et_kicad_v6.sexp (the S-expression tokenizer/
-- parser). Build with "gprbuild -j2" from this directory, run with "./test".
--
-- Phase 1 gate per the project plan: literal snippets first, then
-- parse_file on the small real example pg_01.kicad_sch.

with ada.text_io;				use ada.text_io;
with ada.exceptions;			use ada.exceptions;
with ada.command_line;			use ada.command_line;

with et_kicad_v6;
with et_kicad_v6.sexp;			use et_kicad_v6.sexp;

procedure test is

	pass_count, fail_count : natural := 0;

	procedure check (condition : in boolean; label : in string) is
	begin
		if condition then
			pass_count := pass_count + 1;
			put_line ("  PASS  " & label);
		else
			fail_count := fail_count + 1;
			put_line ("  FAIL  " & label);
		end if;
	end check;

	procedure check_raises_syntax_error (label : in string; test_proc : access procedure) is
	begin
		test_proc.all;
		fail_count := fail_count + 1;
		put_line ("  FAIL  " & label & " (expected sexp_syntax_error, none raised)");
	exception
		when sexp_syntax_error =>
			pass_count := pass_count + 1;
			put_line ("  PASS  " & label);
		when e : others =>
			fail_count := fail_count + 1;
			put_line ("  FAIL  " & label & " (wrong exception: " & exception_name (e) & ")");
	end check_raises_syntax_error;


	----------------------------------------------------------------
	-- 1. Simple flat list
	----------------------------------------------------------------
	procedure test_simple_list is
		n : constant type_node := parse_string ("(a b c)");
	begin
		check (kind (n) = SEXP_LIST, "simple list: kind = SEXP_LIST");
		check (child_count (n) = 3, "simple list: 3 children");
		check (head (n) = "a", "simple list: head = ""a""");
		check (atom_text (get_child (n, 2)) = "b", "simple list: child 2 = ""b""");
		check (not atom_was_quoted (get_child (n, 2)), "simple list: child 2 not quoted");
	end test_simple_list;


	----------------------------------------------------------------
	-- 2. Nested list
	----------------------------------------------------------------
	procedure test_nested_list is
		n : constant type_node := parse_string ("(a (b c) d)");
		inner : type_node;
	begin
		check (child_count (n) = 3, "nested list: 3 children at top");
		inner := get_child (n, 2);
		check (kind (inner) = SEXP_LIST, "nested list: child 2 is a list");
		check (head (inner) = "b", "nested list: inner head = ""b""");
		check (child_count (inner) = 2, "nested list: inner has 2 children");
	end test_nested_list;


	----------------------------------------------------------------
	-- 3. Quoted atoms, including a KiCad-realistic property line
	----------------------------------------------------------------
	procedure test_quoted_atom is
		n : constant type_node := parse_string ("(a ""hello world"")");
		c : type_node;
	begin
		c := get_child (n, 2);
		check (atom_text (c) = "hello world", "quoted atom: text preserved with embedded space");
		check (atom_was_quoted (c), "quoted atom: quoted flag set");
	end test_quoted_atom;


	----------------------------------------------------------------
	-- 4. Escaped quote and backslash
	----------------------------------------------------------------
	procedure test_escaped_quote is
		n : constant type_node := parse_string ("(a ""she said \""hi\"""")");
		c : constant type_node := get_child (n, 2);
	begin
		check (atom_text (c) = "she said ""hi""", "escaped quote: decoded correctly");
	end test_escaped_quote;


	----------------------------------------------------------------
	-- 5. Escaped newline (\n as two literal characters -> one LF)
	----------------------------------------------------------------
	procedure test_escaped_newline is
		n : constant type_node := parse_string ("(a ""line1\nline2"")");
		c : constant type_node := get_child (n, 2);
		text : constant string := atom_text (c);
	begin
		check (text'length = 11, "escaped newline: decoded length is 11 (line1+LF+line2)");
		check (text (6) = ascii.lf, "escaped newline: character 6 is a real LF");
	end test_escaped_newline;


	----------------------------------------------------------------
	-- 6. Numeric / yes-no atom conversions
	----------------------------------------------------------------
	procedure test_atom_conversions is
		n : constant type_node := parse_string ("(at 12.7 -73.66 0)");
	begin
		check (abs (atom_to_real (get_child (n, 2)) - 12.7) < 0.0001, "atom_to_real: 12.7");
		check (abs (atom_to_real (get_child (n, 3)) - (-73.66)) < 0.0001, "atom_to_real: -73.66");
		check (atom_to_natural (get_child (n, 4)) = 0, "atom_to_natural: 0");

		declare
			yn : constant type_node := parse_string ("(in_bom yes)");
			yn2 : constant type_node := parse_string ("(in_bom no)");
		begin
			check (atom_to_yes_no (get_child (yn, 2)) = true, "atom_to_yes_no: yes -> true");
			check (atom_to_yes_no (get_child (yn2, 2)) = false, "atom_to_yes_no: no -> false");
		end;
	end test_atom_conversions;


	----------------------------------------------------------------
	-- 7. find_first_child / find_all_children
	----------------------------------------------------------------
	procedure test_find_children is
		n : constant type_node := parse_string
			("(symbol (property ""Reference"" ""U"") (property ""Value"" ""F00"") (pin ""1""))");
		props : constant pac_node_list.vector := find_all_children (n, "property");
		first_pin : constant type_node := find_first_child (n, "pin");
		missing : constant type_node := find_first_child (n, "nonexistent");
	begin
		check (head (n) = "symbol", "find_children: head = ""symbol""");
		check (natural (props.length) = 2, "find_all_children: 2 property blocks found");
		check (kind (first_pin) = SEXP_LIST, "find_first_child: pin found");
		check (kind (missing) = SEXP_NONE, "find_first_child: missing tag returns SEXP_NONE");
	end test_find_children;


	----------------------------------------------------------------
	-- 8. Malformed input
	----------------------------------------------------------------
	procedure raise_unbalanced is
		n : type_node;
		pragma unreferenced (n);
	begin
		n := parse_string ("(a (b c)");
	end raise_unbalanced;

	procedure raise_unterminated_string is
		n : type_node;
		pragma unreferenced (n);
	begin
		n := parse_string ("(a ""unterminated)");
	end raise_unterminated_string;

	procedure raise_trailing_garbage is
		n : type_node;
		pragma unreferenced (n);
	begin
		n := parse_string ("(a b) (c d)");
	end raise_trailing_garbage;


	----------------------------------------------------------------
	-- 9. parse_file against the real, small example pg_01.kicad_sch
	----------------------------------------------------------------
	example_dir : constant string :=
		"/home/jq/CodeBerg/R1000.DigitizingTheSchematics/Schematics/SEQ/";

	procedure test_parse_file_pg_01 is
		n : constant type_node := parse_file (example_dir & "pg_01.kicad_sch");
		title_block : type_node;
		title : type_node;
	begin
		check (kind (n) = SEXP_LIST, "pg_01: top-level is a list");
		check (head (n) = "kicad_sch", "pg_01: head = ""kicad_sch""");

		title_block := find_first_child (n, "title_block");
		check (kind (title_block) = SEXP_LIST, "pg_01: title_block found");

		title := find_first_child (title_block, "title");
		check (kind (title) = SEXP_LIST, "pg_01: title found inside title_block");
		check (atom_text (get_child (title, 2)) = "MICROSEQUENCER",
			"pg_01: title text = ""MICROSEQUENCER""");

		check (kind (find_first_child (n, "lib_symbols")) = SEXP_LIST,
			"pg_01: lib_symbols section present (even though empty)");
		check (natural (find_all_children (find_first_child (n, "lib_symbols"), "symbol").length) = 0,
			"pg_01: lib_symbols has no symbol entries (matches source)");
	end test_parse_file_pg_01;


	----------------------------------------------------------------
	-- 10. parse_file against the content-heavy pg_12.kicad_sch --
	--     cross-checked counts against "grep -c" on the raw file.
	----------------------------------------------------------------
	procedure test_parse_file_pg_12 is
		n : constant type_node := parse_file (example_dir & "pg_12.kicad_sch");
	begin
		check (kind (n) = SEXP_LIST, "pg_12: top-level is a list");

		check (natural (find_all_children (n, "junction").length) = 19,
			"pg_12: 19 junctions (grep -c '(junction (at')");

		check (natural (find_all_children (n, "no_connect").length) = 3,
			"pg_12: 3 no_connects");

		check (natural (find_all_children (n, "bus_entry").length) = 4,
			"pg_12: 4 bus_entries");

		check (natural (find_all_children (n, "symbol").length) = 34,
			"pg_12: 34 placed symbol instances (grep -c '(symbol (lib_id')");

		check (natural (find_all_children (find_first_child (n, "lib_symbols"), "symbol").length) = 10,
			"pg_12: 10 lib_symbols entries (grep -c symbol-quote)");
	end test_parse_file_pg_12;


begin
	put_line ("=== et_kicad_v6.sexp testbench ===");
	new_line;

	test_simple_list;
	test_nested_list;
	test_quoted_atom;
	test_escaped_quote;
	test_escaped_newline;
	test_atom_conversions;
	test_find_children;

	check_raises_syntax_error ("unbalanced parens raises sexp_syntax_error", raise_unbalanced'access);
	check_raises_syntax_error ("unterminated string raises sexp_syntax_error", raise_unterminated_string'access);
	check_raises_syntax_error ("trailing garbage raises sexp_syntax_error", raise_trailing_garbage'access);

	test_parse_file_pg_01;
	test_parse_file_pg_12;

	new_line;
	put_line ("=== " & natural'image (pass_count) & " passed, "
		& natural'image (fail_count) & " failed ===");

	if fail_count > 0 then
		set_exit_status (failure);
	end if;
end test;
