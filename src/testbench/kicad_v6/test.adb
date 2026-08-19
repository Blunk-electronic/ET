-- Standalone testbench for et_kicad_v6.sexp (the S-expression tokenizer/
-- parser). Build with "gprbuild -j2" from this directory, run with "./test".
--
-- Phase 1 gate per the project plan: literal snippets first, then
-- parse_file on the small real example pg_01.kicad_sch.

with ada.text_io;				use ada.text_io;
with ada.exceptions;			use ada.exceptions;
with ada.command_line;			use ada.command_line;
with ada.directories;

with et_kicad_v6;
with et_kicad_v6.sexp;			use et_kicad_v6.sexp;
with et_kicad_v6.schematic;	use et_kicad_v6.schematic;
with et_kicad_v6_to_native;
with et_module;					use et_module;
with et_device_appearance;		use et_device_appearance;
with et_devices_electrical;		use et_devices_electrical;
with et_nets;					use et_nets;
with et_net_names;
with et_net_scope;				use et_net_scope;
with ada.containers;			use ada.containers;
with et_logging;				use et_logging;
with et_project_name;			use et_project_name;
with et_mirroring;				use et_mirroring;

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


	----------------------------------------------------------------
	-- 11. et_kicad_v6.schematic.read_sheet_file against pg_12 --
	--     Phase 2 gate: counts cross-checked against "grep -c",
	--     plus a hand spot-check of one specific symbol/pin.
	----------------------------------------------------------------
	procedure test_read_sheet_file_pg_12 is
		s : constant type_sheet_data := read_sheet_file (example_dir & "pg_12.kicad_sch", 0);
	begin
		check (natural (s.wires.length) = 186, "read_sheet_file pg_12: 186 wires");
		check (natural (s.buses.length) = 6, "read_sheet_file pg_12: 6 buses");
		check (natural (s.bus_entries.length) = 4, "read_sheet_file pg_12: 4 bus_entries");
		check (natural (s.junctions.length) = 19, "read_sheet_file pg_12: 19 junctions");
		check (natural (s.no_connects.length) = 3, "read_sheet_file pg_12: 3 no_connects");
		check (natural (s.texts.length) = 1, "read_sheet_file pg_12: 1 free text");
		check (natural (s.placed_symbols.length) = 34, "read_sheet_file pg_12: 34 placed symbols");
		check (natural (s.symbols.length) = 10, "read_sheet_file pg_12: 10 lib_symbols entries");

		declare
			local_count, global_count, hier_count : natural := 0;
			lc : pac_labels.cursor := s.labels.first;
		begin
			while pac_labels.has_element (lc) loop
				case pac_labels.element (lc).label_kind is
					when LABEL_LOCAL => local_count := local_count + 1;
					when LABEL_GLOBAL => global_count := global_count + 1;
					when LABEL_HIERARCHICAL => hier_count := hier_count + 1;
				end case;
				pac_labels.next (lc);
			end loop;

			check (local_count = 19, "read_sheet_file pg_12: 19 local labels");
			check (global_count = 35, "read_sheet_file pg_12: 35 global labels");
			check (hier_count = 0, "read_sheet_file pg_12: 0 hierarchical labels");
		end;

		check (to_string (s.title) = "CS DISPLAY REGISTER CONTROL", "read_sheet_file pg_12: title matches");

		-- Spot-check the lib_symbol "r1000:F00" by hand against the
		-- source: unit 1 / convert 1 has exactly 3 pins (D0, D1, Q):
		declare
			f00_id  : constant type_lib_id := to_lib_id ("r1000:F00");
			f00_cur : constant pac_lib_symbols.cursor := s.symbols.find (f00_id);
		begin
			check (pac_lib_symbols.has_element (f00_cur), "read_sheet_file pg_12: lib_symbol r1000:F00 found");

			if pac_lib_symbols.has_element (f00_cur) then
				declare
					sub : constant type_symbol_sub_unit :=
						get_sub_unit (pac_lib_symbols.element (f00_cur), 1, 1);
				begin
					check (natural (sub.pins.length) = 3, "read_sheet_file pg_12: F00 unit1/convert1 has 3 pins");

					if natural (sub.pins.length) = 3 then
						declare
							p1 : constant type_pin := sub.pins.first_element;
						begin
							check (to_string (p1.name) = "D0", "read_sheet_file pg_12: F00 first pin name = D0");
							check (to_string (p1.number) = "1", "read_sheet_file pg_12: F00 first pin number = 1");
							check (p1.electrical_type = PIN_INPUT, "read_sheet_file pg_12: F00 first pin is PIN_INPUT");
						end;
					end if;
				end;
			end if;
		end;

		-- Strand connectivity sanity: every strand's point count
		-- should be >= 1, and the total number of strands should be
		-- well below the raw wire count (i.e. real merging happened,
		-- not just one strand per wire):
		declare
			total_strands : constant natural := natural (s.strands.length);
			sc : pac_strands.cursor := s.strands.first;
			min_points : natural := natural'last;
		begin
			while pac_strands.has_element (sc) loop
				if natural (pac_strands.element (sc).points.length) < min_points then
					min_points := natural (pac_strands.element (sc).points.length);
				end if;
				pac_strands.next (sc);
			end loop;

			check (total_strands > 0, "read_sheet_file pg_12: at least one strand built");
			check (total_strands < 186, "read_sheet_file pg_12: strand count well below raw wire count (merging happened)");
			check (min_points >= 1, "read_sheet_file pg_12: every strand has at least one point");

			put_line ("  INFO  pg_12: " & natural'image (total_strands) & " strands built from 186 wires + 6 buses");
		end;
	end test_read_sheet_file_pg_12;


	----------------------------------------------------------------
	-- 12. import_design against the full 87-sheet SEQ project --
	--     Phase 4 gate per the project plan: sheet-tree node count,
	--     total placed-symbol count, zero cycle errors, all pages
	--     resolved, mirror_y / convert_2 both parsed correctly
	--     somewhere in the project.
	----------------------------------------------------------------

	-- Recursively counts every node in the sheet tree:
	function count_nodes (node : in type_sheet_node_access) return natural is
		total : natural := 0;
	begin
		if node = null then
			return 0;
		end if;

		total := 1;

		for i in node.children.first_index .. node.children.last_index loop
			total := total + count_nodes (node.children (i));
		end loop;

		return total;
	end count_nodes;

	-- Recursively counts nodes whose page field is still empty
	-- (i.e. NOT resolved from the root's sheet_instances block):
	function count_unresolved_pages (node : in type_sheet_node_access) return natural is
		total : natural := 0;
	begin
		if node = null then
			return 0;
		end if;

		if to_string (node.page)'length = 0 then
			total := 1;
		end if;

		for i in node.children.first_index .. node.children.last_index loop
			total := total + count_unresolved_pages (node.children (i));
		end loop;

		return total;
	end count_unresolved_pages;


	procedure test_import_design_seq (proj : out type_project) is
		total_symbols	: natural := 0;
		found_mirror_y	: boolean := false;
		found_convert_2 : boolean := false;
	begin
		put_line ("  ....  import_design (this may take a few seconds) ...");

		proj := import_design (
			project				=> to_project_name ("SEQ"),
			project_directory	=> example_dir,
			log_threshold		=> 0);

		put_line ("  INFO  sheets loaded (file_cache): " & natural'image (natural (proj.file_cache.length)));
		put_line ("  INFO  merged_symbols: " & natural'image (natural (proj.merged_symbols.length)));
		put_line ("  INFO  merged_nets: " & natural'image (natural (proj.merged_nets.length)));

		check (proj.root /= null, "import_design: root node is not null");
		check (count_nodes (proj.root) = 87, "import_design: sheet-tree has 87 nodes (1 root + 86 children)");
		check (count_unresolved_pages (proj.root) = 0, "import_design: every node's page was resolved");

		-- Total placed-symbol count and mirror_y/convert_2 spot
		-- checks, scanning every loaded sheet's own placed_symbols:
		declare
			fc : pac_sheet_data_by_path.cursor := proj.file_cache.first;
		begin
			while pac_sheet_data_by_path.has_element (fc) loop
				declare
					data : constant type_sheet_data_access := pac_sheet_data_by_path.element (fc);
					sc	 : pac_placed_symbols.cursor := data.placed_symbols.first;
				begin
					while pac_placed_symbols.has_element (sc) loop
						declare
							sym : constant type_placed_symbol := pac_placed_symbols.element (sc);
						begin
							total_symbols := total_symbols + 1;

							if sym.mirror = MIRROR_ALONG_Y_AXIS then
								found_mirror_y := true;
							end if;

							if sym.convert = 2 then
								found_convert_2 := true;
							end if;
						end;

						pac_placed_symbols.next (sc);
					end loop;
				end;

				pac_sheet_data_by_path.next (fc);
			end loop;
		end;

		-- 1566 in the 86 leaf pg_*.kicad_sch files (grep -c '(lib_id "'
		-- pg_*.kicad_sch) + 4 more directly on the root SEQ.kicad_sch
		-- itself (easy to miss with a pg_*-only glob, as the first
		-- draft of this check did):
		check (total_symbols = 1570, "import_design: 1570 total placed symbol instances across all sheets");
		check (found_mirror_y, "import_design: at least one symbol parsed with mirror = MIRROR_ALONG_Y_AXIS");
		check (found_convert_2, "import_design: at least one symbol parsed with convert = 2");
	end test_import_design_seq;


	procedure test_convert_to_native (proj : in type_project) is
		module : type_generic_module;

		virtual_count, pcb_count, total_units : natural := 0;
		global_count, local_count : natural := 0;
	begin
		put_line ("  ....  et_kicad_v6_to_native.convert (this may take a few seconds) ...");

		module := et_kicad_v6_to_native.convert (project => proj, log_threshold => 0);

		put_line ("  INFO  native devices: " & count_type'image (module.devices.length));
		put_line ("  INFO  native nets: " & count_type'image (module.nets.length));

		check (natural (module.devices.length) > 0, "convert: at least one native device was created");
		check (natural (module.nets.length) > 0, "convert: at least one native net was created");

		declare
			dc : pac_devices_electrical.cursor := module.devices.first;
		begin
			while pac_devices_electrical.has_element (dc) loop
				declare
					dev : type_device_electrical renames pac_devices_electrical.element (dc);
				begin
					case dev.appearance is
						when APPEARANCE_VIRTUAL => virtual_count := virtual_count + 1;
						when APPEARANCE_PCB     => pcb_count     := pcb_count + 1;
					end case;

					total_units := total_units + natural (dev.units.length);
				end;

				pac_devices_electrical.next (dc);
			end loop;
		end;

		put_line ("  INFO  virtual devices: " & natural'image (virtual_count)
			& ", pcb devices: " & natural'image (pcb_count)
			& ", total units: " & natural'image (total_units));

		check (virtual_count > 0, "convert: at least one virtual (power-symbol) device was created");
		check (pcb_count > 0, "convert: at least one real (pcb) device was created");
		check (total_units >= natural (module.devices.length),
			"convert: every device has at least one unit");

		declare
			nc : et_nets.pac_nets.cursor := module.nets.first;
		begin
			while et_nets.pac_nets.has_element (nc) loop
				declare
					net : et_nets.type_net renames et_nets.pac_nets.element (nc);
				begin
					case net.scope is
						when GLOBAL => global_count := global_count + 1;
						when LOCAL  => local_count  := local_count + 1;
					end case;

					check (not net.strands.is_empty, "convert: net " & et_net_names.to_string (et_nets.pac_nets.key (nc))
						& " has at least one strand");
				end;

				et_nets.pac_nets.next (nc);
			end loop;
		end;

		put_line ("  INFO  global nets: " & natural'image (global_count)
			& ", local/anonymous nets: " & natural'image (local_count));

		check (global_count > 0, "convert: at least one GLOBAL-scope net was created");
	end test_convert_to_native;


	proj : type_project;

begin
	-- et_logging.create_report writes to "ET/reports/messages.log"
	-- relative to the current directory -- the example project
	-- already has that directory (part of its own file layout), so
	-- run from there:
	ada.directories.set_directory (example_dir);

	-- et_kicad_v6.schematic.import_design (and read_sheet_file)
	-- calls et_logging.log, which writes to a report file that must
	-- be opened first:
	create_report;
	log_level := 20; -- otherwise WARNING/NOTE text is counted but not printed

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
	test_read_sheet_file_pg_12;
	test_import_design_seq (proj);
	test_convert_to_native (proj);

	new_line;
	put_line ("=== " & natural'image (pass_count) & " passed, "
		& natural'image (fail_count) & " failed ===");

	close_report;

	if fail_count > 0 then
		set_exit_status (failure);
	end if;
end test;
