------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD COMPONENT LIBRARIES                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;
with ada.environment_variables;

with et_conventions;

package body et_kicad_libraries is

	function f (line : in type_fields_of_line; position : in positive) return string
		renames et_string_processing.field;
	
	function to_string (meaning : in type_placeholder_meaning) return string is begin
		return to_lower (type_placeholder_meaning'image (meaning));
	end;

	function to_meaning (meaning : in string) return type_placeholder_meaning is begin
		return type_placeholder_meaning'value (meaning);
	end;

	function content (text : in type_text_placeholder) return string is
	-- Returns the content of the given text placeholder as string.
		c : et_text.type_text_content.bounded_string;
	begin
		c := text.content;
		return et_text.to_string (c);
	end content;

	function to_string (style : in type_port_style) return string is begin
		return space & to_lower (type_port_style'image (style));
	end to_string;

	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string is
	begin
		if preamble then
			return " direction " & to_lower (type_port_direction'image (direction));
		else
			return space & to_lower (type_port_direction'image (direction));
		end if;
	end to_string;
	
	function to_string (fill : in type_fill) return string is begin
		return space & to_lower (type_fill_border'image (fill.border))
		& space & "pattern" & space 
		& to_lower (type_fill_pattern'image (fill.pattern));
	end to_string;
	
	procedure check_prefix_characters (
		prefix 		: in pac_device_prefix.bounded_string;
		characters	: in character_set) is
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use pac_device_prefix;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> prefix,
			set		=> characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (ERROR, "component prefix " & et_devices.to_string (prefix) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_prefix_characters;
	
	function to_component_reference (	
	-- Converts a string like "IC303" to a composite type_device_name.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character (like in #FLG01).
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the id are removed. R002 becomes R2.
		text_in			: in string;
		leading_hash	: in boolean := false
		) return type_device_name is
		
		use et_devices;

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := text_in;
	
		r : type_device_name := (
				prefix 		=> pac_device_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : pac_device_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (ERROR, latin_1.lf & "invalid component reference " & enclose_in_quotes (text_in_justified),
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use pac_device_prefix;
	begin
		-- assemble prefix
		for i in text_in_justified'first .. text_in_justified'last loop
			c := text_in_justified (i);
			
			case i is 
				-- The first character MUST be a valid prefix character.
				-- If allow_special_charater_in_prefix then the first letter is
				-- allowed to be a special character. (kicad uses '#' for power symbols)
				when 1 => 
					case leading_hash is
						when false =>
							if is_in (c, component_prefix_characters) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
							if is_in (c, component_prefix_characters) or is_special(c) then -- CS: test for et_kicad.schematic_component_power_symbol_prefix instead.
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;
					end case;
					
				-- Further characters are appended to prefix if they are valid prefix characters.
				-- If anything else is found, the prefix is assumed as complete.
				when others =>
					if is_in (c, component_prefix_characters) then
						r.prefix := r.prefix & c;
					else
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in_justified.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in_justified'last loop
			c := text_in_justified(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
-- 		put_line(" id    " & natural'image(r.id));
-- 		put_line(" digits" & natural'image(r.id_width));
		
		return r;
	end to_component_reference;
	
	function to_package_name (
		library_name	: in et_kicad_general.type_device_library_name.bounded_string; -- ../libraries/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string; -- TRANSISTOR_PNP
		package_variant	: in pac_package_variant_name.bounded_string) -- N, D
		return et_packages.type_component_package_name.bounded_string is
	-- Returns the package name for of the given component.
		package_name : et_packages.type_component_package_name.bounded_string; -- to be returned
	begin -- to_package_name
		-- CS
		return package_name;
	end to_package_name;

	function to_string (
		no_connection_flag	: in type_no_connection_flag;
		scope				: in et_kicad_coordinates.type_scope) return string is
	-- Returns the position of the given no-connection-flag as string.
	begin	
		return (to_string (position => no_connection_flag.coordinates, scope => scope));
	end to_string;

	function to_string (port : in type_port_with_reference) return string is
	-- Returns the properties of the given port as string.
	begin
		return "reference " & to_string (port.reference) 
			& " port " & et_symbols.to_string (port.name)
			& " coordinates " & to_string (position => port.coordinates, scope => module);
	end to_string;

	
	function compare_ports (left, right : in type_port_with_reference) return boolean is
	-- Returns true if left comes before right. Compares by component reference and port name.
	-- If left equals right, the return is false.	
	-- CS: needs verification !
		result : boolean := false;
		use et_symbols;
		use et_schematic;
	begin
		-- First we compare the component reference.
		-- Examples: C56 comes before R4, LED5 comes before LED7
		if left.reference < right.reference then
			result := true;

		-- If equal pin names, compare port names -- CS: should never happen. raise alarm ?
		elsif pac_port_name.">" (left.name, right.name) then
			result := true;
			
		else
			result := false;
		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_ports;

	procedure no_generic_model_found (
		reference		: in type_device_name; -- IC303
		library			: in et_kicad_general.type_device_library_name.bounded_string; -- ../lib/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string) -- TRANSISTOR_NPN
		is
		use et_string_processing;
	begin
		log (ERROR, "component " & to_string (reference) -- CS: output coordinates
			& " has no generic model " & to_string (generic_name)
			& " in library " & to_string (library), console => true);
		raise constraint_error;
	end no_generic_model_found;

	
	function component_appearance (cursor : in type_components_library.cursor)
	-- Returns the component appearance where cursor points to.
		return et_symbols.type_appearance is
	begin
		return type_components_library.element (cursor).appearance;
	end component_appearance;

	function first_unit (
	-- Returns the cursor to the first unit of the given component.
		component_cursor : in type_components_library.cursor)
		return type_units_library.cursor is

		unit_cursor : type_units_library.cursor;

		procedure locate (
			name		: in type_component_generic_name.bounded_string;
			component	: in type_component_library) is

			use type_units_library;
			use et_string_processing;
		begin
			-- Set the unit cursor to the first unit of the component.
			unit_cursor := type_units_library.first (component.units);

			-- In case the component has no units, abort.
			if unit_cursor = type_units_library.no_element then
				log (ERROR, "generic component " 
						& to_string (type_components_library.key (component_cursor)) 
						& " has no units !",
					console => true);
				raise constraint_error;
			end if;
		end locate;
	
	begin
		-- locate the component by the given component cursor
		type_components_library.query_element (component_cursor, locate'access);

		-- CS: do something if cursor invalid. via exception handler ?
		return unit_cursor;
	end first_unit;


	function first_port (
	-- Returns the cursor to the first port of the given unit
		unit_cursor : in type_units_library.cursor)
		return type_ports_library.cursor is

		port_cursor : type_ports_library.cursor; -- to be returned

		procedure locate (
			name : in et_devices.pac_unit_name.bounded_string;
			unit : in type_unit_library) is

			use type_ports_library;
			use et_string_processing;
		begin
			-- Set the port cursor to the first port of the unit.
			port_cursor := type_ports_library.first (unit.symbol.ports);

			-- In case the unit has no ports, abort.
			if port_cursor = type_ports_library.no_element then
				log (WARNING, "generic unit " 
						& et_devices.to_string (unit_name => type_units_library.key (unit_cursor)) 
						& " has no ports !");
					--console => true);
				--CS raise constraint_error;
			end if;
		end locate;

	begin
		type_units_library.query_element (unit_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return port_cursor;
	end first_port;


	procedure check_datasheet_length (datasheet : in string) is
	-- Tests if the given datasheet is longer than allowed.
		use et_string_processing;
	begin
		if datasheet'length > component_datasheet_length_max then
			log (ERROR, "max. number of characters for URL is" 
				 & positive'image (component_datasheet_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_datasheet_length;
	
	procedure check_datasheet_characters (
		datasheet : in type_component_datasheet.bounded_string;
		characters : in character_set := component_datasheet_characters) is
	-- Tests if the given URL contains only valid characters as specified
	-- by given character set. Issues warning if invalid character found.
		use et_string_processing;
		use type_component_datasheet;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => datasheet,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (WARNING, "URL to datasheet " & to_string (datasheet) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
		end if;
	end check_datasheet_characters;

	procedure invalid_field (line : in type_fields_of_line) is begin
		log (ERROR, affected_line (line) & "invalid field !", console => true);

		log (text => to_string (line), console => true);

		log (text => "Field indexes must be in range" 
			 & type_component_field_id'image (type_component_field_id'first)
			 & " .." 
			 & type_component_field_id'image (type_component_field_id'last)
			 & " !", 
			console => true);

		raise constraint_error;
	end invalid_field;

	procedure validate_prefix (prefix : in pac_device_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_string_processing;
		use pac_device_prefix;
	begin
		if et_devices.to_string (prefix) = power_flag_prefix 
			or et_devices.to_string (prefix) = power_symbol_prefix then
			null;
		else
			log (ERROR, "invalid prefix "
				 & et_devices.to_string (prefix) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;

	procedure validate_prefix (reference : in type_device_name) is
	-- Tests if the given reference has a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use pac_device_prefix;
	begin
		if et_devices.to_string (reference.prefix) = power_flag_prefix 
			or et_devices.to_string (reference.prefix) = power_symbol_prefix then
			null;
		else
			log (ERROR, "invalid prefix in component reference "
				 & et_devices.to_string (reference) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;
			
	function to_point (x_in, y_in : in string) return type_point is
		point : type_point;
		x, y : et_coordinates.type_distance_xy;

	begin
		x := mil_to_distance (x_in);
		y := mil_to_distance (y_in);

		--set_x (point, x);
		set (et_general.X, x, point);
		--set_y (point, y);
		set (et_general.Y, y, point);
		
		return point;
	end to_point;

	function library_name (text : in string) return et_kicad_general.type_library_name.bounded_string is
	-- extracts from a string like "bel_ic:S_SO14" the library name "bel_ic"
	begin
		return et_kicad_general.type_library_name.to_bounded_string (
			f (
				read_line (
					line 			=> text, 
					comment_mark	=> comment_mark,
					ifs				=> latin_1.colon
					),
				position => 1) -- the part before the colon
				);
	end library_name;

-- 	function to_string (dir : in type_library_directory.bounded_string) return string is
-- 	begin
-- 		return type_library_directory.to_string (dir);
-- 	end to_string;
	
	function package_name (text : in string) return et_packages.type_component_package_name.bounded_string is
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"
	begin
		return et_packages.type_component_package_name.to_bounded_string (
			f (
				read_line (
					line			=> text,
					comment_mark	=> comment_mark,
					ifs 			=> latin_1.colon
					),
				position => 2) -- the part after the colon
				);
	end package_name;

	function to_text_meaning (
	-- Extracts from a scheamtic field like "F 0 "#PWR01" H 2000 3050 50  0001 C CNN" its meaning.
	-- Extracts from a component field like "F0 "IC" 0 50 50 H V C CNN" its meaning.
	-- Since the fields start different in libaray and schematic we also need a flag that tells
	-- the function whether we are dealing with schematic or library fields.
		line 		: in type_fields_of_line;
		schematic	: in boolean) -- set false if it is about fields in a library, true if it is about a schematic field	
		return type_placeholder_meaning is

		meaning : type_placeholder_meaning := placeholder_meaning_default;

		function strip_f ( text : in string) return string is
		-- removes the heading character from the given string.
		begin return text(text'first+1..text'last); end strip_f;

		function strip_id ( text : in string) return string is
		-- removes the trailing id from the given string.
		begin return text(text'first..text'first); end strip_id;
	
	begin -- to_text_meaning
		case schematic is
			when true =>

				-- In a schematic the meaning of a text field is identified by "F 0 ...".

				-- So the first thing to do is test if the letter F at the begin of the line:
				if f (line,1) = component_field_identifier then

					-- Then we test the field id.
					-- The field id must be mapped to the actual field meaning:
					case type_component_field_id'value (f (line,2)) is -- "0.."
						when component_field_reference	=> meaning := NAME;
						when component_field_value		=> meaning := VALUE;
						when component_field_package	=> meaning := PACKGE;
						when component_field_datasheet	=> meaning := DATASHEET;
						when others => null;
					end case;

				else
					invalid_field (line);
				end if;
				
			when false =>

				-- In a library the meaning of a text field is identified by "F0 .. F9".
				
				-- So the first thing to do is test if the letter F at the begin of the line:
				if strip_id (f (line,1)) = component_field_identifier then
				
					case type_component_field_id'value (strip_f (f (line,1))) is
						when component_field_reference	=> meaning := NAME;
						when component_field_value		=> meaning := VALUE;
						when component_field_package	=> meaning := PACKGE;
						when component_field_datasheet	=> meaning := DATASHEET;
						when others => null;
					end case;

				else
					invalid_field (line);
				end if;

		end case;

		return meaning;

		exception
			when constraint_error =>
				invalid_field (line); -- CS: May display the affected line a second time in some cases.
				raise;
		
	end to_text_meaning;
								 
	function to_field_orientation (text : in string) return et_coordinates.type_rotation is
	-- Converts a kicad field text orientation character (H/V) to type_rotation.
	begin	
		case type_field_orientation'value (text) is
			when H => return 0.0;
			when V => return 90.0; -- CS -90.0 ?
		end case;

		exception 
			when constraint_error =>
				log (ERROR, "invalid text orientation !", console => true);
				raise;
			when others =>
				log (ERROR, "invalid text orientation !", console => true);
				raise;
	end to_field_orientation;
	
	function to_alignment_horizontal (text : in string) return et_text.type_text_alignment_horizontal is
	-- Converts a horizontal kicad text alignment to type_text_alignment_horizontal.
		use et_text;
		a : et_text.type_text_alignment_horizontal;
	begin
		case type_field_alignment_horizontal'value(text) is
			when L => a := LEFT;
			when C => a := CENTER;
			when R => a := RIGHT;
		end case;
		return a;
	end to_alignment_horizontal;

	function to_alignment_vertical (text : in string) return et_text.type_text_alignment_vertical is
	-- Converts a vertical kicad text alignment to type_text_alignment_vertical.
	-- The given text is something like CNN. We are interested in the first character only.
		use et_text;
		a : type_text_alignment_vertical;
		s : string (1..1) := text(text'first..text'first);
	begin
		case type_field_alignment_vertical'value(s) is
			when T => a := TOP;
			when C => a := CENTER;
			when B => a := BOTTOM;
		end case;
		return a;
	end to_alignment_vertical;

-- 	function to_text_style (
-- 	-- Converts a vertical kicad text style to type_text_style.
-- 	-- The given style_in is something like CNN or "Italic" (if it is about a text field or a simple text).
-- 	-- We are interested in the 2nd and 3rd character only.
-- 		style_in : in string;
-- 		text : in boolean -- true if it is about the style of a text, false if it is about the style of a field
-- 		-- Explanation: The style of a text is something like "~" or "Italic".
-- 		-- The style of a field comes with the letters 2 and 3 of a string like CNN.
-- 		) return type_text_style is
-- 		
-- 		a : type_text_style;
-- 		s_field : string (1..2);
-- 	
-- 		procedure invalid_style is
-- 		begin
-- 			log (ERROR, "invalid text style '" & style_in & "' !");
-- 			raise constraint_error;
-- 		end invalid_style;
-- 		
-- 	begin -- to_text_style
-- 		case text is
-- 			when true =>
-- 				if style_in = text_schematic_style_normal then
-- 					a := type_text_style'first;
-- 				elsif style_in = text_schematic_style_italic then
-- 					a := ITALIC;
-- 				else
-- 					invalid_style;
-- 				end if;
-- 				
-- 			when false =>
-- 				s_field := style_in (style_in'first + 1 .. style_in'last);
-- 				
-- 				if    s_field = field_style_default then 		a := type_text_style'first;
-- 				elsif s_field = field_style_bold then 			a := BOLD;
-- 				elsif s_field = field_style_italic then 		a := ITALIC;
-- 				elsif s_field = field_style_italic_bold then 	a := ITALIC_BOLD;
-- 				else
-- 					invalid_style;
-- 				end if;
-- 		end case;
-- 
-- 		return a;
-- 		
-- 		exception
-- 			when constraint_error =>
-- 				invalid_style;
-- 
-- 				return a; -- CS: never reached
-- 				
-- 	end to_text_style;
	
-- 	function to_field_visible ( 
-- 	-- Converts the kicad field visible flag to the type_text_visible.
-- 	-- The parameter "schematic" tells whether to convert a schematic or a component library field.
-- 		vis_in 		: in string; -- the string to be converted
-- 		schematic	: in boolean -- set false if it is about fields in a library, true if it is about a schematic field
-- 		-- Explanation: The visibility of fields in schematic is defined by something like "0001" or "0000".
-- 		-- In component libraries it is defined by characters like V or I.
-- 		)
-- 		return et_libraries.type_text_visible is
-- 		
-- 		v_in_lib : type_library_field_visible;
-- 		v_in_sch : type_schematic_field_visible;
-- 		v_out : et_libraries.type_text_visible;
-- 	begin
-- 		case schematic is
-- 			
-- 			when true =>
-- 				-- As the type_schematic_field_visible has letter V as workaround, we must 
-- 				-- prepend it here to vis_in before converting to a type_schematic_field_visible:
-- 				v_in_sch := type_schematic_field_visible'value (schematic_field_visibility_prefix & vis_in);
-- 				case v_in_sch is
-- 					when V0000 => v_out := et_libraries.yes; -- visible
-- 					when V0001 => v_out := et_libraries.no;  -- invisible
-- 				end case;
-- 
-- 			when false =>
-- 				v_in_lib := type_library_field_visible'value(vis_in);
-- 				case v_in_lib is
-- 					when V => v_out := et_libraries.yes;
-- 					when I => v_out := et_libraries.no;
-- 				end case;
-- 
-- 		end case;
-- 
-- 		return v_out;
-- 	end to_field_visible;

	function to_appearance (line : in type_fields_of_line; schematic : in boolean) 
	-- Converts the apperance flag to type_device_appearance.
	-- The parameter "schematic" specifies whether we are dealing with a schematic
	-- or a library component.
	-- The appearance (power symbol or normal) is defined in the component library by P/N
	-- example: DEF 74LS00 IC 0 30 Y Y 4 F N
	-- In a schematic it is defined by a hash sign:
	-- example: L P3V3 #PWR07
		return type_appearance is
		
		comp_app	: type_appearance;
		lca			: type_library_component_appearance;

		procedure invalid_appearance is
		begin
			log (ERROR, et_string_processing.affected_line (line) 
				 & "invalid visibility flag !", console => true);
			raise constraint_error;
		end invalid_appearance;	

	begin -- to_appearance
		case schematic is

			when true =>
				-- If it is about a schematic component we just test if the first
				-- character of the 3rd subfield is a hash sign.
				if f (line,3) (f (line,3)'first) 
					= schematic_component_power_symbol_prefix then
					comp_app := VIRTUAL;
				else
					comp_app := PCB;
				end if;
				
			when false =>
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value (f (line,10));

				-- Evaluate lca and set comp_app accordingly.
				case lca is
					when N =>
						comp_app := PCB;
					when P => 
						comp_app := VIRTUAL;
				end case;
		end case;
		
		return comp_app;

		exception 
			when constraint_error =>
				invalid_appearance;
				raise;
				
	end to_appearance;

	function to_alternative_representation (line : in type_fields_of_line; schematic : in boolean)
	-- Converts the kicad alternative (deMorgan) representation to the type_de_morgan_representation.
	-- In a schematic it is expressed in a line like "U 2 1 5992967A". The 3rd field is the deMorgan flag.
		return type_de_morgan_representation is

		rep_in : type_alternative_representation;
		rep_out : type_de_morgan_representation;
	begin
		rep_in := type_alternative_representation'value (f (line,3));

		case rep_in is
			when alternative_representation_yes =>
				rep_out := yes;

				-- We do not support alternative representations.
				log (ERROR, "alternative representation (DeMorgan) not supported !",
					 console => true);
				raise constraint_error;
				
			when alternative_representation_no =>
				rep_out := no;
		end case;
		
		return rep_out;

		exception
			when others => 
				log (ERROR, "invalid alternative representation flag !", console => true);
				raise;			
		
	end to_alternative_representation;

	function to_degrees (angle : in string) return et_coordinates.type_rotation is
	-- Converts a given angle as string to type_rotation.
		
		a_in  : type_angle; -- unit is tenth of degrees -3599 .. 3599

		-- For the conversion we need an intermediate real type
		type type_angle_real is digits 5 range -3599.0 .. 3599.0;
		-- CS: better type_angle_real is delta 0.1 range -3599.0 .. 3599.0; ?
		-- for type_angle_real'small use 0.1
		
		a_tmp : type_angle_real;
	begin
		-- Convert given string to et_kicad.type_angle. This implies a syntax and range check.
		a_in  := type_angle'value (angle); -- -3599 .. 3599

		-- Convert given angle to a real type
		a_tmp := type_angle_real (a_in); -- -3599.0 .. 3599.0

		-- convert given angle to et_coordinates.type_rotation.
		return et_coordinates.type_rotation (a_tmp / 10.0); -- -359.9 .. 359.9
		-- CS multiply by -1 ? If yes, remove - before all calls of this function.

		-- CS: exception handler
	end to_degrees;

	function to_power_flag (reference : in type_device_name) 
		return type_power_flag is
	-- If the given component reference is one that belongs to a "power flag" returns YES.
		use pac_device_prefix;
	begin
		--log (text => et_schematic.to_string (reference));
		if prefix (reference) = power_flag_prefix then
			--log (text => "power flag on");
			return YES;
		else
			--log (text => "power flag off");
			return NO;
		end if;
	end to_power_flag;

	procedure check_generic_name_characters (
	-- Checks if the given generic component name meets certain conventions.
		name		: in type_component_generic_name.bounded_string; -- TRANSISTOR_NPN
		characters	: in character_set) is

		use et_string_processing;
		invalid_character_position : natural := 0;

	begin
		-- Test given generic name and get position of possible invalid characters.
		invalid_character_position := index (
			source	=> name,
			set		=> characters,
			test	=> outside);

		-- CS: test if tilde is the first character of the generic name.
		-- This requires a special test that allows a tilde at ONLY this position.
				
		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (WARNING, "invalid character in generic component name '" 
				& to_string (name) & "' at position" & natural'image (invalid_character_position));
		end if;
	end check_generic_name_characters;

	function to_string (generic_name : in type_component_generic_name.bounded_string) return string is
	-- Returns the given generic name as as string.
	-- CS: provide a parameter that turns the pretext like "generic name" on/off
	begin
		--return ("generic name " & type_component_generic_name.to_string (name_in_library));
		return type_component_generic_name.to_string (generic_name);
	end to_string;
	
	function strip_tilde (generic_name : in type_component_generic_name.bounded_string) return
		type_component_generic_name.bounded_string is
	-- Removes a possible heading tilde character from a generic component name.
	-- example: ~TRANSISTOR_NPN becomes TRANSISTOR_NPN	
	-- The leading tilde marks a component whose value is set to "invisible".
		use et_import;
		use type_component_generic_name;
		length : type_component_generic_name.length_range;
	begin
		if element (generic_name, 1) = '~' then
			length := type_component_generic_name.length (generic_name);
			return type_component_generic_name.bounded_slice (generic_name, 2, length);
		else
			return generic_name;
		end if;
	end strip_tilde;

	function prepend_tilde (generic_name : in type_component_generic_name.bounded_string) return
		type_component_generic_name.bounded_string is
		use et_import;
		use type_component_generic_name;
	begin
		return '~' & generic_name;
	end prepend_tilde;

	procedure validate_component_package_name 
		(name : in et_packages.type_component_package_name.bounded_string) is
	-- Tests if the given component package name meets certain conventions.
		use et_packages.type_component_package_name;
		use et_string_processing;
		
		procedure no_package is
		begin
			log (ERROR, "no package associated !", 
				console => true);
			raise constraint_error;
		end no_package;
			
	begin -- validate_component_package_name
		if length (name) > 0 then
			et_packages.check_package_name_characters (name, component_package_name_characters);
		else
			no_package;
		end if;
	end validate_component_package_name;

	function full_library_name (
		library_name	: in type_library_name.bounded_string; -- bel_logic
		package_name 	: in et_packages.type_component_package_name.bounded_string; -- S_SO14
		log_threshold	: in et_string_processing.type_log_level)
		return type_package_library_name.bounded_string is
	-- Returns the full library name of the library that
	-- contains the given package library with the given package.
		
	-- V4:
	-- 	- Searches the library directories in the order given in search_list_project_lib_dirs.
	-- 	- The full library name is the result of a search operation:
	-- 	- The first directory and the first library that contains the package.
	-- 	- There can be many library directories to search in.

	-- V5:
	--	- Looks up the fp-lib-table for the first occurence of the given library name.
	--	- The entry in the fp-lib-table in turn provides the full library name (incl. path).
		
		lib : type_package_library_name.bounded_string; -- to be returned

		use et_import;
		use type_project_lib_dirs;
		use et_packages;
		use type_package_library_name;
		use ada.directories;

		-- V4:
		dir_cursor : type_project_lib_dirs.cursor := search_list_project_lib_dirs.first; -- CS access search_list_library_dirs in module instead
		lib_cursor : type_libraries.cursor;

		-- V5:
		use type_lib_table;
		fp_lib_table_cursor : type_lib_table.cursor := fp_lib_tables.first; -- CS access fp_lib_tables in module.fp_lib_tables instead

		use type_library_name;
		full_library_name : type_package_library_name.bounded_string;
		package_found : boolean := false;

		procedure search_package (
		-- Searches the library (indicated by lib_cursor) for the given package.
		-- Sets the flag package_found if the library contains the given package.
			lib_name	: in type_package_library_name.bounded_string;
			library		: in type_packages_library.map) is
		begin
			if type_packages_library.contains (
				container	=> library,
				key			=> package_name) then

				package_found := true;
			end if;
		end search_package;
	
	begin -- full_library_name
		log (text => "locating library '" & et_kicad_general.to_string (library_name) &
			"' containing package '" & to_string (package_name) & "' ...", level => log_threshold);
		log_indentation_up;

		case cad_format is
			when KICAD_V4 =>
				
				-- Loop in search_list_project_lib_dirs. Test if the given library
				-- exists in the directory indicated by dir_cursor..
				while dir_cursor /= type_project_lib_dirs.no_element loop

					-- Test if library exists. package_libraries hosts libraries by their full name.
					-- So the library to test is formed by the current directory name, the given library name
					-- and the package_library_directory_extension (*.pretty)
					full_library_name := et_packages.to_file_name (ada.directories.compose (
						containing_directory	=> to_string (element (dir_cursor)),
						name					=> et_kicad_general.to_string (library_name),
						extension				=> package_library_directory_extension (2..package_library_directory_extension'last))); 

					log (text => "searching in " & et_packages.to_string (full_library_name) & " ...", level => log_threshold + 1);
					
					lib_cursor := type_libraries.find (
						container	=> package_libraries,
						key			=> full_library_name);

					-- If library exists, lib_cursor will point to it. Then the library can be searched 
					-- for the given package.
					if type_libraries."/=" (lib_cursor, type_libraries.no_element) then

						-- search the library for the given package
						type_libraries.query_element (
							position	=> lib_cursor,
							process		=> search_package'access);

						-- The search ends as soon as the given package was found.
						if package_found then exit; end if;

					end if;
					
					next (dir_cursor);
				end loop;

			when KICAD_V5 =>

				-- Search for the given library_name in the fp-lib-tables.
				-- The first matching entry in the table provides the full library name (uri).
				-- Then search in that library for the given package_name. If the package is 
				-- not in the library, search for next matching entry in fp-lib-table ...
				while fp_lib_table_cursor /= type_lib_table.no_element loop

					-- On match, open the library (by its uri).
					if element (fp_lib_table_cursor).lib_name = library_name then

						full_library_name := et_packages.to_file_name (et_devices.to_string (element (fp_lib_table_cursor).lib_uri));

						log (text => "searching in " & et_packages.to_string (full_library_name) & " ...", level => log_threshold + 1);
						
						-- locate the library by full name (uri)
						lib_cursor := type_libraries.find (
									container	=> package_libraries,
									key			=> full_library_name);
						
						-- Search in the library for the given package
						type_libraries.query_element (
							position	=> lib_cursor,
							process		=> search_package'access);

						-- The search ends as soon as the given package was found.
						if package_found then exit; end if;

					end if;

					next (fp_lib_table_cursor); -- advance to next entry in fp-lib-table
				end loop;

				-- If the library could not be located anywhere, abort here:
				if length (full_library_name) = 0 then
					log (ERROR, "No library '" & et_kicad_general.to_string (library_name) 
						 & "' found ! Check local and global fp-lib-tables !", console => true);
					raise constraint_error;
				end if;

			when others =>
				raise constraint_error;
				
		end case;
				
		if package_found then
			log (text => " found !", level => log_threshold + 2);
		else
			log (ERROR, "package '" & to_string (package_name) &
				"' not found in any library named '" & et_kicad_general.to_string (library_name) & "' !", console => true);
			raise constraint_error;
		end if;

		log_indentation_down;
		
		return full_library_name;
	end full_library_name;

	function terminal_port_map_fits (
	-- Used when terminal_port_maps are to be used for packages.
	-- The given package is specified by the library name and package name.
	-- Returns true if the terminal_port_map fits on the given package.
		library_name		: in type_package_library_name.bounded_string;		-- ../lbr/bel_ic.pretty
		package_name 		: in et_packages.type_component_package_name.bounded_string;	-- S_SO14
		terminal_port_map	: in pac_terminal_port_map.map) 
		return boolean is

		use type_libraries;
		library_cursor : type_libraries.cursor;

		procedure validate_terminals (package_terminals : in et_terminals.type_terminals.map) is
		-- Test if the terminals of the terminal_port_map are also in the given package.
		-- Raises constraint_error if a terminal could not be found in the package.
			use et_terminals.type_terminals; -- the terminals of the package
			use pac_terminal_port_map;
		
			-- This cursor points to the terminal in the terminal_port_map
			terminal_cursor : pac_terminal_port_map.cursor; 

			-- For temporarily storage of a terminal name:
			terminal_name_in_map : et_terminals.type_terminal_name.bounded_string;
		begin -- validate_terminals
			-- Loop in terminal_port_map. Test each terminal whether it occurs
			-- in the package_terminals.
			terminal_cursor := terminal_port_map.first;
			while terminal_cursor /= pac_terminal_port_map.no_element loop
				terminal_name_in_map := key (terminal_cursor);

				if package_terminals.find (terminal_name_in_map) = et_terminals.type_terminals.no_element then
					log (ERROR, "package " & et_packages.to_string (packge => package_name)
						 & " does not have a terminal '" 
						 & et_terminals.to_string (terminal_name_in_map) & "' !", console => true);
					raise constraint_error;
				end if;
				
				next (terminal_cursor);
			end loop;
		end validate_terminals;
			
	
		procedure locate_package (
		-- Locates the package by package_name in the given package library.
			library_name	: in type_package_library_name.bounded_string;
			packages		: in type_packages_library.map) is
			package_cursor : type_packages_library.cursor;

			use type_packages_library;
			use et_terminals.type_terminals;
			use pac_terminal_port_map;
			terminals : et_devices.type_terminal_count;
		begin
			if is_empty (packages) then
				log (ERROR, "package library " & et_packages.to_string (library_name)
					 & " is empty !", console => true);
				raise constraint_error;
			else
				-- locate the package
				package_cursor := packages.find (package_name);
				if package_cursor = type_packages_library.no_element then
					log (ERROR, "package " & et_packages.to_string (packge => package_name)
						& " not found in library " & et_packages.to_string (library_name)
						& " !", console => true);
					raise constraint_error;
				else
					-- load the total number of terminals the package provides
					terminals := et_devices.type_terminal_count (length (element (package_cursor).terminals));

					-- If the package has less terminals than the given terminal_port_map abort:
					if et_devices."<" (terminals, et_devices.type_terminal_count (length (terminal_port_map))) then
						log (ERROR, "package " & et_packages.to_string (packge => package_name)
							& " as too few terminals !",
							console => true);
						raise constraint_error;
					else
						validate_terminals (element (package_cursor).terminals);
					end if;
					
				end if;

			end if;
			
		end locate_package;
		
	begin -- terminal_port_map_fits
		if not is_empty (package_libraries) then
			library_cursor := package_libraries.find (library_name);

			if library_cursor = type_libraries.no_element then
				log (ERROR, "package library " & et_packages.to_string (library_name)
					 --& " not found in " & et_libraries.to_string (et_libraries.library_group)
					 & " not found"
					 & " !", console => true);
				raise constraint_error;
			else
				-- locate the given package (by package_name) in the given package library:
				query_element (
					position	=> library_cursor,
					process		=> locate_package'access);
			end if;
				
		else
			log (ERROR, "no package libraries available !", console => true);
			raise constraint_error;
		end if;

		return true;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_message (event), console => true);
					raise;

	end terminal_port_map_fits;

	
	procedure read_components_libraries (log_threshold : in type_log_level) is
	-- Reads component libraries.

		use et_packages;
		use et_devices;
		use type_full_library_names;

		-- This is the library cursor. It points to the library being processed (in the list tmp_component_libraries):
		lib_cursor		: type_device_libraries.cursor;

		-- This is the component cursor. It points to the component being processed.
		comp_cursor		: type_components_library.cursor;
		comp_inserted	: boolean; -- indicates whether a component has been inserted

		-- This is the unit cursor. It points to the unit being processed.
		unit_cursor		: type_units_library.cursor;
		unit_inserted	: boolean; -- indicates whether a unit has been inserted

		procedure read_library (log_threshold : in type_log_level) is
			line : type_fields_of_line; -- the line being processed

			-- This flag goes true once a component section is entered. It is cleared
			-- when the component section is left.
			component_entered : boolean := false; 

			-- The subsection of a component is indicated by variable active_section:			
			type type_active_section is (NONE, FIELDS, FOOTPRINTS, DRAW);
			active_section : type_active_section := NONE; 

			-- This flag is used when ports are added to an extra unit (supply symbols).
			-- It is initialzed by procedure init_temp_variables on entering a component section.
			extra_unit_available: boolean; 
			
			-- These are variables used to temporarily hold component properties before the component
			-- gets fully assembled and inserted into the component list of a particular library.
			-- These properties apply for the whole component (means for all its units):
			tmp_component_name		: type_component_generic_name.bounded_string; -- 74LS00 -- CS: rename to generic_name
			tmp_prefix				: pac_device_prefix.bounded_string; -- IC -- CS: rename to prefix
			tmp_appearance			: type_appearance; -- CS: rename to appearance

			tmp_port_name_visible		: type_port_name_visible;
			tmp_terminal_name_visible	: type_terminal_name_visible;
			tmp_port_name_offset		: et_coordinates.type_distance; -- CS: rename to port_name_offset
			tmp_terminal_name			: et_terminals.type_terminal_name.bounded_string;
			
			tmp_units_total		: type_units_total; -- see spec for range -- CS rename to units_total	
			tmp_unit_id			: type_unit_id; -- assumes 0 if all units are affected, -- see spec	-- CS rename to unit_id

			tmp_unit_swap_level	: type_swap_level := swap_level_default; -- CS: rename to unit_swap_level
			tmp_unit_add_level	: type_add_level := type_add_level'first; -- CS: rename to unit_add_level
			tmp_unit_global		: boolean := false; -- specifies if a unit harbors component wide pins (such as power supply) -- CS: rename to unit_global
			
			field_reference		: type_text_placeholder (meaning => NAME); -- CS: should be field_prefix as it contains just the prefix 
			field_value			: type_text_placeholder (meaning => VALUE);
			field_package		: type_text_placeholder (meaning => PACKGE);
			field_datasheet		: type_text_placeholder (meaning => DATASHEET);

			-- "field found flags" go true once the corresponding field was detected
			-- Evaluated by procedure check_text_fields.
			field_prefix_found			: boolean := false;
			field_value_found			: boolean := false;
			field_package_found			: boolean := false;
			field_datasheet_found		: boolean := false;
			
			-- temporarily used variables to store draw elements (polylines, arcs, pins, ...) 
			-- before they are added to a unit.
			tmp_draw_polyline	: type_symbol_polyline;
			tmp_draw_rectangle	: type_symbol_rectangle;
			tmp_draw_arc		: type_symbol_arc;
			tmp_draw_circle 	: type_symbol_circle;
			tmp_draw_text		: et_symbols.type_text;
			tmp_draw_port		: type_port_library;

			-- The terminal-port map of the current component is stored here temporarily.
			-- When building the package variant (there will be only the default variant)
			-- this map is used by procedure build_package_variant.
			tmp_terminal_port_map : pac_terminal_port_map.map;
			
			procedure init_temp_variables is
			-- Resets "field found flags".
			begin
				-- CS: init other variables
				extra_unit_available := false;
				tmp_unit_add_level := type_add_level'first;
				tmp_unit_global := false;

				field_prefix_found			:= false;
				field_value_found			:= false;
				field_package_found			:= false;
				field_datasheet_found		:= false;

				-- clear terminal-port map for the new component
				pac_terminal_port_map.clear (tmp_terminal_port_map);
			end init_temp_variables;

			function to_swap_level (swap_in : in string)
			-- Converts the kicad interchangeable flag to the et swap level.
			-- Since Kicad has only one swap level (interchangeable yes or no) 
			-- we convert to the lowest swap level available.
			-- Used when reading component libraries.	
				return type_swap_level is
				
				i : type_symbol_interchangeable;
				s : type_swap_level;
				--log_threshold : type_log_level := 2;
			begin
				log_indentation_up;
				
				log (text => "units interchangeable", level => log_threshold + 2);
				log_indentation_up;

				i := type_symbol_interchangeable'value(swap_in);
				
				case i is
					when L =>
						log (text => "no", level => log_threshold + 2);
						s := 0; -- no swapping allowed
					when F =>
						log (text => "yes", level => log_threshold + 2);
						s := 1; -- swapping allowed at this level
				end case;

				log_indentation_down;
				log_indentation_down;
				return s;
			end to_swap_level;

			function to_pin_visibile (vis_in : in string)
			-- Converts the kicad "show pin number" flag to the et type_terminal_name_visible.
			-- Used when reading component libraries.		
				return type_terminal_name_visible is

				v_in	: type_show_pin_number;
				v_out	: type_terminal_name_visible;
			begin
				log_indentation_up;
				
				log (text => "pin/pad names", level => log_threshold + 2);
				log_indentation_up;
				
				v_in := type_show_pin_number'value (vis_in);
				
				case v_in is 
					when Y => 
						log (text => "visible", level => log_threshold + 2);
						v_out := YES;
						
					when N => 
						log (text => "invisible", level => log_threshold + 2);
						v_out := NO;
				end case;

				log_indentation_down;
				log_indentation_down;
				return v_out;
				
			end to_pin_visibile;

			function to_port_visibile (vis_in : in string)
			-- Converts the kicad "show pin name" flag to the et type_port_name_visible.
			-- Used when reading component libraries.		
				return type_port_name_visible is

				v_in	: type_show_pin_name;
				v_out	: type_port_name_visible;
			begin
				log_indentation_up;
				
				log (text => "port names", level => log_threshold + 2);
				log_indentation_up;
				
				v_in := type_show_pin_name'value (vis_in);
				
				case v_in is 
					when Y => 
						log (text => "visible", level => log_threshold + 2);
						v_out := YES;
					when N => 
						log (text => "invisible", level => log_threshold + 2);
						v_out := NO;
				end case;

				log_indentation_down;
				log_indentation_down;

				return v_out;
			end to_port_visibile;

			function to_unit_name (id : in type_unit_id) return pac_unit_name.bounded_string is
			-- returns the given unit id as pac_unit_name
			begin
				return pac_unit_name.to_bounded_string (trim (type_unit_id'image (id), left));
			end to_unit_name;

			function to_fill (fill_style : in string) return type_fill is
			-- Converts the given kicad fill style to a type_fill.
			begin
				if fill_style = library_fill_none then
					return (pattern => none, border => invisible);
					
				elsif fill_style = library_fill_foreground then
					return (pattern => solid, border => invisible);
					
				elsif fill_style = library_fill_background then
					return (pattern => solid, border => visible);

				else
					raise constraint_error; -- CS write an error message about invalid fill style
				end if;
			end to_fill;
			
			function to_polyline (line : in et_string_processing.type_fields_of_line) return type_symbol_polyline is
			-- Returns from the given fields of a line a type_polyline.
				polyline	: type_symbol_polyline;
				total		: positive; -- for cross checking 

				-- A polyline is defined by a string like "P 3 0 1 10 0 0 100 50 70 0 N"
				-- field meaning:
				--  #2 : number of bends (incl. start and end points) (3)
				--  #3 : 0 -> common to all units, otherwise unit id it belongs to
				--  #4 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #5 : line width
				--  #6.. 7  : start point (x/y) (0/0) 
				--  #8.. 9  : bend point (x/y) (0/100)
				-- #10..11  : end point (x/y) (50/70)
				-- last field : fill style N/F/f no fill/foreground/background
			
				-- we start processing the fields from here (where the total number of points is)
				pos 		: positive := 2; 

				-- the x position of the last point of the line is here (field #10 in example above)
				end_point	: positive := positive (et_string_processing.field_count (line)) - 2;

				-- temporarily we store coordinates of a point here
				point		: type_point;

			begin -- to_polyline

				-- read total number of points
				total := positive'value (f (line, pos));
				
				-- read line width (field #5)
				pos := 5;

				-- If line width is too small, use a lower limit instead.
				if mil_to_distance (f (line, pos)) < type_line_width'first then
					polyline.width := type_line_width'first;
				else
					polyline.width := mil_to_distance (f (line, pos));
				end if;

				-- From the next field (#6) on, we find the coordinates of the 
				-- start point, the bend point(s) and the end point:
				pos := 6;
				loop exit when pos > end_point;
					--set_x (point, mil_to_distance (mil => f (line, pos))); -- set x
					set (X, mil_to_distance (mil => f (line, pos)), point); -- set x
				
					--set_y (point, mil_to_distance (mil => f (line, pos+1))); -- set y (right after the x-field)
					set (Y, mil_to_distance (mil => f (line, pos + 1)), point); -- set y (right after the x-field)

					-- For some unknown reason, kicad saves the y position of library objects inverted.
					-- It is probably a bug. However, when importing objects we must invert y. 
					mirror (point => point, axis => x);
					
					polyline.points.append (point); -- append this point to the list of points
					pos := pos + 2; -- advance field pointer to x coordinate of next point
				end loop;

				-- read fill style from last field
				polyline.fill := to_fill (f (line, pos));				
				
				-- CS: log properties
				
				return polyline;
			end to_polyline;

			function to_rectangle (line : in et_string_processing.type_fields_of_line) return type_symbol_rectangle is
			-- Returns from the given fields of a line a type_rectangle.
				rectangle	: type_symbol_rectangle;

				-- A rectangle is defined by a string like "S -40 -100 40 100 0 1 10 N"
				-- field meaning;
				-- #2..5 : diagonal with start point at -40/-100 and end point at 40/100
				-- #6 : 0 -> common to all units, otherwise unit id it belongs to
				-- #7 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				-- #8 : line width
				-- #9 : fill style N/F/f no fill/foreground/background

			begin -- to_rectangle
				--set_x (rectangle.corner_A, mil_to_distance (mil => f (line,2)));
				set (X, mil_to_distance (mil => f (line,2)), rectangle.corner_A);
				
				--set_y (rectangle.corner_A, mil_to_distance (mil => f (line,3)));
				set (Y, mil_to_distance (mil => f (line,3)), rectangle.corner_A);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.corner_A, axis => x);
				
				--set_x (rectangle.corner_B, mil_to_distance (mil => f (line,4)));
				set (X, mil_to_distance (mil => f (line,4)), rectangle.corner_B);

				--set_y (rectangle.corner_B, mil_to_distance (mil => f (line,5)));
				set (Y, mil_to_distance (mil => f (line,5)), rectangle.corner_B);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.corner_B, axis => x);

				-- If line width is too small, use a lower limit instead.
				if mil_to_distance (f (line, 8)) < type_line_width'first then
					rectangle.width := type_line_width'first;
				else
					rectangle.width := mil_to_distance (f (line, 8));
				end if;
				
				rectangle.fill := to_fill (f (line,9));

				-- CS: log properties
				
				return rectangle;
			end to_rectangle;

			function to_circle (line : in et_string_processing.type_fields_of_line) return type_symbol_circle is
			-- Returns from the given fields of a circle a type_circle.
				circle	: type_symbol_circle;

				-- A circle is defined by a string like "C 0 0 112 0 1 23 N"
				-- field meaning:
				--  #2..3 : center (x/y)
				--  #4 : radius
				--  #5 : 0 -> common to all units, otherwise unit id it belongs to
				--  #6 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #7 : line width (23)
				--  #8 : fill style N/F/f no fill/foreground/background

			begin -- to_circle
				--set_x (circle.center, mil_to_distance (mil => f (line,2)));
				set (X, mil_to_distance (mil => f (line,2)), circle.center);
				
				--set_y (circle.center, mil_to_distance (mil => f (line,3)));
				set (Y, mil_to_distance (mil => f (line,3)), circle.center);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => circle.center, axis => x);
	
				circle.radius	:= mil_to_distance (mil => f (line,4));

				-- If line width is too small, use a lower limit instead.
				if mil_to_distance (f (line,7)) < type_line_width'first then
					circle.width := type_line_width'first;
				else
					circle.width := mil_to_distance (f (line,7));
				end if;
				
				circle.fill		:= to_fill (f (line,8));

				-- CS: log properties
				
				return circle;
			end to_circle;

			function to_arc (line : in et_string_processing.type_fields_of_line) return type_symbol_arc is
			-- Returns from the given fields of an arc a type_arc.
				arc		: type_symbol_arc;

				-- An arc is defined by a string like "A 150 0 150 1800 900 0 1 33 N 0 0 150 150"
				-- NOTE: kicad bug: multiply all y values by -1
				-- field meaning:
				--  #2..3 : center (x/y) 
				--  #4 : radius (150)
				--  #5 : start angle in tenth of degrees (1800)
				--  #6 : end angle in tenth of degrees (900)
				--  #7 : 0 -> common to all units, otherwise unit id it belongs to
				--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #9 : line width 33
				-- #10 : fill style N/F/f no fill/foreground/background
				-- #11..12 : start point (x/y)
				-- #13..14 : end point (x/y)

			begin -- to_arc
				--set_x (arc.center, mil_to_distance (mil => f (line,2)));
				set (X, mil_to_distance (mil => f (line,2)), arc.center);
				
				--set_y (arc.center, mil_to_distance (mil => f (line,3)));
				set (Y, mil_to_distance (mil => f (line,3)), arc.center);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.center, axis => x);

				arc.radius		:= mil_to_distance (mil => f (line,4));

				arc.start_angle	:= to_degrees (f (line,5)); -- CS multiply by -1 ?
				arc.end_angle	:= to_degrees (f (line,6)); -- CS multiply by -1 ?
				--arc.direction	:= to_direction (arc);
				if arc.start_angle > arc.end_angle then
					arc.direction := CCW;
				else
					arc.direction := CW;
				end if;
				
				-- If line width is too small, use a lower limit instead.
				if mil_to_distance (f (line,9)) < type_line_width'first then
					arc.width := type_line_width'first;
				else
					arc.width := mil_to_distance (f (line,7));
				end if;

				arc.fill		:= to_fill (f (line,10));
				
				--set_x (arc.start_point, mil_to_distance (mil => f (line,11)));
				set (X, mil_to_distance (mil => f (line,11)), arc.start_point);
				
				--set_y (arc.start_point, mil_to_distance (mil => f (line,12)));
				set (Y, mil_to_distance (mil => f (line,12)), arc.start_point);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.start_point, axis => x);

				--set_x (arc.end_point, mil_to_distance (mil => f (line,13)));
				set (X, mil_to_distance (mil => f (line,13)), arc.end_point);
				
				--set_y (arc.end_point, mil_to_distance (mil => f (line,14)));
				set (Y, mil_to_distance (mil => f (line,14)), arc.end_point);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.end_point, axis => x);
				
				-- CS: log properties
				return arc;
			end to_arc;
	
			function to_text (line : in et_string_processing.type_fields_of_line) return et_symbols.type_text is
			-- Returns from the given fields of a text a type_symbol_text.
				text : et_symbols.type_text;

				-- A text is defined by a string like "T 0 0 300 60 0 0 0 leuchtdiode Normal 0 C C"
				-- Space characters whitin the actual text are replaced by tilde as in this example:
				-- "T 900 -100 0 60 0 1 0 gate~C Normal 0 C C"
				-- field meaning:
				--  #2 : rotation in tenth of degrees (counter clock wise), assumes only values of 0 or 900
				--  #3..4 : center (x/y)
				--  #5 : size 
				--  #6 : ? - unknown CS
				--  #7 : 0 -> common to all units, otherwise unit id it belongs to
				--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				--  #9 : content "leuchtdiode~square"
				-- #10 : style Nomal/Italic
				-- #11 : bold on/off (0/1) 
				-- #12 : horizontal alignment left/center/right L/C/R
				-- #13 : vertical alignment top/center/bottom T/C/B

-- 				function to_style ( style_in : in string; bold_in : in string) return type_text_style is
-- 				-- Composes from style_in and bold_in a type_text_style
-- 					a : type_text_style;
-- 
-- 					procedure invalid_style is begin
-- 						log (ERROR, "invalid text style '" & style_in & "' !");
-- 						raise constraint_error;
-- 					end invalid_style;
-- 
-- 				begin -- to_style
-- 					if bold_in = library_text_bold_off then -- "0" -- bold disabled
-- 						
-- 						if style_in = text_library_style_normal then
-- 							a := normal;
-- 						elsif style_in = text_library_style_italic then
-- 							a := italic;
-- 						else
-- 							invalid_style;
-- 						end if;
-- 
-- 					elsif bold_in = library_text_bold_on then -- "1" -- bold enabled
-- 
-- 						if style_in = text_library_style_normal then
-- 							a := bold;
-- 						elsif style_in = text_library_style_italic then
-- 							a := italic_bold;
-- 						else
-- 							invalid_style;
-- 						end if;
-- 
-- 					else -- "bold" flag invalid
-- 						raise constraint_error; -- CS : write message on invaid "bold" flag
-- 					end if;
-- 
-- 					return a;
-- 				end to_style;

				function to_content (text_in : in string) return et_text.type_text_content.bounded_string is
				-- Replaces tildss in given string by space and returns a type_text_content.bounded_string.
					t : string (1..text_in'length) := text_in; -- copy given text to t
				begin
					-- replace tildes in given text by spaces.
					translate (t, et_string_processing.tilde_to_space'access);
					return et_text.type_text_content.to_bounded_string (t);
				end to_content;

			begin -- to_text
				text.rotation := snap (- to_degrees (f (line,2)));
				
				--set_x (text.position, mil_to_distance (mil => f (line,3)));
				set (X, mil_to_distance (mil => f (line,3)), text.position);
				
				--set_y (text.position, mil_to_distance (mil => f (line,4)));
				set (Y, mil_to_distance (mil => f (line,4)), text.position);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => text.position, axis => x);

				text.size := mil_to_distance (mil => f (line,5));

				-- compose from fields 10 and 11 the text style
				--text.style := to_style (f (line,10), f (line,11));

				-- compose alignment
				text.alignment.horizontal	:= to_alignment_horizontal (f (line,12));
				text.alignment.vertical		:= to_alignment_vertical (f (line,13));

				-- read text content and replace tildes by spaces
				text.content := to_content (f (line,9));

				-- CS: log properties
				return text;
			end to_text;

			function to_port (line : in et_string_processing.type_fields_of_line) return type_port_library is
			-- Converts the given line to a type_port.
			
				port : type_port_library; -- the port being built -> to be returned

				-- A port is defined by a string like "X ~ 1 0 150 52 D 51 50 1 1 P"
				-- field meaning:
				--  #2 : port name (~)
				--  #3 : terminal name (1)
				--  #4..5 : position x/y (0/150)
				--  #6 : port length (52)
				--  #7 : orientation up/down/left/right (U/D/L/R)
				--  #8 : terminal name size (51)
				--  #9 : port name size (50)
				-- #10 : 0 -> common to all units, otherwise unit id it belongs to
				-- #11 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
				-- #12 : electrical type (direction), see et_kicad.ads for more
				-- #13 : optional field: pin style, see et_kicad.ads for more

				function to_direction (dir : in string) return type_port_direction is
					d_in : character := dir (dir'first);
					d_out : type_port_direction;
				begin
					case d_in is
						when library_pin_electrical_type_passive => 
							d_out := PASSIVE;

						when library_pin_electrical_type_input => 
							d_out := INPUT;

						when library_pin_electrical_type_output => 
							d_out := OUTPUT;

						when library_pin_electrical_type_bidir => 
							d_out := BIDIR;

						when library_pin_electrical_type_tristate => 
							d_out := TRISTATE;

						when library_pin_electrical_type_unspecified => 
							d_out := UNKNOWN;

						when library_pin_electrical_type_power_in => 
							d_out := POWER_IN;

						when library_pin_electrical_type_power_out => 
							d_out := POWER_OUT;

						when library_pin_electrical_type_open_collector => 
							d_out := WEAK1;

						when library_pin_electrical_type_open_emitter => 
							d_out := WEAK0;

						when library_pin_electrical_type_not_connected => 
							d_out := NOT_CONNECTED;

						when others =>
							-- CS: write message
							raise constraint_error;
					end case;

					return d_out;
				end to_direction;

				function to_style (style : in string) return type_port_style is
					s_in  : type_library_pin_graphical_style;
					s_out : type_port_style := type_port_style'first;
				begin
					-- convert given string to type_library_pin_graphical_style.
					-- This is an indirect test whether the given style is allowed.
					s_in := type_library_pin_graphical_style'value (style);

					-- map the given style to et.type_port_style
					case s_in is
						when N		=> s_out :=	type_port_style'first; -- default
						when I		=> s_out :=	INVERTED;
						when C		=> s_out :=	CLOCK;
						when IC		=> s_out :=	INVERTED_CLOCK;
						when L		=> s_out :=	INPUT_LOW;
						when CL		=> s_out :=	CLOCK_LOW;
						when V		=> s_out :=	OUTPUT_LOW;
						when F		=> s_out :=	FALLING_EDGE_CLK;
						when X		=> s_out :=	NON_LOGIC;
						when NI		=> s_out :=	INVISIBLE_INVERTED;
						when NC		=> s_out :=	INVISIBLE_CLOCK;
						when NIC	=> s_out :=	INVISIBLE_INVERTED_CLOCK;
						when NL		=> s_out :=	INVISIBLE_INPUT_LOW;
						when NCL	=> s_out :=	INVISIBLE_CLOCK_LOW;
						when NV		=> s_out :=	INVISIBLE_OUTPUT_LOW;
						when NF		=> s_out :=	INVISIBLE_FALLING_EDGE_CLK;
						when NX		=> s_out :=	INVISIBLE_NON_LOGIC;
					end case;
					
					return s_out;
					-- CS: exception handler
				end to_style;

				-- Translates orientation up/down/left/right (U/D/L/R) to rotation:
				function to_rotation (orientation : in string) 
					return et_coordinates.type_rotation_relative 
				is
					orient : constant character := orientation (orientation'first);
					rot : et_coordinates.type_rotation_relative := 0.0;
				begin
					case orient is
						when 'D' => rot :=  90.0; -- to be connected with a net from above,
						when 'U' => rot := -90.0; -- from below,
						when 'R' => rot := 180.0; -- from the left,
						when 'L' => rot :=   0.0; -- from the right
						when others => 
							log (ERROR, "invalid port orientation !", console => true);
							raise constraint_error;
					end case;
					return rot;
				end to_rotation;

				use et_conventions;
				
			begin -- to_port
				log_indentation_up;

				-- port name. to be taken from field #2 of the given line
				port.name := pac_port_name.to_bounded_string (f (line,2)); -- GND, GPIO2
				
				-- compose terminal name. must be stored temporarily. will be inserted in default package variant
				tmp_terminal_name := et_terminals.type_terminal_name.to_bounded_string (f (line,3)); -- H5, 14

				-- compose position
				set (X, mil_to_distance (mil => f (line,4)), port.position);
				set (Y, mil_to_distance (mil => f (line,5)), port.position);
				--mirror (point => port.position, axis => x);

				-- compose length
				port.length := mil_to_distance (mil => f (line,6)); -- CS port length may assume zero. do something !

				-- compose rotation
				port.rotation := to_rotation (f (line,7));

				-- port and termnal name text size (set to lower limit if too small)
				if mil_to_distance (mil => f (line,8)) < et_symbols.pac_text.type_text_size'first then
					port.terminal_name_size := et_symbols.pac_text.type_text_size'first;
				else
					port.terminal_name_size := mil_to_distance (mil => f (line,8));
				end if;
				check_schematic_text_size (category => TERMINAL_NAME, size => port.terminal_name_size);

				if mil_to_distance (mil => f (line,9)) < et_symbols.pac_text.type_text_size'first then
					port.port_name_size	:= et_symbols.pac_text.type_text_size'first;
				else
					port.port_name_size	:= mil_to_distance (mil => f (line,9));
				end if;
				check_schematic_text_size (category => PORT_NAME, size => port.port_name_size);

				-- direction
				port.direction := to_direction (f (line,12));

				-- port style (optional, to be composed if field #13 present)
				if field_count (line) = 13 then
					port.style := to_style (f (line,13));
				end if;

				-- visibility port and pin names
				port.port_name_visible := tmp_port_name_visible;
				port.terminal_name_visible := tmp_terminal_name_visible;

				-- port name offset
				port.port_name_offset := tmp_port_name_offset;

				--log (text => text => et_coordinates.to_string (point => port.coordinates), level => 1);

				-- CS: log other port properties

				log_indentation_down;
				return port;

				-- CS: exception handler
			end to_port;

					
			function to_field (
				line 	: in type_fields_of_line;
				meaning	: in type_placeholder_meaning) 
				return type_text_placeholder is
			-- Reads general text field properties from subfields 3..9 and returns a type_text with 
			-- the meaning as given in parameter "meaning".
			-- Checks basic properties of text fields (allowed charactes, text size, aligment, ...)
			-- NOTE: The contextual validation takes place in procedure check_text_fields.
				use et_text;
				use et_text.type_text_content;

				-- instantiate a text field as speficied by given parameter meaning
				text : type_text_placeholder (meaning);

			begin -- to_field
				-- field #:
				-- 3/4 : x/y coordinates
				-- 5 : size
				-- 6 : orientation (H/V)
				-- 7 : visible/invisible (V/I)
				-- 8 : aligment horizontal (R,C,L)
				-- 9 : aligment vertical (TNN, CNN, BNN) / font normal, italic, bold, bold_italic (TBI, TBN)

				check_text_content_length (strip_quotes (f (line,2)));
				text.content := type_text_content.to_bounded_string (strip_quotes (f (line,2)));
					
				-- check content vs. meaning. 
				case meaning is

					when NAME =>
						check_prefix_length (content (text));
						check_prefix_characters (
							prefix		=> pac_device_prefix.to_bounded_string (content (text)),
							characters	=> component_prefix_characters);
					
					when VALUE =>
						declare
							value : pac_device_value.bounded_string;
						begin
							value := to_value_with_check (
								value 						=> content (text),
								error_on_invalid_character	=> false);
							-- For the operators convenice no error is raised if invalid
							-- character found. This was the design gets imported but with
							-- (lots of) warnings.
						end;
					
					when DATASHEET =>
						check_datasheet_length (content (text));
						check_datasheet_characters (
							datasheet => type_component_datasheet.to_bounded_string (content (text)));
						
					when PACKGE =>
						check_package_name_length (content (text));
						check_package_name_characters (
							packge		=> type_component_package_name.to_bounded_string (content (text)),
							characters	=> component_package_name_characters);

					when others => null; -- CS

				end case;
				
				--set_x (text.position, mil_to_distance (mil => f (line,3)));
				set (X, mil_to_distance (mil => f (line,3)), text.position);

				--set_y (text.position, mil_to_distance (mil => f (line,4)));
				set (Y, mil_to_distance (mil => f (line,4)), text.position);
				
				text.size := mil_to_distance (mil => f (line,5));

				text.rotation := to_field_orientation (f (line,6));
				
				--text.visible := to_field_visible (
				--	vis_in		=> f (line,7),
				--	schematic	=> false);
			
				text.alignment.horizontal := to_alignment_horizontal (f (line,8));

				text.alignment.vertical   := to_alignment_vertical (f (line,9));

				--text.style := to_text_style (style_in => f (line,9), text => false);
				
				-- NOTE: text.line_width assumes default as no explicit line width is provided here.
				return text;

			end to_field;

			procedure check_text_fields (log_threshold : in type_log_level) is
			-- Tests if all text fields have been found by evaluating the "field found flags".
			-- Validates the fields in CONTEXT WITH EACH OTHER.
			-- NOTE: This is library related stuff.

				-- CS: check orientation.
				-- CS: check visibility.
				-- CS: check hor aligment.
				-- CS: check vert aligment.
				-- CS: check style.
			
				procedure missing_field (meaning : in type_placeholder_meaning) is 
				begin
					log (ERROR, "text field " & to_string (meaning) & " missing !",
						console => true);
					raise constraint_error;
				end missing_field;

				use et_conventions;
				
			begin -- check_text_fields
				log_indentation_up;

				-- write precheck preamble
				log (text => "component " & to_string (tmp_component_name) & " prechecking fields ...", level => log_threshold);
				log_indentation_up;

				log (text => "value", level => log_threshold + 1);
				if not field_value_found then
					missing_field (field_value.meaning);
				else
					-- KiCad insists that the value contains something.
					-- So the first choice is to set value like the generic component name:
					if content (field_value) /= to_string (strip_tilde (tmp_component_name)) then
						log (WARNING, "default value " 
							& content (field_value) & " differs from name "
							& to_string (tmp_component_name) & " !");
					end if;

					check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_value.size);
				end if;
				
				-- appearance specific fields:
				case tmp_appearance is
					when PCB =>
						-- This is a real component.

						-- Since this is a real component. we do the prefix character check 
						-- against the default character set for prefixes as specified in et_devices.
						-- Afterward we validate the prefix. The prefixes for real components are specified
						-- in the et configuration file (see et_conventions).						
						log (text => "prefix", level => log_threshold + 1);
						if not field_prefix_found then
							missing_field (field_reference.meaning);
						else
							check_prefix_characters (
								prefix 		=> tmp_prefix,
								characters	=> prefix_characters);
							
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
						end if;

						log (text => "package/footprint", level => log_threshold + 1);
						if not field_package_found then
							missing_field (field_package.meaning);
						else
							validate_component_package_name (
								type_component_package_name.to_bounded_string (f (
									line => read_line ( -- CS use function package_name
										line			=> content (field_package), -- bel_ic:S_SO14
										comment_mark	=> et_kicad_general.comment_mark,
										ifs				=> latin_1.colon),
									position => 2))); -- the block after the colon

							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_package.size);
						end if;
						
						log (text => "datasheet", level => log_threshold + 1);
						if not field_datasheet_found then
							missing_field (field_datasheet.meaning);
						else
							-- CS validate_datasheet
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_datasheet.size);
						end if;

					when VIRTUAL =>
						-- Since this is a virtual component, we do the prefix character check
						-- against the Kicad specific character set for prefixes. see et_kicad.ads.
						-- Afterward we validate the prefix. The prefixes for virtual components
						-- are KiCad specific.
						log (text => "prefix", level => log_threshold + 1);
						if not field_prefix_found then
							missing_field (field_reference.meaning);
						else
							check_prefix_characters (
								prefix => tmp_prefix,
								characters => component_prefix_characters);
							
							validate_prefix (tmp_prefix);

							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
						end if;

				end case;
				
				log_indentation_down;
				log_indentation_down;				

			end check_text_fields;

			
			procedure insert_component (
			-- NOTE: This is library related stuff.
			-- Updates the current library by inserting the component.
			-- If the component was inserted (should be) the comp_cursor points to the component
			-- for later inserting the units:
				key			: in type_device_library_name.bounded_string;
				components	: in out type_components_library.map) is
			begin

-- 				-- For the logfile write the component name.
-- 				-- If the component contains more than one unit, write number of units.

				check_text_fields (log_threshold + 2);

				case tmp_appearance is
					when VIRTUAL =>

						-- we insert into the given components list a new component
						type_components_library.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like #PWR, #FLG 
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance	=> VIRTUAL,

								-- Whether the component is a power flag can be reasoned by the prefix.
								-- At library level there is no indexed prefix. Power flags have just 
								-- the prefix "#FLG". So we can provide an arbitrary index for the conversion
								-- function "to_power_flag".
								power_flag	=> to_power_flag (to_component_reference (
											text_in			=> to_string (tmp_prefix) & "0", -- #FLG0
											leading_hash	=> true)),

								prefix		=> tmp_prefix,
								value		=> to_value_with_check (
												value => content (field_value),
												error_on_invalid_character => false
												),
												-- For the operators convenice no error is raised if invalid
												-- character found. This was the design gets imported but with
												-- (lots of) warnings.
								
								units		=> type_units_library.empty_map
								)
							);
						
					when PCB =>

						-- we insert into the given components list a new component
						type_components_library.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like 74LS00
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> PCB,
								prefix			=> tmp_prefix,
								value			=> to_value_with_check (content (field_value)),
								units			=> type_units_library.empty_map,

								package_filter	=> type_package_filter.empty_set,
								datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),
								variants		=> pac_variants.empty_map
								)
							);

				end case;

				-- Raise alarm if compoenent is already in the libaray.
				if comp_inserted then
					null;
				else
					log (ERROR, "line" & affected_line (line) & " : component already in library !",
						 console => true);
					raise constraint_error;
				end if;

				exception
					when event: others =>
						log (ERROR, "component " & to_string (tmp_component_name) & " invalid !",
							 console => true);
						-- CS: provide details about the problem (line number, ...)
						log (text => ada.exceptions.exception_message (event));
						raise;
						
			end insert_component;

			
			procedure set_unit_cursor (libraries : in out type_device_libraries.map) is
			-- Sets the unit_cursor according to the current unit_id.
			-- If the unit_id is 0, the unit_cursor is not changed.
		
				procedure locate_unit (
				-- sets the unit_cursor
					key			: in type_component_generic_name.bounded_string;
					component	: in type_component_library) is
				begin
					unit_cursor := component.units.find (to_unit_name (tmp_unit_id));
				end locate_unit;

				procedure locate_component ( 
					key			: in et_kicad_general.type_device_library_name.bounded_string;
					components	: in type_components_library.map) is
				begin
					type_components_library.query_element (comp_cursor, locate_unit'access);
				end locate_component;

			begin -- set_unit_cursor
				if tmp_unit_id > 0 then -- if tmp_unit_id is zero, nothing is done
					type_device_libraries.query_element (lib_cursor, locate_component'access);
				end if;

				exception
					when event:
						others =>
							log (text => ada.exceptions.exception_message (event));
							raise;

			end set_unit_cursor;
				
			
			procedure add_unit is
			-- Add the unit with current tmp_unit_id to current component (indicated by comp_cursor).
			-- Leaves unit_cursor pointing to unit that has been added.
			
			-- If the unit has already been inserted, nothing happens.
			
			-- If the tmp_unit_id is 0 and the total number of units is 1, tmp_unit_id is set to 1
			-- and a single unit is added. This is the case if the component has only one unit
			-- and the draw object has the check "common to all units" set.
			
			-- If the tmp_unit_id is 0 and the total number of units is greater 1, nothing happens. 
			-- This is the case when the component has more than one unit and the draw object
			-- has the check "common to all units" set.

			-- The current tmp_unit_swap_level determines the swap level of the unit to be inserted.
			-- The current tmp_unit_add_level determines the add level of the unit to be inserted. It is "request"
			-- in case the unit in question is an extra unit (supply unit).
			
				procedure insert_unit (
				-- Inserts an internal unit in a component.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component_library) is

					unit : type_unit_library (tmp_appearance);
				begin
					unit.global		:= tmp_unit_global;
					
					component.units.insert (
						key			=> to_unit_name (tmp_unit_id),
						new_item	=> unit,
						position	=> unit_cursor,
						inserted	=> unit_inserted);
				end insert_unit;

				procedure locate_component ( 
					key			: in type_device_library_name.bounded_string;
					components	: in out type_components_library.map) is
				begin
					components.update_element (comp_cursor, insert_unit'access);
				end locate_component;

			begin -- add_unit
				if tmp_unit_id > 0 then
					tmp_component_libraries.update_element (lib_cursor, locate_component'access);
				elsif tmp_units_total = 1 then
					tmp_unit_id := 1;
					tmp_component_libraries.update_element (lib_cursor, locate_component'access);
				else
					null; -- CS
				end if;

				exception
					when event:
						others =>
							log (text => ada.exceptions.exception_message (event));
							raise;

			end add_unit;

			procedure create_units is
			-- Creates empty units in the current component.
			-- The number of units is set by unit_total 
			-- (earlier derived from the component header like "DEF 74LS00 IC 0 30 Y Y 4 F N")
			begin
				for u in 1 .. type_unit_id (tmp_units_total) loop
					tmp_unit_id := u;
					add_unit;
				end loop;
			end create_units;

				
			procedure add_symbol_element (
			-- Adds a symbol element (circle, arcs, lines, ports, etc.) to the unit with the current tmp_unit_id.
			-- The kind of symbol element is given by parameter "element".
			-- The symbol properties are taken from the temporarily variables named tmp_draw_*.
				element		: in type_symbol_element) is

				procedure insert (
				-- Inserts the given element in the unit.
				-- If a port is to be inserted: Aborts on multiple usage of port or pin names.
					key		: in pac_unit_name.bounded_string;
					unit	: in out type_unit_library) is
					pos		: natural := 0; -- helps to trace the program position where an exception occured
				begin
					case element is
						when polyline =>
							unit.symbol.shapes.polylines.append (tmp_draw_polyline);

						when rectangle =>
							unit.symbol.shapes.rectangles.append (tmp_draw_rectangle);

						when arc =>
							unit.symbol.shapes.arcs.append (tmp_draw_arc);
							
						when circle =>
							unit.symbol.shapes.circles.append (tmp_draw_circle);

						when text =>
							unit.symbol.texts.append (tmp_draw_text);

						when port =>
							pos := 100;
							-- CS: test if port not used by other units
							pos := 110;
							-- CS: test if pin name not used by other units
							pos := 190;
							unit.symbol.ports.append (tmp_draw_port);

							pac_terminal_port_map.insert (
								container	=> tmp_terminal_port_map,
								key			=> tmp_terminal_name, -- terminal name
								new_item 	=> (
												name => tmp_draw_port.name, -- port name
												unit => to_unit_name (tmp_unit_id))); -- unit name
							
							
						when others =>
							raise constraint_error;
					end case;

-- 					exception 
-- 						when constraint_error =>
-- 							case pos is
-- 								when 190 => 
-- 									-- Tell the operator which port name the problem is:
-- 									log (
-- 										text => ERROR, "file '" 
-- 											& et_libraries.to_string (lib_file_name) & "' "
-- 											& affected_line (line) 
-- 											& "port name '" & to_string (tmp_draw_port_name)
-- 											& "' already used !",
-- 										console => true);
-- 									raise;
-- 									
-- 								when others =>
-- 									raise;
-- 							end case;
-- 							
-- 						when others => raise;
				end insert;
				
				procedure locate_unit (
				-- Locates the unit indicated by unit_cursor.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component_library) is
				begin
					component.units.update_element (unit_cursor, insert'access);
				end locate_unit;
				
				procedure locate_component ( 
				-- Locates the component indicated by comp_cursor.
					key			: in type_device_library_name.bounded_string;
					components	: in out type_components_library.map) is
				begin -- locate_component
					components.update_element (comp_cursor, locate_unit'access);
				end locate_component;

			begin -- add_symbol_element
				if tmp_unit_id > 0 then 
					--log ("unit id " & type_unit_id'image (tmp_unit_id) , level => 1);
					-- The element belongs to a particular unit exclusively.
					-- Only the current unit of the current component receives the symbol element.
					set_unit_cursor (tmp_component_libraries); -- set unit_cursor according to current tmp_unit_id
					tmp_component_libraries.update_element (lib_cursor, locate_component'access);
				else -- tmp_unit_id = 0
					-- The element belongs to all units of the current component.
					-- In a loop the tmp_unit_id is now modified so that all units (except extra units) 
					-- of the component receive the same symbol element.
					-- units_total was set on passing the component header (DEF 74LS00 IC 0 30 Y Y 4 F N)
					if element /= port then -- should always be true since tmp_unit_id is always greater zero when a port is added to a unit
						for u in 1 .. type_unit_id (tmp_units_total) loop
							tmp_unit_id := u; -- set tmp_unit_id
							set_unit_cursor (tmp_component_libraries);  -- set unit_cursor according to current tmp_unit_id
							tmp_component_libraries.update_element (lib_cursor, locate_component'access);
						end loop;
					else
						raise constraint_error; -- should never happen. see comment above after "if" statement
					end if;
				end if;

				exception
					when event:
						others =>
							log (text => ada.exceptions.exception_message (event));
							raise;

			end add_symbol_element;
			

			procedure set_text_placeholder_properties is
			-- Sets the properties of placeholders in all units of the component indicated by comp_cursor.
			
				procedure set (
				-- Sets the properties of the placeholders in the current unit.
					key		: in pac_unit_name.bounded_string;
					unit	: in out type_unit_library) is
				begin
					-- For the unit we are interested in the properties of the component text fields.
					-- The component text fields as given in the component section look like "F0 "IC" 0 50 50 H V C BIB".
					-- The content (in this example "IC") is not relevant here as it applies for the whole component.
					-- We convert the kicad text placeholder to a native text placeholder (omitting the content).

					-- The kicad placeholders are now converted to ET native placeholders:

					unit.symbol.name := (
							meaning		=> NAME,
							position	=> field_reference.position,
							--style		=> field_reference.style,
							rotation	=> snap (field_reference.rotation),
							size		=> field_reference.size,
							--line_width	=> field_reference.line_width,
							alignment	=> field_reference.alignment);

					unit.symbol.value := (
							meaning		=> VALUE,
							position	=> field_value.position,
							--style		=> field_value.style,
							rotation	=> snap (field_value.rotation),
							size		=> field_value.size,
							--line_width	=> field_value.line_width,
							alignment	=> field_value.alignment);

				end set;
				
				procedure locate_unit (
				-- Locates the unit indicated by unit_cursor.
					key			: in type_component_generic_name.bounded_string;
					component	: in out type_component_library) is
				begin
					component.units.update_element (unit_cursor, set'access);
				end locate_unit;
				
				procedure locate_component ( 
				-- Locates the component indicated by comp_cursor.
					key			: in type_device_library_name.bounded_string;
					components	: in out type_components_library.map) is
				begin -- locate_component
					components.update_element (comp_cursor, locate_unit'access);
				end locate_component;

				-- Set the number of units to be updated:
				total : type_unit_id := type_unit_id (tmp_units_total);
				
			begin -- set_text_placeholder_properties

				-- The number of units to be updated increases by one if an extra unit has been created:
				if extra_unit_available then total := total + 1; end if;
				
				-- In a loop we set tmp_unit_id for each unit and update the unit of the component.
				for u in 1..total loop
					tmp_unit_id := u;
					set_unit_cursor (tmp_component_libraries);
					tmp_component_libraries.update_element (lib_cursor, locate_component'access);
				end loop;

				exception
					when event:
						others =>
							log (text => ada.exceptions.exception_message (event));
							raise;

			end set_text_placeholder_properties;
			
			procedure read_draw_object (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- Creates a symbol element from the given line and adds it to the unit indicated by tmp_unit_id.
					
				function to_unit_id (text : in string) return type_unit_id is
				-- converts a unit id given as string to type_unit_id
				begin 
					return type_unit_id'value (text);
					-- CS: exception handler
				end to_unit_id;

				procedure write_scope_of_object (unit : in type_unit_id) is
				-- Outputs whether the current draw object is common to all units or not.
				begin
					log_indentation_up;
				
					if unit = 0 then
						log (text => "scope: common to all units", level => log_threshold + 1);
					else
						log (text => "scope: unit" & type_unit_id'image (unit), level => log_threshold + 1);
					end if;
					
					log_indentation_down;
				end write_scope_of_object;

				--draw_object : constant string (1..12) := "draw object ";
				
			begin -- read_draw_object
				--log (text => "draw object", level => log_threshold + 1);
				log_indentation_up;

				-- At a certain log level we report the bare line of a draw object as it is:
				--log (text => to_string (line), level => log_threshold + 2);
				
				case type_library_draw'value (f (line,1)) is
					when P => -- polyline
						--log (text => draw_object & "polyline", level => log_threshold);
						log (text => "polyline", level => log_threshold);
						-- A polyline is defined by a string like "P 3 0 1 10 0 0 100 50 70 0 N"
						-- field meaning:
						--  #2 : number of bends (incl. start and end points) (3)
						--  #3 : 0 -> common to all units, otherwise unit id it belongs to
						--  #4 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #5 : line width
						--  #6.. 7  : start point (x/y) (0/0) 
						--  #8.. 9  : bend point (x/y) (0/100)
						-- #10..11  : end point (x/y) (50/70)
						-- last field : fill style N/F/f no fill/foreground/background

						log (text => to_string (line), level => log_threshold);
						-- CS: output properties in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,3));
						write_scope_of_object (tmp_unit_id);

						-- compose polyline
						tmp_draw_polyline := to_polyline (line);

						-- add polyline to unit
						add_symbol_element (polyline);
						
					when S => -- rectangle
						--log (text => draw_object & "rectangle", level => log_threshold);
						log (text => "rectangle", level => log_threshold);
						-- A rectangle is defined by a string like "S -40 -100 40 100 0 1 10 N"
						-- field meaning;
						-- #2..5 : start point -40/-100   end point 40/100
						-- #6 : 0 -> common to all units, otherwise unit id it belongs to
						-- #7 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						-- #8 : line width
						-- #9 : fill style N/F/f no fill/foreground/background

						log (text => to_string (line), level => log_threshold);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,6));
						write_scope_of_object (tmp_unit_id);

						-- compose rectangle
						tmp_draw_rectangle := to_rectangle (line);

						-- add rectangle to unit
						add_symbol_element (rectangle);
						
					when C => -- circle
						--log (text => draw_object & "circle", level => log_threshold);
						log (text => "circle", level => log_threshold);
						-- A circle is defined by a string like "C 0 0 112 0 1 23 N"
						-- field meaning:
						--  #2..3 : center (x/y)
						--  #4 : radius
						--  #5 : 0 -> common to all units, otherwise unit id it belongs to
						--  #6 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #7 : line width (23)
						--  #8 : fill style N/F/f no fill/foreground/background

						log (text => to_string (line), level => log_threshold);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,5));
						write_scope_of_object (tmp_unit_id);

						-- compose circle
						tmp_draw_circle := to_circle (line);
						
						-- add circle to unit
						add_symbol_element (circle);
						
					when A => -- arc
						--log (text => draw_object & "arc", level => log_threshold);
						log (text => "arc", level => log_threshold);
						-- An arc is defined by a string like "A 150 0 150 1800 900 0 1 33 N 0 0 150 150"
						-- NOTE: kicad bug: multiply all y values by -1
						-- field meaning:
						--  #2..3 : center (x/y) 
						--  #4 : radius (150)
						--  #5 : start angle in tenth of degrees (1800)
						--  #6 : end angle in tenth of degrees (900)
						--  #7 : 0 -> common to all units, otherwise unit id it belongs to
						--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #9 : line width 33
						-- #10 : fill style N/F/f no fill/foreground/background
						-- #11..12 : start point (x/y)
						-- #13..14 : end point (x/y)

						log (text => to_string (line), level => log_threshold);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,7));
						write_scope_of_object (tmp_unit_id);

						-- compose arc
						tmp_draw_arc := to_arc (line);
						
						-- add arc to unit
						add_symbol_element (arc);

					when T => -- text
						--log (text => draw_object & "text", level => log_threshold);
						log (text => "text", level => log_threshold);
						-- A text is defined by a string like "T 0 0 300 60 0 0 0 leuchtdiode Normal 0 C C"
						-- Space characters whitin the actual text are replaced by tilde as in this example:
						-- "T 0 -100 0 60 0 1 0 gate~C Normal 0 C C"
						-- field meaning:
						--  #2 : rotation in tenth of degrees (counter clock wise), assumes only values of 0 or 900
						--  #3..4 : center (x/y)
						--  #5 : size 
						--  #6 : ? - unknown CS
						--  #7 : 0 -> common to all units, otherwise unit id it belongs to
						--  #8 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						--  #9 : content "leuchtdiode"
						-- #10 : style Nomal/Italic
						-- #11 : bold on/off (0/1) 
						-- #12 : alignment left/center/right L/C/R
						-- #13 : alignment top/center/bottom T/C/B

						log (text => to_string (line), level => log_threshold);
						-- CS: output properites in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,7));
						write_scope_of_object (tmp_unit_id);

						-- compose text
						tmp_draw_text := to_text (line);
						
						-- add text to unit
						add_symbol_element (text);
						
					when X => -- port
						--log (text => draw_object & "port", level => log_threshold);
						log (text => "port", level => log_threshold);
						-- A port is defined by a string like "X ~ 1 0 150 52 D 51 50 1 1 P"
						-- field meaning:
						--  #2 : port name (~)
						--  #3 : pin number (1)
						--  #4..5 : position x/y (0/150)
						--  #6 : pin length (52)
						--  #7 : orientation up/down/left/right (U/D/L/R)
						--  #8 : pin number size (51)
						--  #9 : port name size (50)
						-- #10 : 0 -> common to all units, otherwise unit id it belongs to
						-- #11 : 1 -> not common to all body styles (alternative representation or DeMorgan) -- CS: verify
						-- #12 : electrical type (direction), see et_kicad.ads for more
						-- #13 : optional field: pin style, see et_kicad.ads for more

						log (text => to_string (line), level => log_threshold);
						-- CS: output properties in a human readable form instead.
						
						tmp_unit_id := to_unit_id (f (line,10));
						write_scope_of_object (tmp_unit_id);

						-- compose port
						tmp_draw_port := to_port (line);

						-- If this is a unit specific port it gets added to the unit. If it applies for the
						-- whole component, we create an extra unit and insert it there. An extra unit is
						-- created ONLY ONCE. Successive unit-wide ports are added there.
						-- An extra unit always has the add level "request" since it harbors the supply ports.
						-- When adding a port, tmp_unit_id is always greater zero.
						if tmp_unit_id > 0 then
							-- add unit specific port to unit
							--log (text => "unit id " & type_unit_id'image (tmp_unit_id) , level => log_threshold);
							add_symbol_element (port);
						else 
							-- The unit id changes from 0 to tmp_units_total + 1 (one notch above the total number) :
							tmp_unit_id := type_unit_id (tmp_units_total) + 1;
							-- If no extra unit has been created yet -> create one with add level "request".
							if not extra_unit_available then 
								tmp_unit_add_level := request;
								tmp_unit_global := true; -- this is a unit with power supply pins that apply for the whole component
								add_unit;
								extra_unit_available := true;
							else
								null;
							end if;
							-- insert the port in the extra unit
							--log (text => "unit id " & type_unit_id'image (tmp_unit_id) , level => log_threshold);						
							add_symbol_element (port);
						end if;
				end case;

				log_indentation_down;
			end read_draw_object;

			procedure add_footprint (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- Reads the proposed footprint and adds it to the package filter of the current component.
				fp : type_package_proposal.bounded_string;

				procedure do_it is
				-- Adds the footprint finally.
					procedure insert_footprint (
						key			: in type_component_generic_name.bounded_string;
						component	: in out type_component_library) is
					begin
						component.package_filter.insert (fp);
					end insert_footprint;
					
					procedure locate_component ( 
						key			: in type_device_library_name.bounded_string;
						components	: in out type_components_library.map) is
					begin
						components.update_element (comp_cursor, insert_footprint'access);
					end locate_component;

				begin -- add_footprint
					tmp_component_libraries.update_element (lib_cursor, locate_component'access);

					exception
						when event:
							others =>
								log (text => ada.exceptions.exception_message (event));
								raise;

				end do_it;
				
			begin -- add_footprint
	-- 			log (text => "footpint/package filter", level => log_threshold + 1);
				log_indentation_up;

				fp := type_package_proposal.to_bounded_string (f (line,1));
				log (text => type_package_proposal.to_string (fp), level => log_threshold);

				do_it;
				
				log_indentation_down;
			end add_footprint;
		

			procedure read_field (line : in type_fields_of_line; log_threshold : in type_log_level) is
			-- NOTE: This is library related stuff.
			-- Reads the text field of a component in a set of temporarily variables field_reference, field_value, ...
			-- Sets the "field found flag" according to the field being detected.
			-- Text fields look like:
			-- F0 "#PWR" 0 -200 50 H I C CNN
			-- F1 "GND" 0 -100 50 H V C CNN
			-- F2 "" 0 0 50 H I C CNN
			-- F3 "" 0 0 50 H I C CNN
					
				use et_string_processing;

			begin -- read_field
				
				-- read text fields from a component library (thats why scheamtic => false)
				case to_text_meaning (line => line, schematic => false) is

					-- If we have the prefix field like "F0 "U" 0 50 50 H V C CNN"
					when NAME =>
									
						-- CS: Do a cross check of prefix and reference -- "U" 
						-- The prefix is already defined in the component hearder. 
						-- Why this redundance ? Ask the kicad makers...
						if strip_quotes (f (line,2)) = pac_device_prefix.to_string (tmp_prefix) then
							null; -- fine
						else
							log (WARNING, affected_line (line) & ": prefix vs. reference mismatch !");
							-- CS: better raise constraint_error
						end if;

						field_prefix_found := true;
						field_reference := to_field (line => line, meaning => NAME);

					-- If we have a value field like "F1 "74LS00" 0 -100 50 H V C CNN"
					when VALUE =>
						field_value_found := true;
						field_value := to_field (line => line, meaning => VALUE);

					-- If we have a footprint field like "F2 "bel_resistors:S_0805" 0 -100 50 H V C CNN"
					-- NOTE: the part before the colon is the containing library. The part after the colon 
					-- is the actual footprint/package name.
					when PACKGE =>

						field_package_found := true;
						field_package := to_field (line => line, meaning => packge);

					-- If we have a datasheet field like "F3 "" 0 -100 50 H V C CNN"
					when DATASHEET =>

						field_datasheet_found := true;
						field_datasheet := to_field (line => line, meaning => datasheet);

					when others => null;
						-- CS: warning about illegal fields ?
						-- CS: other text fields ?
				end case;
				
			end read_field;
		

			procedure build_package_variant is
			-- Builds from tmp_terminal_port_map and field_package the default package variant.
			-- NOTE: Since kicad does not know package variants, we can only build the
			-- one and only DEFAULT variant. The default variant name is the same as the package name.
			-- All this applies for real components only (appearance sch_pcb).
				
				procedure locate_component (
					lib_name	: in type_device_library_name.bounded_string;
					components	: in out type_components_library.map) is

					procedure build (
						comp_name	: in type_component_generic_name.bounded_string;
						component	: in out type_component_library) is

						use pac_variants;
						use pac_terminal_port_map;

						tmp_variant_name : pac_package_variant_name.bounded_string; -- temporarily used for building the variant name
						tmp_variants : pac_variants.map; -- temporarily used for building the variant

						full_package_library_name : type_package_library_name.bounded_string;
					begin
						case component.appearance is
							when PCB => -- real component

								-- The name of the default variant is the package
								-- name itself (instead of an empty string or a string like "default"):
								check_variant_name_length (to_string (package_name (content (field_package)))); -- S_SO14
								tmp_variant_name := to_variant_name (to_string (package_name (content (field_package)))); -- S_SO14
								check_variant_name_characters (tmp_variant_name);

								-- Find the library where the given package is stored in.
								full_package_library_name := full_library_name ( -- ../lbr_dir_1/bel_ic.pretty
									library_name	=> library_name (content (field_package)), -- bel_ic
									package_name	=> package_name (content (field_package)), -- S_SO14
									log_threshold	=> log_threshold + 1);
								
								-- Test whether library, package and terminal_port_map fit together.
								if terminal_port_map_fits (
									library_name		=> full_package_library_name,
									package_name		=> package_name (content (field_package)), -- S_SO14
									terminal_port_map	=> tmp_terminal_port_map) then
								
										-- Insert in tmp_variants (which is temporarily) the default variant.
										insert (
											container	=> tmp_variants,
											key			=> tmp_variant_name, -- the same as the package name -- S_SO14
											new_item 	=> (
												-- The package field contains something like "bel_ic:S_SO14".
												-- It provides the library name and the package name.

												-- create package variant
												package_model => to_file_name (compose (
													containing_directory	=> to_string (full_package_library_name),
													name					=> to_string (package_name (content (field_package))))),

												-- The terminal to port map tmp_terminal_port_map is now finally copied
												-- to its final destination:
												terminal_port_map => tmp_terminal_port_map)); -- H4/GPIO2

										log (text => to_string (tmp_variant_name), level => log_threshold + 2); 
									
										-- Assign package variant to component
										component.variants := tmp_variants;
								else
									null; -- CS variant could not be built, output something here, raise constraint error ?
								end if;
								
							when others => null;
						end case;
					end build;
					
				begin -- locate_component
					components.update_element (comp_cursor, build'access);
				end locate_component;
				
			begin -- build_package_variant
				log_indentation_up;
				log (text => "building default package variant ...", level => log_threshold + 1);
				log_indentation_up;
				
				type_device_libraries.update_element ( 
					container	=> tmp_component_libraries,
					position	=> lib_cursor,
					process		=> locate_component'access);
				
				log_indentation_down;
				log_indentation_down;

				exception
					when event:
						others =>
							log (text => ada.exceptions.exception_message (event));
							raise;

			end build_package_variant;

		begin -- read_library
			log_indentation_up;
			
			log (text => "components", level => log_threshold + 1);
			log_indentation_up;
			
			while not end_of_file loop

				-- Store line in variable "line" (see et_string_processing.ads)
				-- The schematic library files use comments (#). But only the comments at the begin
				-- of a line are relevant. Others are to be ignored. Thus test_whole_line is false.
				line := read_line(
							line => get_line,
							comment_mark => "#",
							test_whole_line => false,
							number => ada.text_io.line (current_input));
				
				case field_count (line) is
					when 0 => null; -- we skip empty lines
					when others =>

						-- Wait for component header like "DEF 74LS00 U 0 30 Y Y 4 F N".
						-- Once the header was read, the component_entered flag goes true so that
						-- no further header is expected. Once the component footer (ENDDEF) has been read,
						-- the component_entered flag goes false.
						-- The lines right after the component header are the so called "fields". The fields can be
						-- regarded as attributes (similar to EAGLE). The first four attributes are hard coded in kicad
						-- and are thus always there. In the component library the fields start with "F0" .. "Fn".

						log (text => to_string (line), level => log_threshold + 4);
						
						if not component_entered then 
							
							if f (line,1) = def then
								component_entered := true;

								init_temp_variables;
								
								-- Since we are reading the fields, we set the active_section to "fields"
								active_section := fields;

								-- The commponent header provides the first component properties:
								tmp_component_name := type_component_generic_name.to_bounded_string (
														f (line,2)); -- 74LS00

								-- The generic component name must be checked for invalid characters.
								-- NOTE: we test against the kicad specific character set that allows a tilde.
								check_generic_name_characters (
									name		=> tmp_component_name,
									characters	=> component_generic_name_characters_lib);
								
								-- for the log:
								--log (text => field (line,2), level => log_threshold + 1); -- 74LS00
								log (text => to_string (tmp_component_name), level => log_threshold + 1); -- 74LS00

								-- From the header we extract some basic information about the component:
								
								-- The line it is about looks like:  DEF 74LS00 U 0 30 Y Y 4 F N
								-- The fields meaning is as follows:
								--  #2 : name, like 74LS00
								--  #3 : prefix, like U
								--  #4 : unknown, always zero. CS What is it good for ?
								--  #5 : pin name position offset of supply pins, if "place pin names inside" is off. the offset assumes zero
								--  #6 : show pin/pad number Y/N,
								--  #7 : show pin/port name Y/N,
								--  #8 : units total, -- like 4
								--  #9 : all units not interchangeable L (otherwise F), (similar to swap level in EAGLE)
								--  #10: power symbol P (otherwise N)

								tmp_prefix := pac_device_prefix.to_bounded_string (f (line,3)); -- U

								-- Detect invalid characters in tmp_prefix:
								-- NOTE: we test against the kicad specific character set that allows a #
								check_prefix_characters (
									prefix		=> tmp_prefix,
									characters	=> component_prefix_characters);

								-- The unknown field #4 is always a zero
								if f (line, 4) /= "0" then
									log (WARNING, "expect 0 in field #4 !");
								end if;
								
								tmp_port_name_offset := mil_to_distance (mil => f (line,5)); -- relevant for supply pins only
								tmp_terminal_name_visible := to_pin_visibile (f (line,6));
								tmp_port_name_visible := to_port_visibile (f (line,7));
								
								-- Get number of units and set swap level as specified in field #9.
								-- Swap level assumes default if only one unit available.
								tmp_units_total := type_units_total'value (f (line,8));
								if tmp_units_total > 1 then
									log_indentation_up;
									log (text => "with" & type_units_total'image (tmp_units_total) & " units", level => log_threshold + 2);

									-- From the "interchangeable" flag we set the component wide swap level. It applies for 
									-- all units of the component (except extra units):
									tmp_unit_swap_level := to_swap_level (f (line,9));
									log_indentation_down;
								else
									tmp_unit_swap_level := swap_level_default;
								end if;

								-- read the appearance flag (N/P) in subfield #10
								-- This is about a component in a library -> schematic => false
								tmp_appearance := to_appearance (line => line, schematic => false);

							end if;
						else -- we are inside a component section and process subsections

							-- We wait for the end of component mark (ENDDEF) and clear the component_entered flag accordingly.
							if f (line,1) = enddef then
								component_entered := false;

								-- Set placeholders (reference, value, ...) in internal units.
								-- The placeholder properties are known from the field-section.
								-- The placeholder properties apply for all units.
								set_text_placeholder_properties;

								-- If this is a real component, build package variant from tmp_terminal_port_map
								if tmp_appearance = PCB then
									build_package_variant;
								end if;
								
								-- log component properties
								-- CS: currently no need to output a summary of the component
								--if log_level >= log_threshold + 1 then
								--	et_libraries.write_component_properties (component => comp_cursor);
								--end if;
							else
							-- As long as the component end mark does not appear, we process subsections as 
							-- indicated by active_section:
								log_indentation_up;
							
								case active_section is
									when fields =>
										-- Here we read the "fields". 
										-- There is no end mark for the field list. 
										-- NOTE #1: The only way to detect the end of the field list is to wait for the
										-- header of the footprint list ($FPLIST) or the header of the "draw" list (DRAW).
										-- Then the active_section is set accordingly.

										-- We wait for the header of the footprint or draw list like "$FPLIST" or "DRAW"
										-- and set active_section accordingly.
										-- The component wide data is complete at this time. The component
										-- is to be inserted into the library without any units. Units are assembled and 
										-- added to the component when the section "DRAW" is processed..
										
										-- As long as none of those headers occurs, we read the text fields.
										if f (line,1) = fplist then
											
											-- Insert the component into the current library (indicated by lib_cursor):
											type_device_libraries.update_element ( 
												container	=> tmp_component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;
											
											active_section := footprints;
											--log (text => "footprint/package filter begin", level => log_threshold + 1);
											log (text => "footprint/package filter", level => log_threshold + 2);

										elsif f (line,1) = et_kicad_libraries.draw then

											-- Insert the component into the current library (indicated by lib_cursor):
											type_device_libraries.update_element ( 
												container	=> tmp_component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;

											active_section := draw;
											log (text => "draw begin", level => log_threshold + 2);
										else
											-- Read the text fields in a set of temporarily variables field_prefix, field_value, ...
											read_field (line, log_threshold + 2);
										end if;

									when footprints =>
										-- Here we read the footprint list (similar to package variants in EAGLE):

										-- The list of footprints looks like this :

										-- $FPLIST
										--  Pin_Header_Straight_1X06
										--  Pin_Header_Angled_1X06
										--  Socket_Strip_Straight_1X06
										--  Socket_Strip_Angled_1X06
										-- $ENDFPLIST

										-- As long as the footer of the list ($ENDFPLIST) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- that this subsection has been processed.
										if f (line,1) = endfplist then
											active_section := none;
											--log (text => "footprint/package filter end", level => log_threshold + 1);
										else
											-- Process lines:
											add_footprint (line, log_threshold + 2);
										end if;

									when draw =>
										-- Here we read the drawing list where lines, arcs and pins are.
										
										-- As long as the footer of the list (ENDDRAW) does not appear,
										-- we process the lines of this subsection.
										-- When the footer appears, we set active_section to "none" which means
										-- thate this subsection has been processed.
										if f (line,1) = enddraw then
											active_section := none;
											log (text => "draw end", level => log_threshold + 2);
										else
											-- Read draw objects
											read_draw_object (line, log_threshold + 2);
										end if;
										
									when none =>
										-- If no subsection is being processed, we wait for the "draw" header (DRAW)
										-- and set the active_section accordingly.
										-- NOTE #2: the active section "fields" is not set here but when the fields are read (see NOTE #1)
										if f (line,1) = et_kicad_libraries.draw then
											active_section := draw;
											log (text => "draw begin", level => log_threshold + 2);
										end if;

								end case; -- active_section

								log_indentation_down;
								
							end if;
							
						end if; -- inside component section
				end case;

			end loop;

			log_indentation_down;
			log_indentation_down;

			exception
				when event:
					others =>
						log (ERROR, affected_line (line) & to_string (line), console => true);
						log (text => ada.exceptions.exception_message (event));
						raise;
		end read_library;

		-- When accessing library files we need this:
		library_handle : ada.text_io.file_type;
		
	begin -- read_components_libraries
		log (text => "reading component libraries ...", level => log_threshold);
		log_indentation_up;

		-- 	The tmp_component_libraries are empty (created on reading the project file) and must be filled.
		case et_import.cad_format is

			-- For V4:		
			when et_import.KICAD_V4 =>

				-- If the search_list_tmp_component_libraries is empty if there are no libraries defined -> nothing to do.
				if not type_library_names.is_empty (search_list_component_libraries) then

					-- Set lib_cursor to first library and loop in tmp_component_libraries.
					lib_cursor := tmp_component_libraries.first;
					while type_device_libraries."/=" (lib_cursor, type_device_libraries.no_element) loop

						-- log library file name
						log (text => to_string (type_device_libraries.key (lib_cursor)), level => log_threshold + 1);
						
						-- open the same-named file and read it
						open (
							file => library_handle,
							mode => in_file,
							name => to_string (type_device_libraries.key (lib_cursor)));
							
						-- Now we read the library file and add components
						-- to the library pointed to by lib_cursor:
						set_input (library_handle);
						read_library (log_threshold + 1);

						close (library_handle);

						type_device_libraries.next (lib_cursor);
					end loop;
					
				else
					log (WARNING, "no component libraries defined in project file !");
				end if;

				
			-- For V5;
			when et_import.KICAD_V5 =>

				-- If tmp_component_libraries is empty -> nothing to do
				if not type_device_libraries.is_empty (tmp_component_libraries) then

					-- Set lib_cursor to first library and loop in tmp_component_libraries.
					lib_cursor := tmp_component_libraries.first;
					while type_device_libraries."/=" (lib_cursor, type_device_libraries.no_element) loop

						-- log library file name
						log (text => to_string (type_device_libraries.key (lib_cursor)), level => log_threshold + 1);
						
						-- open the same-named file and read it
						open (
							file => library_handle,
							mode => in_file,
							name => to_string (type_device_libraries.key (lib_cursor)));
							
						-- Now we read the library file and add components
						-- to the library pointed to by lib_cursor:
						set_input (library_handle);
						read_library (log_threshold + 1);

						close (library_handle);

						type_device_libraries.next (lib_cursor);
					end loop;
					
				else
					log (WARNING, "no component libraries defined !");
				end if;

				
			when others =>
				raise constraint_error;

		end case;
		log_indentation_down;
	end read_components_libraries;


	function to_package_variant (
	-- Used when reading schematic. Returns the package variant of a component.
	-- Input parameters: the full name of the component library, generic name therein,
	-- name of package library and package name.
		component_library 	: in et_kicad_general.type_device_library_name.bounded_string; 	-- ../lbr/bel_logic.lib
		generic_name 		: in type_component_generic_name.bounded_string; 				-- 7400
		package_library 	: in et_kicad_general.type_library_name.bounded_string; 		-- bel_ic
		package_name 		: in et_packages.type_component_package_name.bounded_string;	-- S_SO14
		log_threshold		: in et_string_processing.type_log_level)
		return pac_package_variant_name.bounded_string is 					-- D

		library_cursor : type_device_libraries.cursor; -- points to the component library
		
		use et_string_processing;

		use et_devices;
		variant : pac_package_variant_name.bounded_string; -- variant name to be returned
		
		-- temporarily here the name of the package library is stored:
		use type_package_library_name;
		full_package_library_name : type_package_library_name.bounded_string; -- ../lbr/bel_ic

		use et_packages;
		
		procedure locate_component (
		-- Locates the given generic component in the component libraray.
			library_name	: in type_device_library_name.bounded_string;
			components 		: in out type_components_library.map) is

			use type_components_library;
			component_cursor : type_components_library.cursor; -- points to the generic component
			
			procedure query_variants (
			-- Queries the package variants of the generic component.
				component_name	: in type_component_generic_name.bounded_string; -- RESISTOR
				component 		: in out type_component_library) is

				use type_component_package_name;
				use pac_variants;
				use pac_package_variant_name;

				-- This cursor points to the package variant being queryied.
				variant_cursor : pac_variants.cursor := component.variants.first;

				-- If a new package variant is to be built, it is temporarily stored here:
				new_variant : type_variant;
			
			begin -- query_variants
				log (text => "querying package variants ...", level => log_threshold + 2);
				log_indentation_up;

				-- Loop through package variants. The first variant is the default variant
				-- as specified by the component model.
				-- If no suitable variant in component.variants can be located, then
				-- a new variant must be built. This will be the case when the user has
				-- assigned a different package to the component from inside the schematic editor.
				while variant_cursor /= pac_variants.no_element loop

					log (text => "probing " 
						 & enclose_in_quotes (et_devices.to_string (key (variant_cursor)))
						 & " ...", level => log_threshold + 3);

					-- From the library and package name we can reason the variant name.
					-- So if both the given library and package name match, the variant name
					-- is set to be returned.
					if element (variant_cursor).package_model = et_packages.to_file_name (compose (
							containing_directory	=> et_packages.to_string (full_package_library_name),
							name					=> et_packages.to_string (package_name))) then
						
						log (text => "variant " 
							& to_string (package_variant => key (variant_cursor)) 
							& " used", level => log_threshold + 1);

						-- Set the variant name to be returned:
						variant := key (variant_cursor);
						
						exit; -- no further search required
					end if;

					next (variant_cursor);
				end loop;

				-- If no suitable package variant has been found, a new one must be created.
				if variant_cursor = pac_variants.no_element then
					
					-- Package variant not defined in library. Package assigned inside the schematic editor.
					-- Make sure the terminal_port_map (there is only one) can be applied 
					-- on this package variant.
					log (text => "unknown variant found. validating ...", level => log_threshold + 3);
					log_indentation_up;

					-- Set variant cursor to default variant. Later the terminal_port_map is 
					-- copied from here.
					variant_cursor := component.variants.first; 

					-- Test whether the new variant complies with the terminal_port_map
					if terminal_port_map_fits (
						library_name 		=> full_package_library_name,
						package_name 		=> package_name,
						terminal_port_map	=> element (variant_cursor).terminal_port_map) then

						log (text => "Terminal-port-map fits. Updating library ...", level => log_threshold + 4);

						-- build the new package variant
						new_variant := (
							package_model => et_packages.to_file_name (compose (
								containing_directory	=> et_packages.to_string (full_package_library_name),
								name					=> et_packages.to_string (package_name))),
							
							terminal_port_map	=> element (variant_cursor).terminal_port_map
							);

						-- insert the new package variant in the component (in library)
						pac_variants.insert (
							container	=> component.variants,
							key			=> to_variant_name (to_string (packge => package_name)),
							new_item	=> new_variant);

						--log (text => count_type'image (pac_variants.length (component.variants)));
						
						-- Set the variant name to be returned:
						variant := to_variant_name (to_string (packge => package_name));
						
					else
						log (ERROR, "Terminal-port-map does not fit !", console => true); -- CS: more details
						raise constraint_error; -- CS
					end if;

					log_indentation_down;
				end if;

				log_indentation_down;
				
				exception
					when event:
						others =>
							log_indentation_reset;
							put_line (ada.exceptions.exception_message (event));
							raise;

			end query_variants;
			
		begin -- locate_component
			log (text => "locating generic component " & enclose_in_quotes (to_string (generic_name)) 
				 & " in library ...", level => log_threshold + 1);
			log_indentation_up;

			-- Locate the component in the library by its generic name.
			-- If not found, search the component again with a tilde prepended to
			-- to the generic name:
			component_cursor := components.find (generic_name);
			if component_cursor = type_components_library.no_element then
				component_cursor := components.find (prepend_tilde (generic_name));
			end if;

			-- query the package variants of the generic component
			type_components_library.update_element (
				container	=> components,
				position 	=> component_cursor,
				process 	=> query_variants'access);

			log_indentation_down;

			exception
				when event:
					others =>
						log_indentation_reset;
						put_line (ada.exceptions.exception_message (event));
						raise;

		end locate_component;
		
	begin -- to_package_variant
		log (text => "validating/making package variant ...", level => log_threshold);
		log_indentation_up;

		-- Compose the full name of the package library:
		--full_package_library_name := to_full_library_name (group => library_group, lib_name => package_library);
		full_package_library_name := full_library_name (
			library_name 	=> package_library, -- bel_ic
			package_name	=> package_name,	-- S_SO14
			log_threshold	=> log_threshold + 1);

		log (text => "full package library name is " 
			 & enclose_in_quotes (et_packages.to_string (full_package_library_name)),
			 level => log_threshold + 1);
		
		-- locate the given component library
		library_cursor := tmp_component_libraries.find (component_library);

		log (text => "component library is " 
			 & enclose_in_quotes (to_string (type_device_libraries.key (library_cursor))),
			 level => log_threshold + 1);

		-- locate the given generic component
		type_device_libraries.update_element (
			container	=> tmp_component_libraries,
			position	=> library_cursor,
			process		=> locate_component'access);

		log (text => "variant is " & enclose_in_quotes (to_string (variant)), level => log_threshold + 1);
		
		log_indentation_down;
		
		return variant;
	end to_package_variant;

	function component_power_flag (cursor : in type_components_library.cursor)
	-- Returns the component power flag status.
		return type_power_flag is
		use et_string_processing;
	begin
		-- Only vitual components have the power flag property. 
		-- For real components the return is always false;
-- 		if et_libraries."=" (type_components_library.element (cursor).appearance, et_libraries.SCH) then
		if type_components_library.element (cursor).appearance = VIRTUAL then
			--log (text => "virtual component");
			--if type_components.element (cursor).power_flag then
			--	log (text => "power flag on");
			--else
			--	log (text => "power flag off");
			--end if;
			return type_components_library.element (cursor).power_flag;
		else
			--log (text => "real component");
			return NO;
		end if;
	end component_power_flag;

	function first_port (component_cursor : in type_portlists.cursor) return type_ports.cursor is
	-- Returns a cursor pointing to the first port of a component in the portlists.
		port_cursor : type_ports.cursor;
	
		procedure set_cursor (
			name 	: in type_device_name;
			ports	: in type_ports.list) is
		begin
			port_cursor := type_ports.first (ports);
		end set_cursor;
			
	begin -- first_port
		type_portlists.query_element (
			position	=> component_cursor,
			process 	=> set_cursor'access);

		return port_cursor;
	end first_port;
	
	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in et_kicad_general.type_device_library_name.bounded_string;
		component	: in type_component_generic_name.bounded_string) 
		return type_components_library.cursor is

		lib_cursor	: type_device_libraries.cursor;
		use type_components_library;
		comp_cursor	: type_components_library.cursor := no_element;
	
		use type_device_libraries;
		use et_string_processing;

		procedure locate (
			library 	: in et_kicad_general.type_device_library_name.bounded_string;
			components	: in type_components_library.map) is
		begin
			-- Generic names in library sometimes start with a tilde. 
			-- So, first we search for the given component without tilde.
			-- If no match, sarch for the given component with a tilde prepended.
			-- If still no match, comp_cursor is empty (no_element).
			comp_cursor := components.find (component); -- TRANSISTOR_NPN

			-- CS: the follwing should be executed if the import format is kicad_v4:
			if comp_cursor = type_components_library.no_element then
				comp_cursor := components.find (prepend_tilde (component)); -- ~TRANSISTOR_NPN
				--CS: log ?
			end if;
		end locate;
	
	begin
		lib_cursor := tmp_component_libraries.find (library);

		-- If the given library exists, locate the given component therein.
		-- Otherwise generate a warning.
		if lib_cursor /= type_device_libraries.no_element then
			query_element (
				position	=> lib_cursor,
				process		=> locate'access);
		else
			log (WARNING, "library " & et_devices.to_string (library) & " not found !");
			-- CS: raise constraint_error ?
		end if;

		return comp_cursor;
	end find_component;

	procedure write_note_properties (
		note 			: in type_text;
		log_threshold	: in et_string_processing.type_log_level := 0) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_text;
	begin
		log (text => "text note" & to_string (
			position => note.position, scope => et_kicad_coordinates.XY), level => log_threshold);

		log_indentation_up;

		-- content
		if type_text_content.length (note.content) > 0 then
			log (text => "content '" & to_string (note.content) & "'", level => log_threshold);
		else
			log (text => message_warning & "no content !", level => log_threshold); 
		end if;
	
		if log_level >= log_threshold + 1 then
			
			-- size
			log (text => "size" & to_string (note.size));

-- 			-- style
-- 			log (text => "style " & to_lower (to_string (note.style)));

			-- line width
-- 			log (text => "line width" & to_string (note.line_width));

			-- rotation
			log (text => "rotation" & to_string (note.rotation));

			-- visible
			--log (text => "visible " & to_lower (et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log (text => et_text.to_string (note.alignment));
		end if;
		
		log_indentation_down;
	end write_note_properties;
	
end et_kicad_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
