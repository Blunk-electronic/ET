------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                KICAD                                     --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;
with ada.environment_variables;

with et_conventions;
with et_kicad.pcb;				use et_kicad.pcb;

package body et_kicad.schematic is

	use et_general.type_net_name;


	
	-- Returns the base name of the given schematic file name as submodule name.
	function to_submodule_name (file_name : in type_schematic_file_name.bounded_string)
		return type_submodule_name.bounded_string is
		use ada.directories;
	begin
		-- CS: test if given submodule has an extension. if not return
		-- submodule as it is.
		return to_submodule_name (base_name (et_kicad_coordinates.to_string (file_name)));
	end to_submodule_name;

	-- Here we append a sheet name to the path_to_sheet.
	-- CS: unify with procedure delete_last_module_name_from_path
	--procedure append_name_of_parent_module_to_path (submodule : in et_coordinates.type_submodule_name.bounded_string) is
	procedure append_sheet_name_to_path (sheet : in type_submodule_name.bounded_string) is
		use et_string_processing;
		use ada.directories;
		--use type_submodule_name;
	begin
		-- CS: limit path length !
-- 		log (text => "append path_to_submodule " 
-- 			& base_name (type_submodule_name.to_string (submodule)), level => 1);

		type_path_to_submodule.append (path_to_sheet, sheet);
			--to_bounded_string (base_name (type_submodule_name.to_string (submodule))));

	end append_sheet_name_to_path;
	
	-- Here we remove the last submodule name form the path_to_sheet.
	procedure delete_last_module_name_from_path is begin
		type_path_to_submodule.delete_last (path_to_sheet);
	end delete_last_module_name_from_path;

	procedure module_not_found (module : in type_submodule_name.bounded_string) is
	-- Returns a message stating that the given module does not exist.
		use et_string_processing;
	begin
		log (ERROR, " module " & to_string (module) & " not found !");
		raise constraint_error;
	end module_not_found;

	
	function unit_exists (
	-- Returns true if the unit with the given name exists in the given list of units.
		name	: in et_devices.pac_unit_name.bounded_string; -- the unit being inquired
		units	: in type_units_schematic.map) -- the list of units
		return boolean is
		use type_units_schematic;
	begin
		if type_units_schematic.find (container => units, key => name) = type_units_schematic.no_element then
			return false;
		else	
			return true;
		end if;
	end unit_exists;

	function position_of_unit (
	-- Returns the coordinates of the unit with the given name.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name 	: in et_devices.pac_unit_name.bounded_string; -- the unit being inquired
		units 	: in type_units_schematic.map) -- the list of units
		return et_kicad_coordinates.type_position is
		unit_cursor : type_units_schematic.cursor;
	begin
		unit_cursor := type_units_schematic.find (container => units, key => name);
		return type_units_schematic.element (unit_cursor).position;
	end position_of_unit;

	function mirror_style_of_unit (
	-- Returns the mirror style of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name 	: in et_devices.pac_unit_name.bounded_string; -- the unit being inquired
		units 	: in type_units_schematic.map) -- the list of units
		return et_schematic.type_mirror is
		unit_cursor : type_units_schematic.cursor;
	begin
		unit_cursor := type_units_schematic.find (container => units, key => name);
		return type_units_schematic.element (unit_cursor).mirror;
	end mirror_style_of_unit;

	function orientation_of_unit (
	-- Returns the orientation of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name	: in et_devices.pac_unit_name.bounded_string; -- the unit being inquired
		units	: in type_units_schematic.map) -- the list of units
		return et_coordinates.type_rotation is
		unit_cursor : type_units_schematic.cursor;
	begin
		unit_cursor := type_units_schematic.find (container => units, key => name);
		return type_units_schematic.element (unit_cursor).rotation;
	end orientation_of_unit;

	
	procedure write_unit_properties (
	-- Writes the properties of the unit indicated by the given cursor.
		unit			: in type_units_schematic.cursor;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_symbols;
	begin
		log_indentation_up;
		
		-- unit name
		log (text => "properties of unit " 
			& to_string (type_units_schematic.key (unit)), level => log_threshold);

		log_indentation_up;
		
		--alternative representation
		log (text => "alternative (deMorgan) representation " 
			 & to_lower (type_de_morgan_representation'image (type_units_schematic.element (unit).alt_repres)),
			 level => log_threshold);

		-- timestamp
		log (text => "timestamp " 
			& string (type_units_schematic.element (unit).timestamp), level => log_threshold);

		-- position
		log (text => to_string (position => type_units_schematic.element (unit).position), level => log_threshold);

		-- orientation or angle
		log (text => to_string (type_units_schematic.element (unit).rotation), level => log_threshold);

		-- mirror style
		log (text => et_schematic.to_string (type_units_schematic.element (unit).mirror, verbose => true), level => log_threshold);

		-- placeholders
		log (text => "placeholders", level => log_threshold + 1);
		log_indentation_up;

			-- reference
			write_placeholder_properties (
				placeholder		=> type_units_schematic.element (unit).reference,
				log_threshold	=> log_threshold + 1);

			-- value
			write_placeholder_properties (
				placeholder		=> type_units_schematic.element (unit).value,
				log_threshold	=> log_threshold + 1);

			-- some placeholders exist depending on the component appearance
			case type_units_schematic.element (unit).appearance is
				when et_symbols.PCB =>
					null;
					
-- 					-- package/footprint
-- 					write_placeholder_properties (
-- 						placeholder		=> type_units_schematic.element (unit).packge,
-- 						log_threshold	=> log_threshold + 1);
-- 
-- 					-- datasheet
-- 					write_placeholder_properties (
-- 						placeholder		=> type_units_schematic.element (unit).datasheet,
-- 						log_threshold	=> log_threshold + 1);

				when others => null;
			end case;

		log_indentation_down;
		log_indentation_down;
		log_indentation_down;		
	end write_unit_properties;

	function units_of_component (component_cursor : in type_components_schematic.cursor) return type_units_schematic.map is
	-- Returns the units of the given component.
		u : type_units_schematic.map;

		procedure locate (
			name		: in type_device_name;
			component	: in type_component_schematic) is
		begin
			-- copy the units of the component to the return value
			u := component.units;
		end locate;
		
	begin
		-- locate the given component by component_cursor
		type_components_schematic.query_element (component_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return u;
	end units_of_component;

	procedure check_prefix_characters (
		prefix 		: in type_prefix.bounded_string;
		characters	: in character_set) is
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use et_devices.type_prefix;
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
				prefix 		=> type_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : type_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (ERROR, latin_1.lf & "invalid component reference " & enclose_in_quotes (text_in_justified),
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_devices.type_prefix;
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


	
	function component_reference (cursor : in type_components_schematic.cursor) 
		return type_device_name is
	-- Returns the component reference where cursor points to.
	begin
		return type_components_schematic.key (cursor);
	end component_reference;


	procedure write_component_properties (
	-- Writes the properties of the component indicated by the given cursor.
		component 		: in type_components_schematic.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_symbols;

	begin
		-- reference (serves as key in list of components)
		log (text => "component " & to_string (type_components_schematic.key (component)) & " properties",
			 level => log_threshold);

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log (text => "name in library "
			 & to_string (type_components_schematic.element (component).generic_name), 
			level => log_threshold);
		
		-- value
		log (text => "value "
			& to_string (type_components_schematic.element (component).value), level => log_threshold);

		-- appearance
		log (text => to_string (type_components_schematic.element (component).appearance, verbose => true),
			 level => log_threshold);

		-- depending on the component appearance there is more to report:
		case type_components_schematic.element(component).appearance is
			when et_symbols.PCB =>

-- 				-- package
-- 				log (text => "package " 
-- 					& to_string (type_components.element (component).packge), level => log_threshold);

				-- datasheet
				log (text => "datasheet "
					 & type_component_datasheet.to_string (type_components_schematic.element (component).datasheet),
					level => log_threshold);

			when others => null; -- CS should never happen as virtual components do not have a package
		end case;

		log_indentation_down;
		
	end write_component_properties;

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
		elsif type_port_name.">" (left.name, right.name) then
			result := true;
			
		else
			result := false;
		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_ports;

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

	procedure validate_prefix (prefix : in type_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_string_processing;
		use type_prefix;
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
		use type_prefix;
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

	function to_string (dir : in type_library_directory.bounded_string) return string is
	begin
		return type_library_directory.to_string (dir);
	end to_string;
	
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
		return et_symbols.type_appearance is

		use et_symbols;
		
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
					comp_app := et_symbols.PCB;
				end if;
				
			when false =>
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value (f (line,10));

				-- Evaluate lca and set comp_app accordingly.
				case lca is
					when N =>
						comp_app := et_symbols.PCB;
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
		use type_prefix;
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


	procedure link_strands (log_threshold : in et_string_processing.type_log_level) is
	-- Links local and global strands to nets (see type_module.nets).

	-- Builds the nets (see type_module.nets) of the current module from its strands (see type_module.strands).
	-- NOTE: This is NOT about generating or exporting a netlist. See package et_netlist instead.
	-- This procdure should be called AFTER netlist generation because some strands may have changed their name.
	-- (Names of strands have changee due to power-out ports connected with them.)

	-- Build the module nets. Build_nets merges the strands which are still independed of
	-- each other. For example a strand named "VCC3V3" exists on submodule A on sheet 2. 
	-- Another strand "VCC3V3" exists on submodule C on sheet 1. They do not "know" each other
	-- and must be merged into a single net.
		use et_string_processing;
		use type_strands;

        net_name : type_net_name.bounded_string;
	
		strand	: type_strands.cursor;
	
		procedure add_net (
		-- Creates a net with the name and the scope (local, global) of the current strand. 
		-- If strand is local, the net name is rendered to a full hierarchic name.
		-- If the net existed already, then strand is appended to the strands of the net.
			mod_name : in type_submodule_name.bounded_string;
			module   : in out et_kicad.pcb.type_module) is

			use type_nets;
			
			net_created : boolean;
			net_cursor : type_nets.cursor;

			procedure add_strand (
				name	: in type_net_name.bounded_string;
				net		: in out type_net) is
			begin
				log (text => "strand of net " & et_general.to_string (name), level => log_threshold + 2);
				
				if net_created then -- net has just been created
					net.scope := element (strand).scope; -- set scope of net
				end if;

				if log_level >= log_threshold + 2 then
					log_indentation_up;
					log (text => "strand at " & to_string (
						position => element (strand).position, scope => et_kicad_coordinates.MODULE));
					log_indentation_down;
				end if;
				
				-- append strand to the net
				net.strands.append (new_item => element (strand));
			end add_strand;

		begin -- add_net
			module.nets.insert (
				key 		=> net_name,
				position	=> net_cursor,
				inserted	=> net_created);

			-- If net created or already there, net_cursor points to the net where the strand is to be added.
			module.nets.update_element (
				position	=> net_cursor,
				process		=> add_strand'access);
		end add_net;

	begin -- link_strands
		log (text => "linking local and global strands to nets ...", level => log_threshold);

		log_indentation_up;

		-- loop in strands of the current module
		strand := first_strand;
		log_indentation_up;
		while strand /= type_strands.no_element loop

            case type_strands.element (strand).scope is
                when LOCAL =>

					-- Output a warning if strand has no name.
					if anonymous (element (strand).name) then
						log (WARNING, "net " & et_general.to_string (element (strand).name) 
							& " at" & to_string (
								position => element (strand).position, scope => et_kicad_coordinates.MODULE)
							& " has no dedicated name !");
					end if;

					-- form the net name depending on scope
					-- For local strands the full hierarchic name of the net must be formed
					-- in order to get something like "driver.GND" :

-- 					net_name := type_net_name.to_bounded_string (
-- 						et_coordinates.to_string (et_coordinates.path (element (strand).coordinates), top_module => false)
-- 							& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
-- 							& et_coordinates.hierarchy_separator & et_schematic.to_string (element (strand).name));

-- 					if ada.directories.base_name (to_string (top_level_schematic)) = to_string (et_coordinates.module (element (strand).coordinates)) then -- CS: make function and use it in procedure write_strands too
-- 						net_name := type_net_name.to_bounded_string (hierarchy_separator 
-- 							& et_schematic.to_string (element (strand).name));
-- 					else
-- 						net_name := type_net_name.to_bounded_string (
-- 							et_coordinates.to_string (et_coordinates.path (element (strand).coordinates))
-- 							& et_coordinates.to_string (et_coordinates.module (element (strand).coordinates))
-- 							& et_coordinates.hierarchy_separator & et_schematic.to_string (element (strand).name));
-- 					end if;

					-- if strand is in top module form a net name like "/MASTER_RESET"
					if type_path_to_submodule.is_empty (path (element (strand).position)) then
						net_name := to_net_name (
							et_coordinates.hierarchy_separator
							& et_general.to_string (element (strand).name));

					else -- strand is in any submodule. form a net name like "/SENSOR/RESET"
						net_name := to_net_name (
							to_string (path (element (strand).position))
							& et_coordinates.hierarchy_separator 
							& et_general.to_string (element (strand).name));
					end if;
				
                    -- Create net and append strand to module.nets
                    modules.update_element (
                        position	=> module_cursor,
                        process		=> add_net'access);

				when GLOBAL =>
					-- form the net name depending on scope
					net_name := element (strand).name;

                    -- Create net and append strand to module.nets
                    modules.update_element (
                        position	=> module_cursor,
                        process		=> add_net'access);

				when UNKNOWN =>
					log (ERROR, "unknown scope of net !");
					raise constraint_error; -- CS: should never happen as all strands should have a scope by now

				when HIERARCHIC =>
					null; -- CS special threatment
					
			end case;
            
			next (strand);
		end loop;
		log_indentation_down;
		log_indentation_down;
	end link_strands;

	function first_segment (cursor : in type_strands.cursor) return type_net_segments.cursor is
	-- Returns a cursor pointing to the first net segment of the given strand.
		segment_cursor : type_net_segments.cursor;

		procedure set_cursor (
			strand : in type_strand) is
		begin
			segment_cursor := strand.segments.first;
		end set_cursor;

	begin
		type_strands.query_element (
			position	=> cursor,
			process		=> set_cursor'access
			);
		return segment_cursor;
	end first_segment;
	
	function first_net return type_nets.cursor is
	-- Returns a cursor pointing to the first net of the module (indicated by module_cursor).
		cursor : type_nets.cursor;	

		procedure set_cursor (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.nets.first;
		end set_cursor;
	
	begin
		type_modules.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_net;
	
	procedure process_hierarchic_nets (log_threshold : in et_string_processing.type_log_level) is
	-- Looks up strands of hierarchic nets and appends them to the local or global nets (if connected via gui_submodules). 
	-- Hierarchic nets are mere extensions of a global or local net at deeper levels in the design hierarchy. 
	-- So every hierarchic net is connected with a local or global net at a higher level. 
	-- The link between a global or local net and a hierarchic net is the gui_submodule (see spec. of type_hierarchic_sheet). 
	-- IMPORTANT: Gui_submodules and hierarchic nets are virtual components in a graphical GUI. Neither of them exists in reality.
		use et_string_processing;
		use type_nets;
		use type_strands;
		net : type_nets.cursor;

		-- Temparily we collect the hierarchic strands that are to be appended 
		-- (to the net being examined) here. Once the net has been examined completely
		-- we append hierarchic_strands_tmp to the strands of the net.
		hierarchic_strands_tmp : type_strands.list := type_strands.empty_list;
	
		-- This construct returned after examining a gui_submodule for a suitable hierarchic net at a deeper level:
        type type_hierachic_net is record
			available	: boolean := false; -- when false, path and port are without meaning
			path        : type_path_to_submodule.list := type_path_to_submodule.empty_list;	-- the path of the submodule
			name		: type_net_name.bounded_string := to_net_name (""); -- the name of the hierarchic net -- CS: rename to name
        end record;

		function on_segment (
			port 	: in type_hierarchic_sheet_port;
			segment : in type_net_segment_base)
			return boolean is
		-- Returns true if given port sits on given segment.

			-- CS this is a workaround in order to provide a line for function on_line:
			type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
			line : type_line_scratch := (
				start_point	=> type_point (segment.coordinates_start), 
				end_point	=> type_point (segment.coordinates_end));
			
		begin -- on_segment
			return on_line (port.coordinates, line);
		end on_segment;

		function hierarchic_net (segment : in type_net_segments.cursor) return type_hierachic_net is
		-- Tests if the given segment is connected with a hierarchic net via a gui_submodule.
		-- When positive: marks the port as "processed" and returns a type_hierachic_net (see spec above):
		--	- net.available true
		--	- path to submodule where the hierarchic net is
		--	- name of the hierarchic net in the submodule

		-- One submodule after another is loaded. Then its ports are loaded one after another
		-- and tested if they are connected with the given segment.
		
			net : type_hierachic_net;
			use type_modules;

			procedure query_gui_submodules (
				mod_name	: in type_submodule_name.bounded_string;
				module 		: in out type_module) is
				submodule_cursor : type_hierarchic_sheets.cursor := module.hierarchic_sheets.first; -- CS: rename to gui_submodule_cursor
				use type_hierarchic_sheets;

				procedure query_ports (
				-- Tests if the "port" of the given gui_submodule is connected with the given net segment.
				-- If connected, the path of the gui_submodule and the submodule_name form the path to the real submodule. This
				-- path is subsequently returned. The query ends.
					submodule_name	: in type_hierarchic_sheet_name; -- incl. file and sheet name
					gui_submodule	: in out type_hierarchic_sheet -- This is the gui_submodule being queried.
					) is
					-- These are the "ports" of the gui_submodule (they represent the hierarchic nets within the real submodule).
					port : type_hierarchic_sheet_ports.cursor := gui_submodule.ports.first; -- default to first port
					use type_hierarchic_sheet_ports;
					use type_net_segments;

					procedure mark_processed (
						name : in type_net_name.bounded_string;
						port : in out type_hierarchic_sheet_port) is
					begin
						port.processed := true;
					end mark_processed;

					function append_submodule_to_path (
					-- This function appends the name of a submodule to a path.
					-- Required to form the full path to the submodule.
						path_in		: in type_path_to_submodule.list;
						submodule	: in type_hierarchic_sheet_name)
						return type_path_to_submodule.list is
						path_out : type_path_to_submodule.list := path_in;
					begin
						type_path_to_submodule.append (
							container	=> path_out,
							new_item	=> submodule.name);
						return path_out;
					end append_submodule_to_path;
					
				begin -- query_ports of the given gui_submodule. Test only the non-processed ones.
					-- If "port" sits on given segment, mark the "port" as processed.
					-- NOTE: The "processed" mark prevents multiple testing of the same "port" (which could lead to a forever-loop)
					while port /= type_hierarchic_sheet_ports.no_element loop

						-- we are interested in non-processed ports only
						if not element (port).processed then

							-- if segment is connected with port
							if on_segment (element (port), type_net_segment_base (element (segment))) then

								-- mark port as processed
								update_element (
									container	=> gui_submodule.ports,
									position	=> port,
									process		=> mark_processed'access);

								-- form the return value
								net := (
									available	=> true, -- means: there is a subordinated hierarchical net available
										   
									-- Form the path of the real submodule (gui_submodule path + submodule name):
									path		=> append_submodule_to_path (path (gui_submodule.coordinates), submodule_name), 
												-- example /core/LEVEL_SHIFTER

									-- The name of the subordinated hierarchical net:
									name		=> to_net_name (et_general.to_string (key (port))));

								-- prematurely exit as the return is ready now
								exit;
							end if;

						end if;

						next (port);
					end loop;
				end query_ports;

			begin -- query_gui_submodules
				-- Query gui_submodules. For each gui_submodule query its "ports".
				-- These "ports" are virtual and tell the name of the subordinated hierarchic net.
				while submodule_cursor /= type_hierarchic_sheets.no_element loop

					update_element (
						container	=> module.hierarchic_sheets,
						position	=> submodule_cursor,
						process 	=> query_ports'access);

					-- Once a hierarchic net has been found, the job is done.
					if net.available then exit; end if;
					
					next (submodule_cursor);
				end loop;
			end query_gui_submodules;
			
		begin -- hierarchic_net

			-- Locate the module as indicated by module_cursor. Then query the gui_submodules.
			update_element (
				container	=> modules,
				position	=> module_cursor,
				process		=> query_gui_submodules'access);
			
			return net;
		end hierarchic_net;


		procedure collect_hierarchic_strands (
			net				: in type_hierachic_net;
			log_threshold	: in et_string_processing.type_log_level) is
		-- Locates hierarchic strands as specified by given hierarchic net.
		-- "net" provides the "available" flag. If false, this procedure does nothing.
		-- "net" provides the path to the submodule to search in.
		-- "net" provides the net name to search for.

		-- IMPORTANT: As a hierarchic strand may have other subordinated hierarchic strands (via gui_submodule)
		-- the search may decend indefinitely into the hierarchy.

		-- The hierarchic strands found, are collected in the temparily collector hierarchic_strands_tmp.
		
			-- Cursor h_strand points to the hierarchic strand being examined.
			-- Defaults to the first strand of the module (indicated by module_cursor):
			h_strand : type_strands.cursor := first_strand;
			use type_strands;
			use type_path_to_submodule;
			use type_submodule_name;

			-- This flag goes true once the given net has been found in the submodule.
			-- It serves to warn the operator about a missing hierarchic net.
			hierarchic_net_found : boolean := false;

			procedure query_segments (
			-- Tests if the given hierarchic strand is connected to any hierarchical nets.
				h_strand	: in type_strand -- the hierachic strand being examined
				) is
				-- The cursor that points to the segment being examined.
				-- Defaults to the first segment of h_strand:
				segment : type_net_segments.cursor := h_strand.segments.first;
				use type_net_segments;

				-- If a hierarchic net is available, it will be loaded here temparily.
				h_net : type_hierachic_net;
			begin
				-- Test segment if it is connected to a hierarchic net (via gui_submodules):
				while segment /= type_net_segments.no_element loop

					-- Test if any hierarchic nets are connected (via gui_submodules):
					h_net := hierarchic_net (segment);
					-- h_net may contain a suitable hierarchic net
					
					-- Append all hierarchic strands (if any) to the net being built
					-- (see top level code of procedure process_hierarchic_nets. 
					-- The net being built is indicated by cursor "net").
					collect_hierarchic_strands (h_net, log_threshold);

					-- If one hierarchic net has been detected, there could be more. 
					-- This loop goes on until no more hierarchic nets are available.
					while h_net.available loop
						h_net := hierarchic_net (segment);
						collect_hierarchic_strands (h_net, log_threshold);
					end loop;
					
					next (segment);
				end loop;
			end query_segments;

		begin -- collect_hierarchic_strands

			-- If a hierarchic net is available, query all hierarchic strands of the module.
			-- We have a match if the path of the given hierarchic net equals the
			-- path of the h_strand AND
			-- if the name of the given hierarchic net equals the name of the h_strand.
			if net.available then
				log_indentation_up;

				log (text => "probing hierarchic net " & et_general.to_string (net.name) 
						& " in sheet " & to_string (net.path) & " ...",
					level => log_threshold + 2);
				
				while h_strand /= type_strands.no_element loop
					--if et_schematic."=" (element (h_strand).scope, et_schematic.hierarchic) then
					if element (h_strand).scope = HIERARCHIC then
						if path (element (h_strand).position) = net.path then
							if element (h_strand).name = net.name then
								hierarchic_net_found := true;

								log (text => "reaches down into sheet " 
									& to_string (net.path) 
									& " as net " & et_general.to_string (net.name),
									level => log_threshold + 1
									);

								-- append the strand to the temparily collection of hierarchic strands
								type_strands.append (
									container	=> hierarchic_strands_tmp,
									new_item	=> element (h_strand));

								-- Test if hierarchic h_strand itself is connected to any gui_submodules.
								-- So we query the segments of h_strand for any hierarchic strands connected.
								query_element (
									position 	=> h_strand,
									process 	=> query_segments'access);

							end if;
						end if;
					end if;
					next (h_strand);
				end loop;

				-- Raise warning if hierarchic net not found in submodule:
				if not hierarchic_net_found then
					log (WARNING, "hierarchic net " & et_general.to_string (net.name) 
						& " in submodule " & to_string (net.path) 
						& " not found ! "
						& "Hierarchic sheet in parent module requires this net !");
				end if;
				
				log_indentation_down;
			end if;
		end collect_hierarchic_strands;
		
		procedure query_strands (
		-- Looks for any hierarchic nets connected via gui_submodules with the given net.
			net_name : in type_net_name.bounded_string; -- the name of the net being examined
			net      : in type_net -- the net being examined
			) is
			use type_strands;
			-- The cursor pointing to the strand of the net. Defaults to the first strand.
			strand : type_strands.cursor := net.strands.first; 

			procedure query_segments (
			-- Looks for any hierarchic nets connected via gui_submodules with the given net.
				strand   : in type_strand -- the strand being examined
				) is 
				-- The cursor pointing to the segment of the strand. Defaults to the first segment.
				use type_net_segments;
				segment : type_net_segments.cursor := strand.segments.first;

				-- If a hierarchic net is available, it will be loaded here temparily.
				h_net : type_hierachic_net;
			begin
				-- Load one segment after another and test if the segment
				-- is connected with any hierarchic nets (at deeper levels in the design hierarchy).
				while segment /= type_net_segments.no_element loop

					-- Test if any hierarchic nets are connected (via gui_submodules):
					h_net := hierarchic_net (segment);
					-- h_net may contain a suitable hierarchic net
					
					-- Append all hierarchic strands (if any) to the net being built
					-- (see top level code of procedure process_hierarchic_nets. 
					-- The net being built is indicated by cursor "net").
					collect_hierarchic_strands (h_net, log_threshold);

					-- If one hierarchic net has been detected, there could be more. 
					-- This loop goes on until no more hierarchic nets are available.
					while h_net.available loop
						h_net := hierarchic_net (segment);
						collect_hierarchic_strands (h_net, log_threshold);
					end loop;
					
					next (segment);
				end loop;
			end query_segments;

		begin -- query_strands
			-- Load one strand after another. Then query its segments.
			while strand /= type_strands.no_element loop
				query_element (
					position	=> strand,
					process		=> query_segments'access);

				next (strand);
			end loop;
		end query_strands;

		procedure append_hierarchic_strands (
			--net_name : in type_net_name.bounded_string;
			net_cursor	: in type_nets.cursor;
			strands	 	: in type_strands.list
			) is
			use type_modules;

			procedure locate_net (
				module_name	: in type_submodule_name.bounded_string;
				module		: in out type_module
				) is
				use type_nets;
				--net_cursor : type_nets.cursor;

				procedure append_strands (
					net_name	: in type_net_name.bounded_string;
					net			: in out type_net
					) is
					use type_strands;
				begin
					splice (
						target => net.strands,
						before => type_strands.no_element,
						source => hierarchic_strands_tmp);
				end append_strands;

			begin -- locate_net
				type_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> append_strands'access);
				
			end locate_net;
			
		begin -- append_hierarchic_strands
			-- locate module as indicated by module_cursor
			update_element (
				container	=> modules,
				position	=> module_cursor,
				process		=> locate_net'access);
		end append_hierarchic_strands;
			
    begin -- process_hierarchic_nets
		log (text => "linking hierarchic strands to nets ...", level => log_threshold);

		-- Load one net after another. 
		-- NOTE: The nets of the module are either local or global (see spec type_net_scope).
		-- Then query the strands of the net.
		net := first_net;
		log_indentation_up;
		while net /= type_nets.no_element loop
			log (text => "net " & et_general.to_string (key (net)), level => log_threshold + 1);

			-- Examine the global or local net for any hierarchical nets connected to it.
			-- If there are any, they are collected in hierarchic_strands_tmp.
			query_element (
				position	=> net,
				process		=> query_strands'access);

			-- What we have collected in hierarchic_strands_tmp is now appended to the net.
			append_hierarchic_strands (
				--net_name => key (net),
				net_cursor	=> net,
				strands		=> hierarchic_strands_tmp); 
				-- NOTE: clears hierarchic_strands_tmp by its own
				-- in order to provide a clean collector for the next net.

			next (net);
		end loop;
		log_indentation_down;

	end process_hierarchic_nets;


	procedure write_nets (log_threshold : in et_string_processing.type_log_level) is
	-- Writes a nice overview of all nets, strands, segments and labels.
	-- Bases on the element "nets" of the modules. See specification of type_module.
		use et_string_processing;
	
		procedure query_label (
			segment : in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			if log_level >= log_threshold + 3 then
				
				log_indentation_up;
				while label_simple /= type_simple_labels.no_element loop
					--log (text => "simple label at " & to_string (position => element (label_simple).coordinates, scope => xy));
					log (text => "simple label at " & to_string (element (label_simple).coordinates));
					next (label_simple);
				end loop;

				while label_tag /= type_tag_labels.no_element loop
					if element (label_tag).hierarchic then
						--log (text => "hierarchic label at " 
							   --	& to_string (position => element (label_tag).coordinates, scope => xy));
						log (text => "hierarchic label at " & to_string (element (label_tag).coordinates));
					end if;

					if element (label_tag).global then
						--log (text => "global label at " 
							   --	& to_string (position => element (label_tag).coordinates, scope => xy));
						log (text => "global label at " & to_string (element (label_tag).coordinates));
					end if;
					
					next (label_tag);
				end loop;
				
				log_indentation_down;
			end if;
		end query_label;
		
		procedure query_segment (
			strand : in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;
			
			-- for the segment we provide a consequtive number which has no further meaning
			segment_number : count_type := 1;			
		begin
			if log_level >= log_threshold + 2 then
				log_indentation_up;
				while segment /= type_net_segments.no_element loop
					log (text => "segment #" 
						& count_type'image (segment_number) 
						& latin_1.space
						& to_string (
							segment	=> element (segment), 
							scope	=> XY));

					query_element (
						position	=> segment,
						process		=> query_label'access);
					
					segment_number := segment_number + 1;
					next (segment);
				end loop;
				log_indentation_down;
			end if;
		end query_segment;
		
		procedure query_strand (
			net_name 	: in type_net_name.bounded_string;
			net 		: in type_net) is
			
			strand : type_strands.cursor := net.strands.first;
			use type_strands;
			
			-- for the strand we provide a consequtive number which has no further meaning
			strand_number : count_type := 1;			
		begin -- query_strand
			if log_level >= log_threshold + 1 then
				log_indentation_up;
				while strand /= type_strands.no_element loop
					log (text => "strand #" & trim (count_type'image (strand_number), left) &
						 " at" & to_string (
							position => element (strand).position, scope => et_kicad_coordinates.MODULE)
						);

					query_element (
						position	=> strand,
						process		=> query_segment'access);
					
					strand_number := strand_number + 1;
					next (strand);
				end loop;
				log_indentation_down;
			end if;
		end query_strand;
		
		procedure query_net (
			mod_name	: in type_submodule_name.bounded_string;
			module 		: in type_module) is
			net : type_nets.cursor := module.nets.first;
			use type_nets;
		begin
			log_indentation_up;
			while net /= type_nets.no_element loop
				log (text => "net " & et_general.to_string (key (net)));

				query_element (
					position	=> net,
					process		=> query_strand'access);
				
				next (net);
			end loop;

			log_indentation_down;
		end query_net;

		use type_modules;
		
	begin -- write_nets
		if log_level >= log_threshold then
			log (text => "net report");
			log_indentation_up;
				
			--first_module;
			--while module_cursor /= type_modules.no_element loop
					
			--	log (text => "module " & to_string (key (module_cursor)));

				query_element (
					position	=> module_cursor,
					process		=> query_net'access);
				
			--	next (module_cursor);
			--end loop;

			log_indentation_down;
		end if;
	end write_nets;

	-- Converts the rotaton of a label or a text to a relative rotation.
	function to_relative_rotation (text_in : in string) 
		return et_coordinates.type_rotation_relative is
	-- CS: use a dedicated type for input parameter.
		o_in	: type_label_orientation := type_label_orientation'value (text_in);
		o_out	: et_coordinates.type_rotation_relative;
	begin
		case o_in is
			when 0 => o_out := 180.0;
			when 1 => o_out :=  90.0;
			when 2 => o_out :=   0.0;
			when 3 => o_out := -90.0;
		end case;
		return o_out;
		-- CS: exception handler
	end to_relative_rotation;

	function to_direction (text_in : in string) 
		return et_schematic.type_net_label_direction is
	-- Converts the direction of a label to a type_label_direction. 
	-- CS: currently case sensitive ! Use dedicated type for input parameter.
		use et_schematic;		
		d_out : type_net_label_direction := input;
	begin
		if text_in = schematic_keyword_label_dir_input then
			d_out := INPUT;
		elsif text_in = schematic_keyword_label_dir_output then
			d_out := OUTPUT;
		elsif text_in = schematic_keyword_label_dir_bidir then
			d_out := BIDIR;
		elsif text_in = schematic_keyword_label_dir_tristate then
			d_out := TRISTATE;
		elsif text_in = schematic_keyword_label_dir_passive then
			d_out := PASSIVE;
		else
			log (ERROR, "Label direction unknown !", console => true);
			raise constraint_error;
		end if;
		
		return d_out;
	end to_direction;		


	
	-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
	-- they will be returned in hierarchic_sheet_file_names. Otherwise the returned list is empty.
	function read (
		current_schematic	: in type_hierarchic_sheet_file_name_and_timestamp;
		sheet_number		: in et_coordinates.type_sheet;
		log_threshold		: in type_log_level)
		return type_hierarchic_sheet_file_names_extended is separate;

	
	-- Imports the design libraries and the actual design as specified by parameter "project".
	-- Inserts the created (sub)module in the module collection (see type_modules).
	-- Leaves the global module_cursor pointing where the module was inserted.
	procedure import_design (
		project			: in et_project.pac_project_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		-- backup current working directory
		current_working_directory : constant string := current_directory;
		
		use pac_lines_of_file;
		
		hierarchic_sheet_file_names : type_hierarchic_sheet_file_names_extended;

		current_schematic : type_hierarchic_sheet_file_name_and_timestamp; -- sensor.sch / B7F2F34A

		-- The sheet number is incremented each time a sheet has been read.
		-- NOTE: The sheet number written in the schematic file header (a line like "Sheet 1 7") has no meaning.
		sheet_number : et_coordinates.type_sheet := 1;

		package stack_of_sheet_lists is new et_general.stack_lifo (max => 10, item => type_hierarchic_sheet_file_names_extended);
        use stack_of_sheet_lists;
		
		function read_project_file (log_threshold : in et_string_processing.type_log_level)
			return type_schematic_file_name.bounded_string is
		-- V4:
		--	- Reads the project file (component libraries, library directories, ...) 
		--	- Returns the name of the top level schematic file.
		--	- Creates temparily search_list_component_libraries and search_list_library_dirs.
		--	- Creates empty component/symbol libraries in tmp_component_libraries.

		-- V5:
		--	- Reads the local and global sym-lib-tables and stores them temparily in sym_lib_tables.
		--	- Reads the local and global fp-lib-tables and stores them temparily in fp_lib_tables.
		--	- Creates empty component/symbol libraries in et_kicad_pcb.package_libraries.
			
			line : type_fields_of_line;

			procedure read_proj_v4 is
			-- Reads the project file and stores search lists in module components
			-- search_list_library_dirs and search_list_library_comps. See spec for type_module.
			-- Creates empty component/symbol libraries in tmp_component_libraries.
			-- V5: Creates empty footprint libraries in et_kicad_pcb.package_libraries.
				
				-- "section entered flags"
				section_eeschema_entered 			: boolean := false;
				section_eeschema_libraries_entered	: boolean := false;            
			
				procedure clear_section_entered_flags is
				-- clears section_eeschema_entered and section_eeschema_libraries_entered.
				begin
					section_eeschema_entered := false;
					section_eeschema_libraries_entered := false;
				end clear_section_entered_flags;
				
				procedure locate_library_directories (
					directories		: in string;
					log_threshold	: in type_log_level) is
				-- The library directories must be inserted in the project_lib_dirs
				-- The given string is something like "../../lbr;../connectors;../misc_components".
				-- Library directories are separated by semicolon.
				-- Tests if each directory exists -> abort if not.
				-- In search_list_project_lib_dirs the order of appearance of the directories 
				-- is kept (because it is a simple list). 
				-- search_list_project_lib_dirs applies for the single module only and is cleared once a 
				-- kicad project file is read.
				-- search_list_project_lib_dirs assists search operations.
					use type_library_directory;
					directory_count 	: natural;
					lib_dir_separator 	: constant string (1..1) := ";";
					lib_dir_name 		: type_library_directory.bounded_string;
					
				begin -- locate_library_directories
					log (text => "locating library directories ...", level => log_threshold);
					log_indentation_up;
					
					-- If no library directory is specified then issue a warning, otherwise:
					if directories'length > 0 then

						-- If there is no semicolon, there is 1 directory.
						-- If there are two semicolons, there are 2 directories ...
						directory_count := ada.strings.fixed.count (directories, lib_dir_separator) + 1;

						-- extract directory names and create a group for each of them:
						for place in 1..directory_count loop

							-- get the directory name where "place" points to:
							lib_dir_name := to_bounded_string (get_field_from_line (
													text_in 	=> directories,
													position 	=> place,
													ifs 		=> lib_dir_separator (1)));

							log (text => "directory " & to_string (lib_dir_name), level => log_threshold + 1);

							-- Insert the library directory in the project_lib_dirs.
							-- project_lib_dirs is a simple list where the directory names are kept in the
							-- same order as they appear in the project file ("../../lbr;../connectors;../misc_components")
							-- See more in specs of project_lib_dirs in et_kicad.ads.
							if not type_project_lib_dirs.contains (search_list_project_lib_dirs, lib_dir_name) then
								type_project_lib_dirs.append (search_list_project_lib_dirs, lib_dir_name);
							else
								log (WARNING, "multiple usage of directory " & to_string (lib_dir_name));
							end if;

							-- Test if the library directory exists:
							if not exists (to_string (lib_dir_name)) then
								log (ERROR, "directory " & to_string (lib_dir_name) & " does not exist !", console => true);
								raise constraint_error;
							end if;
								
						end loop;
					else
						log (WARNING, "no directory for libraries specified !");
					end if;

					log_indentation_down;
				end locate_library_directories;

				
				procedure locate_libraries (log_threshold : in type_log_level) is
				-- Tests if the libraries (listed in search_list_component_libraries) exist in the
				-- directories listed in search_list_project_lib_dirs.
				-- If a library was found, a same-named empty library is created in the container tmp_component_libraries.
					use type_library_names;
					search_list_library_cursor : type_library_names.cursor;
					library_found		: boolean; -- true if library file exists
				
					use type_project_lib_dirs;
					search_list_lib_dir_cursor : type_project_lib_dirs.cursor;
				
				begin -- locate_libraries
					log (text => "locating library directories ...", level => log_threshold);
					log_indentation_up;

					-- loop in project_libraries (specified in the kicad project file)
					search_list_library_cursor := search_list_component_libraries.first;
					while search_list_library_cursor /= type_library_names.no_element loop

						-- library_cursor points to the current library
						log (text => "library " & to_string (element (search_list_library_cursor)), level => log_threshold + 1);
						log_indentation_up;
						
						-- This flag goes true once the library has been found at file system level
						library_found := false;
					
						-- Loop in directories (specified in the kicad project file by LibDir=../../lbr_dir_1;../lbr_dir_2).
						search_list_lib_dir_cursor := search_list_project_lib_dirs.first;
						while search_list_lib_dir_cursor /= type_project_lib_dirs.no_element loop

							log (text => "searching in " & to_string (element (search_list_lib_dir_cursor)), -- ../../lbr_dir_1; ../../lbr_dir_2; ...
								level => log_threshold + 3); 
							
							-- Test at file system level, whether the current project library exists
							-- in the directory indicated by search_list_lib_dir_cursor.
							-- If exists, create an empty library (with a full name) in tmp_component_libraries.
							if exists (compose (
								containing_directory	=> to_string (element (search_list_lib_dir_cursor)), -- ../../lbr_dir_1
								name					=> to_string (element (search_list_library_cursor)), -- connectors, active, ...
								extension				=> file_extension_schematic_lib)) 
								then
									log (text => " found", level => log_threshold + 3);
									library_found := true;

									-- create empty component library
									type_device_libraries.insert (
										container	=> tmp_component_libraries,
										key 		=> type_device_library_name.to_bounded_string (compose (
											containing_directory	=> to_string (element (search_list_lib_dir_cursor)), -- ../../lbr
											name					=> to_string (element (search_list_library_cursor)), -- connectors, active, ...
											extension				=> file_extension_schematic_lib)),
										new_item	=> type_components_library.empty_map
										--inserted	=> library_inserted,
										--position	=> library_cursor
										); 
									
							end if;

							next (search_list_lib_dir_cursor);
						end loop;

						-- raise alarm and abort if current library not found in any directory
						if not library_found then
							log (ERROR, "library " & to_string (element (search_list_library_cursor)) &
								" not found in any directory !", console => true);
							raise constraint_error;
						end if;

						log_indentation_down;
						next (search_list_library_cursor);
					end loop;

					log_indentation_down;
				end locate_libraries;

				project_file_handle : ada.text_io.file_type;
				
			begin -- read_proj_v4
				log (
					text => "V4 project file is " 
					& enclose_in_quotes (compose (
						name		=> base_name (et_project.pac_project_name.to_string (project)), 
						extension	=> file_extension_project)) & " ...",
					level => log_threshold + 1
					);
				log_indentation_up;

				-- Clear search list of project libraries from earlier projects that have been imported.
				-- If we import only one project, this statement does not matter:
				type_project_lib_dirs.clear (search_list_project_lib_dirs);
				type_library_names.clear (search_list_component_libraries);

				-- Open project file. 
				-- The file name is composed of project name and extension.
				open (
					file => project_file_handle,
					mode => in_file,
					name => compose (
								name		=> base_name (et_project.pac_project_name.to_string (project)), 
								extension	=> file_extension_project)
					);
				set_input (project_file_handle);
				
				while not end_of_file loop

					-- Save a line in variable "line" (see et_string_processing.ads)
					line := read_line(
								line => get_line,
								comment_mark => "#", -- use constant comment_mark
								number => ada.text_io.line (current_input),
								ifs => latin_1.equals_sign); -- fields are separated by equals sign (=)

					case field_count (line) is
						when 0 => null; -- we skip empty lines
						when 1 => -- we have a line with just one field. those lines contain headers like "[eeschema]"

							-- test header [eeschema]
							if f (line,1) = project_header_eeschema then
								clear_section_entered_flags;
								section_eeschema_entered := true;
							end if;

							-- test header [eeschema/libraries]
							if f (line,1) = project_header_eeschema_libraries then
								clear_section_entered_flags;
								section_eeschema_libraries_entered := true;
							end if;

						when 2 =>
							if section_eeschema_entered then

								-- Get library directory names 
								if f (line,1) = project_keyword_library_directory then
									log (text => "library directories are: " & f (line,2), level => log_threshold + 2);

									-- The library directories must be
									-- inserted in the search list of library directories (search_list_project_lib_dirs).
									-- These directories assist search operations for both components and packages.
									locate_library_directories (f (line,2), log_threshold + 3);
								end if;
								
							end if;

							if section_eeschema_libraries_entered then

								-- From a line like "LibName1=bel_supply" get component library names 
								-- (incl. path and extension) and
								-- store them in search_list_component_libraries (see et_kicad.ads).
								-- We ignore the index of LibName. Since we store the lib names in a 
								-- simple list their order remains unchanged anyway.
								if f (line,1)(1..project_keyword_library_name'length) 
									= project_keyword_library_name then

									-- The component library could have been referenced already. If so,
									-- there is no need to append it again to search_list_component_libraries.
									if not type_library_names.contains (
										container 	=> search_list_component_libraries,
										item		=> type_library_name.to_bounded_string (f (line,2))) then
										
											type_library_names.append (
												container	=> search_list_component_libraries, 
												new_item	=> type_library_name.to_bounded_string (f (line,2)));

											-- NOTE: search_list_component_libraries keeps the libraries in the same order as they appear
											-- in the project file. search_list_component_libraries assists search operations.
											-- It applies for the current project only and
											-- is cleared as soon as another kicad project file is read.
											
											-- For the log write something like "LibName bel_connectors_and_jumpers"
											log (text => f (line,1) & " " & f (line,2), level => log_threshold + 2);
									end if;

								end if;

							end if;
							
						when others => null;
					end case;

	-- 				if section_eeschema_entered or section_eeschema_libraries_entered then
	-- 					put_line(" " & et_string_processing.to_string(line));
	-- 				end if;
					
				end loop;

				-- Test if the libraries collected in search_list_component_libraries
				-- exist in any of the library directories.
				-- Create empty component libraries.
				locate_libraries (log_threshold + 3);

				close (project_file_handle);

				log (text => "V4 project file reading done", level => log_threshold + 1);
			end read_proj_v4;
			
			procedure read_lib_tables (log_threshold : in et_string_processing.type_log_level) is
			-- Reads local and global sym-lib-tables and stores them in module component sym_lib_tables.
			-- Reads local and global fp-lib-tables and stores them in module component fp_lib_tables.
			-- See spec for type_module.
			-- IMPORTANT AND CS: This procedure currently works under linux only !
				use ada.environment_variables;
				use ada.directories;

			    table_path_length_max : constant natural := 200;
				package type_lib_table_path is new generic_bounded_length (table_path_length_max);
				use type_lib_table_path;
				lib_table_path : type_lib_table_path.bounded_string; -- stores the path and name of a sym-lib-table or fp-lib-table file 
				lib_table_handle : ada.text_io.file_type;

				-- After reading the local and global sym-lib-tables they are stored here:
				sym_table_local, sym_table_global : type_lib_table.list;

				-- After reading the local and global fp-lib-tables they are stored here:
				fp_table_local, fp_table_global : type_lib_table.list;
				
				procedure locate_component_libraries is
				-- Tests if the libraries (listed in sym_lib_table) exist.
				-- If a library was found, a same-named empty library is created in the container tmp_component_libraries.
					lib_cursor : type_lib_table.cursor := sym_lib_tables.first;
					use type_lib_table;
					use et_devices;
					uri : type_device_library_name.bounded_string;
				begin
					log (text => "locating libraries ...", level => log_threshold + 1);
					log_indentation_up;

					while lib_cursor /= type_lib_table.no_element loop
						uri := element (lib_cursor).lib_uri; -- get full name like /home/user/kicad_libs/bel_stm32.lib
						log (text => to_string (uri), level => log_threshold + 2);

						-- Test if library file exists:
						if ada.directories.exists (to_string (uri)) then

							-- create empty component library
							type_device_libraries.insert (
								container	=> tmp_component_libraries,
								key 		=> uri,
								new_item	=> type_components_library.empty_map
								); 

							-- CS library type, options and description not processed here.
							-- See comment on type_libraries in et_kicad.ads.
							
						-- raise alarm and abort if library file not found
						else
							log (ERROR, "library " & to_string (uri) 
								 & " not found !", console => true);
							raise constraint_error;
						end if;

						next (lib_cursor);
					end loop;
					log_indentation_down;
				end locate_component_libraries;

				procedure locate_package_libraries is
				-- Tests if the libraries (listed in fp_lib_table) exist.
				-- If a library was found, a same-named empty library is created in the container et_kicad_pcb.package_libraries.
					lib_cursor : type_lib_table.cursor := fp_lib_tables.first;
					use type_lib_table;

					uri : type_device_library_name.bounded_string; 
					-- CS: not really correct. see spec for type_lib_table_entry

					use et_kicad_packages;
				begin
					log (text => "locating libraries ...", level => log_threshold + 1);
					log_indentation_up;

					while lib_cursor /= type_lib_table.no_element loop
						uri := element (lib_cursor).lib_uri; -- get full name like /home/user/kicad_libs/bel_ic.pretty
						log (text => et_devices.to_string (uri), level => log_threshold + 2);

						-- Test if library file exists:
						if ada.directories.exists (et_devices.to_string (uri)) then

							-- create empty package/footprint library
							type_libraries.insert (
								container	=> package_libraries,
								key 		=> et_packages.to_file_name (et_devices.to_string (uri)),
								new_item	=> type_packages_library.empty_map
								); 

							-- CS library type, options and description not processed here.
							-- See comment on type_package_libraries in et_kicad_pcb.ads.
							
						-- raise alarm and abort if library file not found
						else
							log (ERROR, "library " & et_devices.to_string (uri) 
								 & " not found !", console => true);
							raise constraint_error;
						end if;

						next (lib_cursor);
					end loop;
					log_indentation_down;
				end locate_package_libraries;

				
				function read_table return type_lib_table.list is
				-- Reads the file that contains a sym-lib-table or fp-lib-table. The current_input points to the particular file.
				-- The file is read into container "lines" which is then parsed to obtain the table content.
					table : type_lib_table.list; -- to be returned

					line : et_string_processing.type_fields_of_line; -- a line of the table
					lines : pac_lines_of_file.list; -- all lines of the table

					-- This cursor points to the line being processed (in the list of lines given in "lines"):
					line_cursor : pac_lines_of_file.cursor;
					
					opening_bracket : constant character := '(';
					closing_bracket : constant character := ')';

					term_char_seq : constant string (1..2) := latin_1.space & closing_bracket;
					term_char_set : character_set := to_set (term_char_seq);
					
					-- the section prefix is a workaround due to GNAT reserved keywords.
					sec_prefix : constant string (1..4) := "sec_";

					-- These are the keywords used in the sym-lib tables:
					type type_keyword is (
						INIT,
						SEC_SYM_LIB_TABLE,
						SEC_FP_LIB_TABLE,
						SEC_LIB,
						SEC_NAME,
						SEC_TYPE,
						SEC_URI,
						SEC_OPTIONS,
						SEC_DESCR
						);

					argument_length_max : constant positive := 300; -- CS: could become an issue if long URIs used ...
					package type_argument is new generic_bounded_length (argument_length_max);

					-- After a section name, arguments follow. For each section arguments are counted:
					type type_argument_counter is range 0..1;

					function to_string (arg_count : in type_argument_counter) return string is begin
					-- Returns the given argument count as string.
						return trim (type_argument_counter'image (arg_count), left);
					end to_string;			

					-- Type contains the current section name, the parent section name and the pointer to the argument.
					-- The argument counter is reset on entering a section.
					-- It is incremented once an argument is complete.
					type type_section is record
						name 		: type_keyword := INIT;
						parent		: type_keyword := INIT;
						arg_counter	: type_argument_counter := type_argument_counter'first;
					end record;

					section : type_section; -- the section being processed

					-- Since there are numerous subsections we store sections on a stack.
					-- Once a subsection as been entered the previous section is pushed 
					-- on stack (see procedure read_section).
					-- One leaving a subsection the previous section is popped 
					-- from stack (see end of procedure exec_section).
					package sections_stack is new et_general.stack_lifo (max => 20, item => type_section);

					function to_string (section : in type_keyword) return string is
					-- Converts a section name to a string.
						len : positive := type_keyword'image (section)'last;
					begin
						-- Due to the workaround with the SEC_ prefix (see above), it must be removed from
						-- the section image.
						return to_lower (type_keyword'image (section)(sec_prefix'last+1 ..len));
						--return type_keyword'image (section);
					end to_string;
				
					function enter_section (section : in type_keyword) return string is begin
						return ("entering section " & to_string (section));
					end enter_section;

					function return_to_section (section : in type_keyword) return string is begin
						return ("returning to section " & to_string (section));
					end return_to_section;

					function process_section (section : in type_keyword) return string is begin
						return ("processing section " & to_string (section));
					end process_section;

					-- TEMPORARILY STORAGE PLACES

					lib_name	: et_kicad_general.type_library_name.bounded_string;
					lib_type	: type_lib_type;
					lib_uri		: et_kicad_general.type_device_library_name.bounded_string; -- CS not exact. see specs of type type_lib_table_entry
					-- CS lib_options
					-- CS lib_description

					-- When a line is fetched from the container "lines", it is stored in variable
					-- "current_line". CS: The line length is limited by line_length_max and should be increased
					-- if neccessary. 
					-- The character_cursor points to the character being tested or processed in that line.
					line_length_max : constant positive := 1000;
					package type_current_line is new generic_bounded_length (line_length_max);
					use type_current_line;
					current_line : type_current_line.bounded_string;
					character_cursor : natural;

					procedure get_next_line is begin
					-- Fetches a new line from the container "lines".
						next (line_cursor);
						if line_cursor /= pac_lines_of_file.no_element then

							-- Since a single line in container "lines" (where line_cursor points to) is a list 
							-- of strings itself, we convert them first to a fixed string and then to a bounded string.
							current_line := type_current_line.to_bounded_string (to_string (element (line_cursor)));
							--log (text => "line " & to_string (current_line), level => log_threshold);
						else
							-- This should never happen:
							log (ERROR, "in " & to_string (lib_table_path), console => true);
							log (ERROR, "no more lines available !", console => true);
							raise constraint_error;
						end if;
					end get_next_line;
								
					procedure next_character is
					-- Updates the cursor position to the position of the next
					-- non_space character starting from the current cursor position.
					-- Fetches a new line if no further characters after current cursor position.
					begin
						character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
						while character_cursor = 0 loop
							get_next_line;
							character_cursor := index_non_blank (source => current_line, from => character_cursor + 1);
						end loop;
					end next_character;
					
					procedure read_section is 
					-- Stores the section name and current argument counter on sections_stack.
					-- Reads the section name from current cursor position until termination
					-- character or its last character.
						end_of_kw : integer;  -- may become negative if no terminating character present

						procedure invalid_section is begin
							log (ERROR, "invalid subsection '" & to_string (section.name) 
								& "' in parent section '" & to_string (section.parent) & "' ! (read section)", console => true);
							raise constraint_error;
						end invalid_section;

					begin -- read_section
						-- save previous section on stack
						sections_stack.push (section);

						-- the former actvie section name becomes the parent section name
						section.parent := section.name;

						-- CS provide log info on current section
						--log ("section " & to_string (section.name), level => log_threshold + 1);
						
						section.arg_counter := 0;
						
						-- get position of last character
						end_of_kw := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

						-- if no terminating character found, end_of_kw assumes length of line
						if end_of_kw = -1 then
							end_of_kw := length (current_line);
						end if;

						-- Compose section name from cursor..end_of_kw.
						-- This is an implicit general test whether the keyword is a valid keyword.
						section.name := type_keyword'value (sec_prefix & slice (current_line, character_cursor, end_of_kw));
						
						-- This is the validation of a section regarding its parent section.
						-- If an invalid subsection occurs, raise alarm and abort.
						case section.parent is
							when SEC_SYM_LIB_TABLE | SEC_FP_LIB_TABLE =>
								case section.name is
									when SEC_LIB => null;
									when others => invalid_section;
								end case;

							when SEC_LIB =>
								case section.name is
									when SEC_NAME | SEC_TYPE | SEC_URI | SEC_OPTIONS | SEC_DESCR => null;
									when others => invalid_section;
								end case;

							when others => null;
						end case;

						-- update cursor
						character_cursor := end_of_kw;

						log (text => enter_section (section.name), level => log_threshold + 5);

						exception
							when event:
								others =>
									log (ERROR, "in " & to_string (lib_table_path), console => true);
									log (ERROR, affected_line (element (line_cursor)) 
										& to_string (element (line_cursor)), console => true);

									log (ERROR, "section '" & slice (current_line, character_cursor, end_of_kw) 
										& "' invalid or not supported yet", console => true);
									raise;

						
					end read_section;
					
					procedure read_arg is
					-- Reads the arguments of a section.
					-- Increments the argument counter after each argument.
					-- Validates the arguments according to the current section.
					-- Leaves the character_cursor at the position of the last character of the argument.
					-- If the argument was enclosed in quotations the character_cursor is left at
					-- the position of the trailing quotation.
						end_of_arg : integer; -- may become negative if no terminating character present

						use type_argument;

						arg : type_argument.bounded_string; -- here the argument goes temporarily

						procedure too_many_arguments is begin
							log (ERROR, "too many arguments in section " & to_string (section.name) & " !", console => true);
							log (text => "excessive argument reads '" & to_string (arg) & "'", console => true);
							raise constraint_error;
						end too_many_arguments;

						procedure invalid_section is begin
							log (ERROR, "invalid subsection '" & to_string (section.name) 
								& "' in parent section '" & to_string (section.parent) & "' ! (read argument)", console => true);
							raise constraint_error;
						end invalid_section;
						
					begin -- read_arg
						-- We handle an argument that is wrapped in quotation different from a non-wrapped argument:
						if element (current_line, character_cursor) = latin_1.quotation then
							-- Read the quotation-wrapped argument (strip quotations)

							-- get position of last character (before trailing quotation)
							end_of_arg := index (source => current_line, from => character_cursor + 1, pattern => 1 * latin_1.quotation) - 1;

							-- if no trailing quotation found -> error
							if end_of_arg = -1 then
								log (ERROR, affected_line (element (line_cursor))
									& latin_1.space & latin_1.quotation & " expected");
									raise constraint_error;
							end if;

							-- compose argument from first character after quotation until end_of_arg
							arg := to_bounded_string (slice (current_line, character_cursor + 1, end_of_arg));

							-- update cursor (to position of trailing quotation)
							character_cursor := end_of_arg + 1;
						else
							-- Read the argument from current cursor position until termination
							-- character or its last character.

							-- get position of last character
							end_of_arg := index (source => current_line, from => character_cursor, set => term_char_set) - 1;

							-- if no terminating character found, end_of_arg assumes length of line
							if end_of_arg = -1 then
								end_of_arg := length (current_line);
							end if;

							-- compose argument from cursor..end_of_arg
							arg := to_bounded_string (slice (current_line, character_cursor, end_of_arg));

							-- update cursor
							character_cursor := end_of_arg;
						end if;

						-- Argument complete. Increment argument counter of section.
						section.arg_counter := section.arg_counter + 1;
						
						log (text => "arg" & to_string (section.arg_counter) & latin_1.space & to_string (arg), level => log_threshold + 5);

						-- Validate arguments according to current section and the parent section.
						-- Load variables. When a section closes, the variables are used to build an object. see exec_section.
						case section.parent is
							when SEC_LIB =>
								case section.name is
									when SEC_NAME =>
										case section.arg_counter is
											when 0 => null;
											when 1 =>
												lib_name := type_library_name.to_bounded_string (to_string (arg));
											when others => too_many_arguments;
										end case;

									when SEC_TYPE =>
										case section.arg_counter is
											when 0 => null;
											when 1 =>
												lib_type := type_lib_type'value (to_string (arg));
											when others => too_many_arguments;
										end case;

									when SEC_URI =>
										case section.arg_counter is
											when 0 => null;
											when 1 =>
												lib_uri := et_devices.to_file_name (to_string (arg));
											when others => too_many_arguments;
										end case;

									when SEC_OPTIONS =>
										case section.arg_counter is
											when 0 => null;
											when 1 =>
												-- CS lib_options := to_bounded_string (to_string (arg));
												null;
											when others => too_many_arguments;
										end case;

									when SEC_DESCR =>
										case section.arg_counter is
											when 0 => null;
											when 1 =>
												-- CS lib_description := to_bounded_string (to_string (arg));
												null;
											when others => too_many_arguments;
										end case;

									when others => invalid_section;
								end case;
								
							when others => null; -- Not all sections require arguments.
						end case;

						exception
							when event:
								others =>
									log (ERROR, "in " & to_string (lib_table_path), console => true);
									log (ERROR, affected_line (element (line_cursor)) 
										& to_string (element (line_cursor)), console => true);
									log (text => ada.exceptions.exception_message (event));
									raise;

					end read_arg;
					

					procedure exec_section is
					-- Performs an operation according to the active section and variables that have been
					-- set earlier (when processing the arguments. see procedure read_arg).
					-- Restores the previous section.
					begin -- exec_section
						log (text => process_section (section.name), level => log_threshold + 3);
						case section.parent is
							when SEC_SYM_LIB_TABLE | SEC_FP_LIB_TABLE =>
								case section.name is

									-- When this section closes, the entry is complete and 
									-- can be appended to the sym-list-table.
									when SEC_LIB =>
										log (text => "library " 
											 & type_library_name.to_string (lib_name)
											 & " type "
											 & type_lib_type'image (lib_type)
											 & " path "
											 -- CS options and description
											 & et_devices.to_string (lib_uri), 
											level => log_threshold + 2); 

										type_lib_table.append (
											container	=> table,
											new_item	=> (
												lib_name	=> lib_name,
												lib_type	=> lib_type,
												lib_uri		=> lib_uri)
												);
										
									when others => null;
								end case;

							when others => null;
						end case;

						-- restore previous section from stack
						section := sections_stack.pop;
						log (text => return_to_section (section.name), level => log_threshold + 5);
						
						exception
							when event:
								others =>
									log (ERROR, "in " & to_string (lib_table_path), console => true);
									log (ERROR, affected_line (element (line_cursor)) 
										& to_string (element (line_cursor)), console => true);
									log (text => ada.exceptions.exception_message (event));
									raise;

					end exec_section;

				begin -- read_table
					log_indentation_up;

					-- Import the file in container "lines"
					set_input (lib_table_handle);
					while not end_of_file loop
						-- log (text => get_line);

						-- Store a single line in variable "line" (see et_string_processing.ads)
						line := et_string_processing.read_line (
								line 			=> get_line,
								test_whole_line	=> false, -- comment marks at begin of line matter
								number 			=> ada.text_io.line (current_input),
								comment_mark	=> comment_mark,
								ifs 			=> latin_1.space); -- fields are separated by space

						-- insert line in container "lines"
						if field_count (line) > 0 then -- we skip empty or commented lines
							lines.append (line);
						end if;
							
					end loop;
					
					set_input (standard_input);
					-- Now the table is available in container "lines" which is a list of lines.
					-- A line in turn is a list of strings.

					-- Set line cursor to first line in container "lines":
					line_cursor := lines.first;

					sections_stack.init;

					--log (text => "test 1 section " & to_string (section.name), level => log_threshold + 1);
					--log (text => "test 1 section " & type_keyword'image (section.name), level => log_threshold + 1);
					
					-- get first line
					current_line := type_current_line.to_bounded_string (to_string (pac_lines_of_file.element (line_cursor)));
					--log (text => "line " & to_string (current_line), level => log_threshold + 4);

					-- get position of first opening bracket
					character_cursor := type_current_line.index (current_line, 1 * opening_bracket);

					-- This is the central loop where decisions are made whether to read a section name,
					-- an argument or whether to "execute" a section.
					-- An opening bracket indicates a new (sub)section. A closing bracket indicates that a section
					-- finishes and is to be executed. The loop comes to an end if the sections stack depth reaches zero.
					loop
						-- read (sub)section
						<<label_read_section>>
							next_character; -- set character cursor to next character
							read_section;
							next_character; -- set character cursor to next character

							-- if a new subsection starts, read subsection
							if element (current_line, character_cursor) = opening_bracket then goto label_read_section; end if;

						-- read argument
						<<label_read_argument>>
							read_arg;
							next_character; -- set character cursor to next character
						
							-- Test for cb, opening_bracket or other character after argument:
							case element (current_line, character_cursor) is

								-- If closing bracket after argument, the (sub)section ends
								-- and must be executed:
								when closing_bracket => goto label_execute_section;

								-- If another section at a deeper level follows,
								-- read (sub)section:
								when opening_bracket => goto label_read_section;

								-- In case another argument follows, it must be read:
								when others => goto label_read_argument; 
							end case;

						-- execute section
						<<label_execute_section>>
							exec_section;

							-- After executing the section, check the stack depth.
							-- Exit when zero reached (topmost section has been executed).
							if sections_stack.depth = 0 then exit; end if;
							
							next_character; -- set character cursor to next character

							-- Test for cb, opening_bracket or other character after closed section:
							case element (current_line, character_cursor) is

								-- If closing bracket after closed section,
								-- execute parent section:
								when closing_bracket => goto label_execute_section;

								-- If another section at a deeper level follows,
								-- read subsection:
								when opening_bracket => goto label_read_section;

								-- In case an argument follows, it belongs to the parent
								-- section and is to be read:
								when others => goto label_read_argument; 
							end case;
							
					end loop;

					-- check section name. must be top level section
					if section.name /= INIT then -- should never happen
						log (ERROR, "in " & to_string (lib_table_path), console => true);
						log (ERROR, "top level section not closed !", console => true);
						raise constraint_error;
					end if;

					log_indentation_down;
					
					return table;
				end read_table;

				procedure warning_on_multiple_entry_in_lib_table (library : in string) is
				begin
					log (WARNING, "global library table: '" & library 
						 & "' already in local library table -> skipped !");
				end warning_on_multiple_entry_in_lib_table;
				
				procedure concatenate_local_and_global_sym_tables is
				-- Concatenates local and global sym-lib-tables so that global libraries come AFTER local libraries.
					use type_lib_table;
					cursor : type_lib_table.cursor := sym_table_global.first;
				begin
					log (text => "concatenating local and global symbol table ...", level => log_threshold + 1);
					
					-- Append table_global to table_local so that global libraries come AFTER local libraries.
					-- Loop in table_global and append element per element to table_local.
					-- If an library entry already exists in the local table, issue a warning and skip it.
					while cursor /= type_lib_table.no_element loop

						-- Test if entry already in local table.
						if not type_lib_table.contains (
							container	=> sym_table_local,
							item		=> element (cursor)) then

							-- Library entry not in local table -> append it.
							type_lib_table.append (
								container	=> sym_table_local,
								new_item	=> element (cursor)); -- fetch element from global table

						else -- entry already in local table -> warning and skip
							warning_on_multiple_entry_in_lib_table (
								et_devices.to_string (element (cursor).lib_uri));
						end if;
						
						next (cursor);
					end loop;

					-- Copy the resulting table to the tempoarily list "sym_lib_tables".
					-- When the module is created, it will be copied into the modules.
					sym_lib_tables := sym_table_local;
				end concatenate_local_and_global_sym_tables;

				procedure concatenate_local_and_global_fp_tables is
				-- Concatenates local and global fp-lib-tables so that global libraries come AFTER local libraries.
					use type_lib_table;
					cursor : type_lib_table.cursor := fp_table_global.first;
				begin
					log (text => "concatenating local and global footprint table ...", level => log_threshold + 1);
					
					-- Append table_global to table_local so that global libraries come AFTER local libraries.
					-- Loop in table_global and append element per element to table_local.
					-- If an library entry already exists in the local table, issue a warning and skip it.
					while cursor /= type_lib_table.no_element loop

						-- Test if entry already in local table.
						if not type_lib_table.contains (
							container	=> fp_table_local,
							item		=> element (cursor)) then

							-- Library entry not in local table -> append it.
							type_lib_table.append (
								container	=> fp_table_local,
								new_item	=> element (cursor)); -- fetch element from global table

						else -- entry already in local table -> warning and skip
							warning_on_multiple_entry_in_lib_table (
								et_devices.to_string (element (cursor).lib_uri));
						end if;
						
						next (cursor);
					end loop;

					-- Copy the resulting table to the tempoarily list "fp_lib_tables".
					-- When the module is created, it will be copied into the modules.
					fp_lib_tables := fp_table_local;
				end concatenate_local_and_global_fp_tables;
				
			begin -- read_lib_tables
				log (text => "reading sym-lib-tables", level => log_threshold);

				log_indentation_up;

				-- local table
				lib_table_path := to_bounded_string (file_sym_lib_table);
				if ada.directories.exists (to_string (lib_table_path)) then
					log (text => "local: " & to_string (lib_table_path), level => log_threshold + 1); -- show file path and name

					open (
						file => lib_table_handle,
						mode => in_file,
						name => to_string (lib_table_path));

					sym_table_local := read_table;
					
					close (lib_table_handle);
				end if;

				-- global table
				lib_table_path := to_bounded_string (value ("HOME") & file_sym_lib_table_global_linux);
				if ada.directories.exists (to_string (lib_table_path)) then
					log (text => "global: " & to_string (lib_table_path), level => log_threshold + 1); -- show file path and name

					open (
						file => lib_table_handle,
						mode => in_file,
						name => to_string (lib_table_path));

					sym_table_global := read_table;

					close (lib_table_handle);
				end if;

				concatenate_local_and_global_sym_tables;
				-- container sym_lib_tables now contains all library names and paths in this order:
				--  - local, in the order of appearance in the project specific sym-lib-table file
				--  - global, in the order of appearance in the global sym-lib-table file

				locate_component_libraries; -- as given in container sym_lib_tables. creates empty libraries in tmp_component_libraries.
				
				log_indentation_down;

				--------------

				log (text => "reading fp-lib-tables", level => log_threshold);

				log_indentation_up;

				-- local table
				lib_table_path := to_bounded_string (file_fp_lib_table);
				if ada.directories.exists (to_string (lib_table_path)) then
					log (text => "local: " & to_string (lib_table_path), level => log_threshold + 1); -- show file path and name

					open (
						file => lib_table_handle,
						mode => in_file,
						name => to_string (lib_table_path));

					fp_table_local := read_table;
					
					close (lib_table_handle);
				end if;

				-- global table
				lib_table_path := to_bounded_string (value ("HOME") & file_fp_lib_table_global_linux);
				if ada.directories.exists (to_string (lib_table_path)) then
					log (text => "global: " & to_string (lib_table_path), level => log_threshold + 1); -- show file path and name

					open (
						file => lib_table_handle,
						mode => in_file,
						name => to_string (lib_table_path));

					fp_table_global := read_table;

					close (lib_table_handle);
				end if;

				concatenate_local_and_global_fp_tables;
				-- container fp_lib_tables now contains all library names and paths in this order:
				--  - local, in the order of appearance in the project specific fp-lib-table file
				--  - global, in the order of appearance in the global fp-lib-table file

				locate_package_libraries; -- as given in container fp_lib_tables. creates empty libraries in et_kicad_pcb.package_libraries.
				
				log_indentation_down;

			end read_lib_tables;
			
			use et_import;
			
		begin -- read_project_file
			log (text => "reading project file ...", level => log_threshold);
			log_indentation_up;

			-- Clear tmp_component_libraries because it still contains librares of earlier project imports.
			-- If we import only one project, this statement does not matter:
			type_device_libraries.clear (tmp_component_libraries);
			
			case cad_format is
				
				-- For V4;
				--	The project file provides information where to search for libraries and search orders.
				when KICAD_V4 => read_proj_v4;

				-- For V5;
				--	The local symbol libraries are located as specified in the project directory in file sym-lib-table.
				--	The global symbol libraries are located as specified in file $HOME/.config/kicad/sym-lib-table.

				--	The local package libraries are located as specified in the project directory in file fp-lib-table.
				--	The global package libraries are located as specified in file $HOME/.config/kicad/fp-lib-table.

				when KICAD_V5 => read_lib_tables (log_threshold + 1);

				when others =>
					raise constraint_error;
					
			end case;

			log_indentation_down;
			
			-- Derive the top level schematic file name from the project name.
			-- It is just a matter of file extension.
			return to_schematic_file_name (
				compose (
					name		=> base_name (et_project.pac_project_name.to_string (project)),
					extension	=> file_extension_schematic)
					);
		end read_project_file;

		
		
-- 		function read_schematic (
-- 		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
--         -- they will be returned in hierarchic_sheet_file_names. Otherwise the returned list is empty.
-- 			--current_schematic	: in type_schematic_file_name.bounded_string;
-- 			current_schematic	: in type_hierarchic_sheet_file_name_and_timestamp;
-- 			log_threshold		: in type_log_level)
-- 			return type_hierarchic_sheet_file_names_extended is separate;

		module_name : type_submodule_name.bounded_string; -- the name of the module to be created
		module_inserted : boolean := false; -- goes true if module already created. should never happen

		procedure save_libraries is
			use et_import;

			procedure save_components (
			-- Saves the current tmp_component_libraries in the current module.
				module_name	: in type_submodule_name.bounded_string;
				module		: in out et_kicad.pcb.type_module) is
			begin
				module.component_libraries := tmp_component_libraries;
			end save_components;

			procedure save_packages (
			-- Saves the package_libraries in the current module.
				module_name	: in type_submodule_name.bounded_string;
				module		: in out et_kicad.pcb.type_module) is
			begin
				module.footprints := package_libraries;
			end save_packages;
			
		begin -- save_libraries
			-- tmp_component_libraries is a tempoarily storage place.
			-- It must be saved in module.component_libraries.
			-- tmp_component_libraries is furhter-on requried for other operations (like read_schematic)
			-- within the current module.
			-- CS: in the future tmp_component_libraries should be discarded. update_element and query_element
			-- operations should access the component_libraries of a module directly.
			type_modules.update_element (modules, module_cursor, save_components'access);
			
			-- V5: et_kicad_pcb.package_libraries is a temparily storage place. It must be saved in module.footprints.
			if cad_format = KICAD_V5 then
				type_modules.update_element (modules, module_cursor, save_packages'access);
			end if;
			
		end save_libraries;
		
	begin -- import_design

		-- change to given project directory
		log (text => "changing to project directory " & (et_project.to_string (project) & " ..."), level => log_threshold);
		set_directory (et_project.to_string (project));
		
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>

				-- Kicad uses Y axis positive downwards style (in both schematic and board)
				-- Derive top level schematic file name from project name.
				-- Clears tmp_component_libraries (which is a temparily storage).
				-- Creates empty component/symbol libraries in tmp_component_libraries.
				-- For V4:	This action creates new directory and component library search lists
				-- 			in search_list_component_libraries and search_list_project_lib_dirs.
				-- For V5:	Reads sym-lib-tables and stores them in sym_lib_tables.
				--			Creates empty package libraries in et_kicad_pcb.package_libraries.
				top_level_schematic	:= read_project_file (log_threshold + 1);
				log_indentation_up;
				
				log (text => "top level schematic is " & enclose_in_quotes (to_string (top_level_schematic)),
					 level => log_threshold + 2);
				
				module_name := to_submodule_name (top_level_schematic);

				log (text => "creating module " & enclose_in_quotes (to_string (module_name)),
					 level => log_threshold + 2);
				
				-- create the module:
				type_modules.insert (
					container	=> modules,
					key			=> module_name,
					new_item 	=> (

						-- These search lists are used in V4:
						search_list_library_comps	=> search_list_component_libraries, -- see function read_project_file
						search_list_library_dirs	=> search_list_project_lib_dirs, -- see function read_project_file

						-- V5 uses sym-lib-tables and fp-lib-tables:
						sym_lib_tables		=> sym_lib_tables, -- see function read_project_file
						fp_lib_tables		=> fp_lib_tables, -- see function read_project_file

						-- symbol/component libraries
						component_libraries => type_device_libraries.empty_map,

						-- V5
						-- package/footprint libraries
						footprints			=> type_libraries.empty_map,
						
						strands				=> schematic.type_strands.empty_list,
						junctions			=> type_junctions.empty_list,
						nets				=> type_nets.empty_map,
						net_classes			=> et_pcb.type_net_classes.empty_map, -- net classes are defined in the board file
						components			=> type_components_schematic.empty_map,
						no_connections		=> type_no_connection_flags.empty_list,
						portlists			=> type_portlists.empty_map,
						netlist				=> type_netlist.empty_map,
						hierarchic_sheets	=> type_hierarchic_sheets.empty_map,
						frames				=> type_frames.empty_list,
						notes				=> type_texts.empty_list,
						sheet_headers		=> type_sheet_headers.empty_map,

						board_available		=> <>,
						board				=> (others => <>) -- no board stuff available at this time -> use defaults
						),
					
					position	=> module_cursor,
					inserted	=> module_inserted);

				if not module_inserted then -- CS should never happen
					log (ERROR, "module " & to_string (module_name) & " already in imported !");
					raise constraint_error;
				end if;

				-- read package libraries
				read_libraries (log_threshold); -- fills package_libraries
				
				-- read component libraries
				read_components_libraries (log_threshold); -- fills tmp_component_libraries

-- 				-- copy temparily library containers in module
-- 				save_libraries;

				current_schematic.sheet.file := top_level_schematic;
				check_submodule_name_characters (to_submodule_name (current_schematic.sheet.file));
				
                -- The top level schematic file is the first entry in the module path.
				-- The top level schematic file is the root in the module path.
				-- Starting from the top level module, we read its schematic file. The result can be a list 
				-- of sheets which means that the design is hierarchic.
				
				-- The function read_schematic requires the name of the current schematic,
				-- It returns a list of hierachic sheets.
				hierarchic_sheet_file_names := read (current_schematic, sheet_number, log_threshold);

				-- If read_schematic returns an empty list of hierachic sheets file names,
				-- we are dealing with a flat design. Otherwise the design is hierarchic.
				if type_hierarchic_sheet_file_names.is_empty (hierarchic_sheet_file_names.sheets) then -- flat design
					log (text => "design structure FLAT");
				else -- hierarchic design
					-- In the following we dive into the sheets. Each time before a deeper level is entered,
					-- the list of sheets (of the current level) is saved on a LIFO stack.
					-- The top level schematic is at level 0. The level decreases (negative) each time a deeper
					-- level is entered.
					log (text => "design structure HIERARCHIC");
					
					stack_of_sheet_lists.init; -- stack init

					log_indentation_up;
					
					-- output the number of sheets found at level 0:
					log (text => "number of hierarchic sheets total" & natural'image (
						natural (type_hierarchic_sheet_file_names.length (hierarchic_sheet_file_names.sheets)))); -- CS: use count_type

					-- Initially set sheet pointer at first sheet of list:
					hierarchic_sheet_file_names.id := 1;
                    
					loop
						if hierarchic_sheet_file_names.id <= positive (type_hierarchic_sheet_file_names.length (hierarchic_sheet_file_names.sheets)) then

							current_schematic := type_hierarchic_sheet_file_names.element (
										container	=> hierarchic_sheet_file_names.sheets,
										index		=> hierarchic_sheet_file_names.id);

							check_submodule_name_characters (to_submodule_name (current_schematic.sheet.file));
							
							-- backup hierarchic_sheet_file_names OF THIS LEVEL on stack (including the current sheet id)
							push (hierarchic_sheet_file_names);
							
							append_sheet_name_to_path (current_schematic.sheet.name);
							
							-- Read schematic file as indicated by hierarchic_sheet_file_names.id. 
							-- Read_schematic receives the name of the schematic file to be read.
							-- The sheet number increases each time.
							sheet_number := et_coordinates."+" (sheet_number, 1);
							hierarchic_sheet_file_names := read (current_schematic, sheet_number, log_threshold);

							-- If the schematic file contains hierarchic sheets, set hierarchic_sheet_file_names.id to the first 
							-- sheet of them. Otherwise restore sheet list of parent sheet and advance there to next sheet.
							if type_hierarchic_sheet_file_names.is_empty (hierarchic_sheet_file_names.sheets) then -- flat schematic (no hierarchic sheets)

								hierarchic_sheet_file_names := pop;
								delete_last_module_name_from_path;
								hierarchic_sheet_file_names.id := hierarchic_sheet_file_names.id + 1;

							else
								-- set cursor to first sheet of list
								hierarchic_sheet_file_names.id := 1;
							end if;

						end if;
							
						-- Once the last sheet of the list has been processed, restore list of the overlying 
						-- level and advance to next sheet.
						-- Exit after last sheet in level 0 has been processed.
						if hierarchic_sheet_file_names.id > positive (type_hierarchic_sheet_file_names.length (hierarchic_sheet_file_names.sheets)) then

							if depth = 0 then 
								exit; 
							end if;

							hierarchic_sheet_file_names := pop; -- restore overlying list
							delete_last_module_name_from_path;
							hierarchic_sheet_file_names.id := hierarchic_sheet_file_names.id + 1;
							
						end if;
						
					end loop;

				end if;

				-- During reading the schematic files, the component libraries in tmp_component_libraries
				-- may have been extended by package variants. Now they are complete and can be finally
				-- assigned to the module.
				-- CS: test in V5
				save_libraries;
				
				-- Update strand names according to power in/out ports connected with them:
				update_strand_names (log_threshold + 1); -- includes portlist generation

				-- write strands report
				write_strands (log_threshold + 1);
				
				-- Merge the strands which are still independed of each other. 
				-- For example a strand named "VCC3V3" exists on submodule A on sheet 2. 
				-- Another strand "VCC3V3" exists on submodule C on sheet 1. They do not "know" each other
				-- and must be merged into a single net.
				link_strands (log_threshold + 1);

				-- Append hierarchic strands to global or local nets.
				-- IMPORTANT: Hierarchic nets are nothing more than extensions of local or global nets !
				process_hierarchic_nets (log_threshold + 1);
				
				-- write net report
				write_nets (log_threshold + 1);

				-- do some simple design checks
				check_junctions (log_threshold + 1);
				check_orphaned_junctions (log_threshold + 1);
				check_misplaced_junctions (log_threshold + 1);	
				check_misplaced_no_connection_flags (log_threshold + 1);
				check_orphaned_no_connection_flags (log_threshold + 1);

				-- make netlists
				make_netlists (log_threshold + 1);

				-- import layout(s)
				log_indentation_reset;
				log (text => row_separator_double);
				log (text => "importing layouts/boards ...", console => true);
				log_indentation_up;
				et_kicad.pcb.read_boards (log_threshold);
				log_indentation_down;
				
			when others =>
				null; -- CS: add import of other CAD kicad formats (v6, v7, ..) here
				
		end case;

		-- change back to initial directory
		set_directory (current_working_directory);
		
		exception
-- 			-- CS: log exception message
			when event:
				others =>
					log (text => ada.exceptions.exception_message (event), console => true);
					set_directory (current_working_directory);
				
-- 					log (ERROR, "in schematic file '" 
-- 						& to_string (current_schematic) & "' " 
-- 						console => true);
-- 						et_import.close_report;
-- 						put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;
		
	end import_design;
	
	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment_base'class) 
		return boolean is

		sits_on_segment : boolean := false;

		use et_schematic.pac_shapes;
		d : type_distance_point_line;

		-- CS this is a workaround in order to provide a line for function distance_point_line:
		type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
		line : type_line_scratch := (
			start_point	=> type_point (segment.coordinates_start), 
			end_point	=> type_point (segment.coordinates_end));
		
	begin
		-- calculate the shortes distance of point from line.
		d := distance_point_line (
			point 		=> type_point (junction.coordinates),
			line		=> line,
			line_range	=> BETWEEN_END_POINTS);

		if (not out_of_range (d)) and distance (d) = zero then
			sits_on_segment := true;
		end if;

		return sits_on_segment;
	end junction_sits_on_segment;

	
	function component_power_flag (cursor : in type_components_library.cursor)
	-- Returns the component power flag status.
		return type_power_flag is
		use et_string_processing;
		use et_symbols;
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

	
-- 	function purpose ( -- CS move to et_schematic or et_project
-- 	-- Returns the purpose of the given component in the given module.
-- 	-- If no purpose specified for the component, an empty string is returned.
-- 		module_name		: in et_coordinates.type_submodule_name.bounded_string; -- led_matrix_2
-- 		reference		: in type_device_name; -- X701
-- 		log_threshold	: in et_string_processing.type_log_level)
-- 		return et_libraries.type_component_purpose.bounded_string is
-- 
-- 		use et_string_processing;	
-- 		use et_libraries.type_component_purpose;
-- 		use type_modules;
-- 	
-- 		module_cursor : type_modules.cursor;
-- 		purpose : et_libraries.type_component_purpose.bounded_string; -- to be returned
-- 	
-- 		procedure query_components (
-- 		-- Searches the components of the module for the given reference.
-- 			module_name : in et_coordinates.type_submodule_name.bounded_string;
-- 			module		: in type_module) is
-- 			use type_components_schematic;
-- 			component_cursor : type_components_schematic.cursor := module.components.first;
-- 		begin
-- 			log (text => "querying components ...", level => log_threshold + 1);
-- 			log_indentation_up;
-- 
-- 			while component_cursor /= type_components_schematic.no_element loop
-- 				if et_libraries."=" (key (component_cursor), reference) then
-- 
-- 					-- component with given reference found.
-- 					purpose := element (component_cursor).purpose;
-- 					exit; -- no need for further searching
-- 					
-- 				end if;
-- 				next (component_cursor);
-- 			end loop;
-- 
-- 			log_indentation_down;
-- 		end query_components;
-- 			
-- 	begin -- purpose
-- 		log (text => "module " & et_coordinates.to_string (module_name) 
-- 			 & " looking up purpose of " 
-- 			 & et_libraries.to_string (reference) & " ...", level => log_threshold);
-- 		log_indentation_up;
-- 
-- 		-- set module cursor
-- 		module_cursor := find (et_kicad.modules, module_name);
-- 
-- 		-- if module exists, query its component list
-- 		if module_cursor /= type_modules.no_element then
-- 			query_element (
-- 				position	=> module_cursor,
-- 				process		=> query_components'access);
-- 		else
-- 			module_not_found (module_name); -- module does not exist -> error
-- 		end if;
-- 
-- 		-- Show purpose.
-- 		if length (purpose) = 0 then
-- 			log (text => "no purpose specified", level => log_threshold + 1);
-- 		else
-- 			log (text => "purpose " & et_libraries.to_string (purpose), level => log_threshold + 1);
-- 		end if;
-- 		
-- 		log_indentation_down;
-- 		return purpose;
-- 	end purpose;

	procedure add_strand (
	-- Adds a strand into the the module (indicated by module_cursor).
		strand : in type_strand) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
			use et_string_processing;
		begin
			log_indentation_up;
			log (text => "inserting strand " 
				 & et_general.to_string (strand.name) & " in database ...", level => 3); -- CS log_threshold ?
			log_indentation_down;

			module.strands.append (strand);
		end add;
		
	begin -- add_strand
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_strand;

	function first_strand return type_strands.cursor is
	-- Returns a cursor pointing to the first strand of the module (indicated by module_cursor).
		cursor : type_strands.cursor;	

		procedure set_cursor (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
 		begin
			cursor := module.strands.first;
		end set_cursor;
	
	begin
		type_modules.query_element (
			position	=> module_cursor,
			process		=> set_cursor'access
			);
		return cursor;
	end first_strand;

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

	procedure rename_strands (
	-- Renames all strands with the name_before to the name_after.
	-- Changes the scope of the affected strands to "global".
	-- This procdure is required if a strand is connected to a power-out port.
	-- The power-out port enforces its name onto the strand.
		name_before		: in type_net_name.bounded_string;
		name_after		: in type_net_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_schematic;

		count : natural := 0;
	
		procedure rename (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) is

			use type_strands;
			
			cursor : type_strands.cursor := module.strands.first;
			-- Points to the strand being processed
			
			renamed : boolean := false; -- signals that a strand renaming took place
			-- Used to abort renaming after an anonymous strands has been renamed.
			-- The names of anonymous strands like (N$6) are unique. So after the first
			-- renaming the procedure comes to an early end.

			procedure do_it (strand : in out type_strand) is begin
				-- search for the strand to be renamed
				if strand.name = name_before then

					count := count + 1;
					log_indentation_up;
-- 					log (text => 
-- 						text => to_string (strand.name) 
-- 							& " to "
-- 							& to_string (name_after) & " ...",
-- 							level => 2
-- 						);
					log (text => to_string (position => strand.position, scope => et_kicad_coordinates.MODULE),
						level => log_threshold + 1);

					log_indentation_down;

					strand.name := name_after; -- assign new name to strand
					strand.scope := global;

					renamed := true; -- signal that renaming took place
				end if;
			end do_it;
			
		begin -- rename
			while cursor /= type_strands.no_element loop
				module.strands.update_element (
					position => cursor,
					process => do_it'access);

				-- Exit prematurely if name_before was anonymous. anonymous strand names are unique.
				-- So it is ok to exit prematurely.
				if renamed and anonymous (name_before) then
					exit;
				end if;
				
				next (cursor);
			end loop;
		end rename;
		
	begin -- rename_strands
		log (text => "renaming strands from " & et_general.to_string (name_before)
			 & " to " & et_general.to_string (name_after) & " ...", level => log_threshold);
		
		modules.update_element (
			position	=> module_cursor,
			process		=> rename'access
			);

		if count > 0 then
			log (text => "renamed" & natural'image (count) & " strands", level => log_threshold);
		else
			-- CS: This should never happen
			log (ERROR, "strand " 
				& et_general.to_string (name_before) & " not found !");
			raise constraint_error;
		end if;
	end rename_strands;

	function port_connected_with_segment (
	-- Returns true if the given port sits on the given net segment.
		port	: in type_port'class; -- CS class wide required ?
		segment	: in type_net_segment_base'class) -- CS class wide required ?
		-- NOTE: Passing a cursor to given segment does not work. This measure would make
		-- excluding the same segment easier in procedure query_segments. The cursor to the given segment
		-- would be the same type as the segment being inquired, yet they do not point to the same
		-- memory location. So forget this idea.
		return boolean is

		use et_string_processing;
		
		sits_on_segment : boolean := false;

		use et_schematic.pac_shapes;
		distance_port_segment : type_distance_point_line;

		function junction_here return boolean is
		-- Returns true if a junction sits at the coordinates of the given port.
			junction_found : boolean := false; -- to be returned
		
			procedure query_junctions (
			-- Query junctions. Exits prematurely once a junction is found.
				module_name	: in type_submodule_name.bounded_string;
				module 		: in type_module) is
				use type_junctions;
				junction_cursor : type_junctions.cursor := module.junctions.first;
			begin -- query_junctions
				junction_found := false;
				while junction_cursor /= type_junctions.no_element loop
					-- compare coordinates of junction and given port
					if element (junction_cursor).coordinates = port.coordinates then
						junction_found := true;
						exit; -- no further search required
					end if;
					next (junction_cursor);	
				end loop;
			end query_junctions;
		
		begin -- junction_here
			type_modules.query_element (
				position	=> module_cursor,
				process 	=> query_junctions'access);

			return junction_found;
		end junction_here;

		function another_segment_here return boolean is
		-- Returns true if another segment is placed at the coordinates of the given port.
			segment_found : boolean := false; -- to be returned
		
			procedure query_strands (
			-- Query net segments. Exits prematurely once a segment is found.
				module_name	: in type_submodule_name.bounded_string;
				module 		: in type_module) is
				use type_strands;
				strand_cursor : type_strands.cursor := module.strands.first;

				procedure query_segments (
					strand : in type_strand) is
					use type_net_segments;
					segment_cursor : type_net_segments.cursor := strand.segments.first;
				begin
					while segment_cursor /= type_net_segments.no_element loop
				
						-- The inquired segment must not be the same as the given segment:
						if not (element (segment_cursor).coordinates_start = segment.coordinates_start and
							element (segment_cursor).coordinates_end = segment.coordinates_end) then

							--log (text => "probing segment " & to_string (element (segment_cursor)));
							
							-- If the inquired segment is placed with start or end point 
							-- at the given port position, we exit prematurely:
							if	element (segment_cursor).coordinates_start = port.coordinates or
								element (segment_cursor).coordinates_end   = port.coordinates then
									--log (text => "segment found");
									segment_found := true;
									exit;
							end if;

						end if;
							
						next (segment_cursor);
						
					end loop;
				end query_segments;
				
			begin -- query_strands
				-- Once a segment has been found or all strands have been processed:
				while (not segment_found) and strand_cursor /= type_strands.no_element loop

					type_strands.query_element (
						position	=> strand_cursor,
						process 	=> query_segments'access);

					next (strand_cursor);
				end loop;
			end query_strands;
		
		begin -- another_segment_here
			--log (text => "probing for other segment at " & to_string (port.coordinates, et_coordinates.module));
		
			type_modules.query_element (
				position	=> module_cursor,
				process		=> query_strands'access);

			return segment_found;
		end another_segment_here;

		procedure test_junction is
		begin
			if junction_here then
				sits_on_segment := true;
			else
				log (ERROR, "missing junction at " 
					& to_string (port.coordinates, et_kicad_coordinates.MODULE),
						console => true);
				raise constraint_error;
			end if;
		end test_junction;

		-- CS this is a workaround in order to provide a line for function distance_point_line:
		type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
		line : type_line_scratch := (
			start_point	=> type_point (segment.coordinates_start), 
			end_point	=> type_point (segment.coordinates_end));
		
	begin -- port_connected_with_segment
		-- First make sure the port is to be connected at all. Ports intended to be open
		-- are regarded as "not connected with the segment".
		--if not port.intended_open then
		if not (port.intended_open) then
	
			-- Make sure port and segment share the same module path and sheet.
			-- It is sufficient to check against the segment start coordinates.
			if same_path_and_sheet (port.coordinates, segment.coordinates_start) then

				-- calculate the shortes distance of point from line.
				distance_port_segment := distance_point_line (
					point 		=> type_point (port.coordinates),
					line		=> line,
					line_range	=> WITH_END_POINTS);

				if (not out_of_range (distance_port_segment)) and distance (distance_port_segment) = zero then

					-- If point sits on either start or end point of given line
					if on_start_point (distance_port_segment) or on_end_point (distance_port_segment) then

						-- If another segment meets here a junction is required:
						if another_segment_here then
							test_junction;
						else 
							-- no other segment here -> port is connected 
							-- only with start or end point of given segment:
							sits_on_segment := true;
							--log (text => "port on segment", level => 5);
						end if;
							
					else 
						-- Point sits between start and end point of given line.
						-- This case requires a junction:
						test_junction;

					end if;
				end if;

			end if;
		end if;
		
		return sits_on_segment;
	end port_connected_with_segment;
	
	
	procedure update_strand_names (log_threshold : in et_string_processing.type_log_level) is
	-- Tests if a power in/out port is connected to a strand and renames the strand if necessary.
	-- Depending on the CAE system power-out or power-in ports may enforce their name on a strand.
		use et_string_processing;
		--use et_schematic;
	
		portlists : type_portlists.map := type_portlists.empty_map;

		strand		: type_strands.cursor := first_strand;
		segment		: type_net_segments.cursor;
		component	: type_portlists.cursor;
		port		: type_ports.cursor;

		use et_symbols;
		use type_strands;
		use type_net_segments;
		use type_portlists;
		use type_ports;
		
		function to_net_name (port_name : in type_port_name.bounded_string) 
		-- Converts the given port name to a net name.
			return type_net_name.bounded_string is
		begin
			return to_net_name (to_string (port_name));
		end to_net_name;
		
	begin -- update_strand_names
		log (text => "updating strand names by power-out ports ...", level => log_threshold);

		-- Generate the portlists of the module indicated by module_cursor.
		portlists := build_portlists (log_threshold + 1);

		-- LOOP IN STRANDS OF MODULE
		while strand /= type_strands.no_element loop
			log_indentation_up;
			log (text => "strand of net " & et_general.to_string (element (strand).name), level => log_threshold + 3);

			-- LOOP IN SEGMENTS OF STRAND
			segment := first_segment (strand);
			while segment /= type_net_segments.no_element loop
				log_indentation_up;
				log (text => "probing segment " & to_string (element (segment)), level => log_threshold + 3);

				-- LOOP IN COMPONENTS (of portlists)
				component := first (portlists);
				while component /= type_portlists.no_element loop
					log_indentation_up;
					log (text => "probing component " & to_string (key (component)), level => log_threshold + 4);

					-- LOOP IN PORTLIST (of component)
					port := first_port (component);
					while port /= type_ports.no_element loop
						log_indentation_up;

						-- CS: skip already processed ports to improve performance

						log (text => "probing port " & to_string (position => element (port).coordinates), level => log_threshold + 4);

						-- test if port is connected with segment
						if port_connected_with_segment (element (port), element (segment)) then
							log_indentation_up;
							-- log (text => "match", level => log_threshold + 2);

							-- We are interested in "power in" ports exclusively. Only such ports may enforce their
							-- name on a strand.
							if element (port).direction = POWER_IN then

								-- If strand has no name yet, it is to be named after the name of the port that sits on it.
								-- If strand has a name already, its scope must be global
								-- because power-in ports are allowed in global strands exclusively !
								if anonymous (element (strand).name) then
									log (text => "component " & to_string (key (component)) 
										& " port name " & to_string (element (port).name) 
										& " is a power input -> port name sets strand name", level => log_threshold + 2);

									-- rename strand
									rename_strands (
										name_before => element (strand).name,
										name_after => to_net_name (element (port).name),
										log_threshold => log_threshold + 3);

								-- If strand has been given a name already (for example by previous power-in ports) AND
								-- if strand name differs from name of current power-in port -> warning
								elsif et_general.to_string (element (strand).name) /= to_string (element (port).name) then
									log (WARNING, "component " & to_string (key (component)) 
										& " POWER IN port " & to_string (element (port).name) 
										& " at" & to_string (element (port).coordinates, module)
										& " conflicts with net " & et_general.to_string (element (strand).name) & " !");
									--raise constraint_error;

								-- If strand has a name and is local or hierarchic -> error and abort
								--elsif et_schematic."/=" (element (strand).scope, et_schematic.global) then
								elsif element (strand).scope /= GLOBAL then
									log (ERROR, "component " & to_string (key (component)) 
										& " POWER IN port " & to_string (element (port).name) 
										& " at" & to_string (element (port).coordinates, module)
										& " conflicts with " & to_string (element (strand).scope) 
										& " net " & et_general.to_string (element (strand).name) & " !");
									raise constraint_error;

								end if;

							end if;

							log_indentation_down;
						end if;
						
						log_indentation_down;
						next (port);
					end loop;

					log_indentation_down;
					next (component);
				end loop;


				log_indentation_down;
				next (segment);
			end loop;

			log_indentation_down;
			next (strand);
		end loop;
		
	end update_strand_names;

	procedure write_strands (log_threshold : in et_string_processing.type_log_level) is
	-- Writes a nice overview of strands, net segments and labels
	-- CS: output consequtive number for strands and segments (as in procedure write_nets)
		use et_string_processing;

		procedure query_label (
			segment : in type_net_segment) is
			label_simple	: type_simple_labels.cursor	:= segment.label_list_simple.first;
			label_tag		: type_tag_labels.cursor	:= segment.label_list_tag.first;
			use type_simple_labels;
			use type_tag_labels;
		begin
			if log_level >= log_threshold + 2 then
				log_indentation_up;
				while label_simple /= type_simple_labels.no_element loop
					--log (text => "simple label " & to_string (position => element (label_simple).coordinates));
					log (text => "simple label " & to_string (element (label_simple).coordinates));
					next (label_simple);
				end loop;

				while label_tag /= type_tag_labels.no_element loop
					--log (text => "tag label " & to_string (position => element (label_tag).coordinates));
					log (text => "tag label " & to_string (element (label_tag).coordinates));
					next (label_tag);
				end loop;

				log_indentation_down;
			end if;
		end query_label;
	
		procedure query_segment (
			strand : in type_strand) is
			segment : type_net_segments.cursor := strand.segments.first;
			use type_net_segments;
		begin
			if log_level >= log_threshold + 1 then
				while segment /= type_net_segments.no_element loop
					log_indentation_up;
					log (text => "segment" & to_string (element (segment)));

					type_net_segments.query_element (
						position	=> segment,
						process		=> query_label'access);

					log_indentation_down;				
					next (segment);
				end loop;
			end if;
		end query_segment;
	
		procedure query_strands (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			strand : type_strands.cursor := module.strands.first;
			use type_strands;
			use type_path_to_submodule;
			use ada.directories;
		begin
			if log_level >= log_threshold then
				while strand /= type_strands.no_element loop
					log_indentation_up;

					log (text => et_general.to_string (element (strand).name) &
						 " scope " & to_string (element (strand).scope) &
						 " in " & to_string (path (element (strand).position)));
					
					type_strands.query_element (
						position	=> strand,
						process		=> query_segment'access);
					
					log_indentation_down;
					next (strand);
				end loop;
			end if;
		end query_strands;
		
	begin -- write_strands
		if log_level >= log_threshold then
			log (text => "strands report");
			
			type_modules.query_element (
				position	=> module_cursor,
				process		=> query_strands'access);
		end if;
	
	end write_strands;


	
	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in et_kicad_general.type_device_library_name.bounded_string;
		component	: in type_component_generic_name.bounded_string) 
		return type_components_library.cursor is

		lib_cursor	: type_device_libraries.cursor;
		
		use type_components_library;
		comp_cursor	: type_components_library.cursor := no_element;
	
		use type_device_libraries;

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

	procedure reset_component_cursor (cursor : in out type_components_schematic.cursor) is
	-- Resets the given component cursor to the begin of the component list
	-- of the module indicated by module_cursor.
		procedure reset (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components_schematic;
		begin
			cursor := type_components_schematic.first (module.components);
		end reset;
	begin
		type_modules.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_component_cursor;

	function build_portlists (log_threshold : in et_string_processing.type_log_level) 
		return type_portlists.map is
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.
	-- This applies to the module indicated by module_cursor.
		
	-- Locates the components of the schematic in the libraries. 
	-- Computes the absolute port positions of components from:
	--  - the port coordinates provided by the librares
	--  - the unit coordinates provided by the schematic
	--  - the unit mirror style provided by the schematic
	--  - the unit orientation provided by the schematic

	-- Special threatment for "common to all units". Such units are global units.
	-- Their ports apply to all units and are added in the portlist of a component multiple
	-- times but with different coordinates.
	-- See comments.

	-- Sets the "open" flag of the port if it was marked with a no-connect-flag.
	
	-- Stores the absolute port coordinates in map "portlists". 
	-- The key into this map is the component reference.
	
	-- Saves the portlists in the module (indicated by module_cursor).
	
		-- Here we collect the portlists:
		portlists					: type_portlists.map;
		component_inserted			: boolean;
		component_cursor_portlists	: type_portlists.cursor; -- points to the portlist being built
	
		use type_full_library_names;
		use et_string_processing;

		-- This component cursor points to the schematic component being processed.
		use type_components_schematic;
		component_cursor_sch: type_components_schematic.cursor;

		-- The component reference in the schematic (like R44 or IC34)
		-- is tempoarily held here:
		component_reference	: type_device_name;
	
		-- This component cursor points to the library component being processed.
		use type_components_library;
		component_cursor_lib: type_components_library.cursor;

		-- CS: log_threshold for messages below

		-- For tempoarily storage of units of a component (taken from the schematic):
		units_sch : type_units_schematic.map;

		procedure extract_ports is
		-- Extracts the ports of the component indicated by component_cursor_lib.
		-- NOTE: The library contains the relative (x/y) positions of the ports.
			use type_ports_library;
			use et_schematic;
			use et_devices;
			use et_symbols;
		
			-- The unit cursor of the component advances through the units stored in the library.
			unit_cursor : type_units_library.cursor;

			-- The port cursor of the unit indicates the port of a unit.
			port_cursor : type_ports_library.cursor; 

			unit_name_lib : pac_unit_name.bounded_string; -- the unit name in the library. like "A", "B" or "PWR"
			unit_position : et_kicad_coordinates.type_position; -- the coordinates of the current unit
			-- CS: external units

			procedure add_port is
			-- Builds a new port and appends it to portlist of the current 
			-- component (indicated by component_cursor_portlists).
			
			-- The library defined properties of the port are taken from where port_cursor points to.
			-- They are copied to the new port without change.
			
			-- Properites set in the schematic such as path, module name, sheet are copied into the
			-- new port unchanged. X and Y position of the port must be re-computed according to
			-- the rotation, mirror style and position of the unit in the schematic.
			-- NOTE: It is important first to rotate, then mirror (if required) and finally to move/offset it.

				procedure add (
					component	: in type_device_name;
					ports		: in out type_ports.list) is
					use type_modules;
					
					port_coordinates : et_kicad_coordinates.type_position;

					function left_open return type_port_open is
					-- Returns true if a no-connect-flag sits at the port_coordinates.

						port_open : type_port_open := false;
					
						procedure query_no_connect_flags (
							module_name	: in type_submodule_name.bounded_string;
							module 		: in et_kicad.pcb.type_module) is
							use type_no_connection_flags;
							flag_cursor : type_no_connection_flags.cursor := module.no_connections.first;

							use type_path_to_submodule;
						begin
							-- Compare coordinates of no-connection-flags with port_coordinates
							-- and exit prematurely with "open" set to true.
							while flag_cursor /= type_no_connection_flags.no_element loop

-- 								log (text => "probing port at         " & to_string (port_coordinates, et_coordinates.module));
-- 								log (text => "probing no-connect-flag " & to_string (element (flag_cursor), et_coordinates.module));

								-- CS: to improve performance, test if flag has not been processed yet
								-- But first implement a test that raises error if more than one port 
								-- sits on the same position.
								
 								if element (flag_cursor).coordinates = port_coordinates then
									--log (text => " intentionally left open", level => log_threshold + 3);
									log (text => " has no-connect-flag -> intentionally left open", level => log_threshold + 1);
									port_open := true;
									exit;
								end if;
								
								next (flag_cursor);
							end loop;
						end query_no_connect_flags;
						
					begin -- left_open

						-- Query no-connect-flags:
						query_element (
							position => module_cursor,
							process => query_no_connect_flags'access);

						-- If this statement is reached, no flag was found -> return false
						return port_open;
					end left_open;
					
				begin -- add
					-- Init port coordinates with the coordinates of the port found in the library.
					-- The port position is a type_point and must be converted to type_position.
					set (
						point		=> port_coordinates,
						position	=> element (port_cursor).position);

					-- rotate port coordinates
					rotate_by (
						point		=> port_coordinates,
						rotation	=> orientation_of_unit (unit_name_lib, units_sch)
						);

					-- Mirror port coordinates if required.
					case mirror_style_of_unit (unit_name_lib, units_sch) is
						when NO => null; -- unit not mirrored in schematic
						when X_AXIS => mirror (point => port_coordinates, axis => X);
						when Y_AXIS => mirror (point => port_coordinates, axis => Y);
					end case;

					-- offset port coordinates by the coordinates of the unit found in the schematic
					move_by (point => port_coordinates, offset => unit_position);

					-- path remains unchanged because the port is still where the unit is
					set_path (
						position	=> port_coordinates,
						path		=> path (unit_position));

					-- sheet name remains unchanged because the sheet is still the same
					set_sheet (port_coordinates, sheet (unit_position));
					
					-- Insert a the newly built port in the portlist of the component.
					-- This action depends on the appearance of the schematic component being processed.
					-- For example: only virtual components can be power_flags.
					case element (component_cursor_sch).appearance is
						when VIRTUAL =>
							type_ports.append (
								container => ports,
								new_item => (

									-- library defined properites:
									name		=> element (port_cursor).name, -- the port name like GPIO4
									direction	=> element (port_cursor).direction, -- the port direction

									-- Set the power_flag status (by taking it from the schematic component begin processed).
									power_flag	=> element (component_cursor_sch).power_flag,
									
									style		=> element (port_cursor).style, -- port style

									-- We also set the port appearance (by taking it from the schematic component begin processed).
									-- Later when writing the netlist, this property
									-- serves to tell real from virtual ports.
									appearance	=> element (component_cursor_sch).appearance,

									-- schematic defined properties:
									coordinates	=> port_coordinates,

									-- if to be left open intentionally, the list of no-connection-flags must be looked up
									intended_open => left_open,
									
									connected	=> no -- used by netlist generator (procedure make_netlists)
									));

						when et_symbols.PCB =>
							type_ports.append (
								container => ports,
								new_item => (

									-- library defined properites:
									name		=> element (port_cursor).name, -- the port name like GPIO4
									direction	=> element (port_cursor).direction, -- the port direction

									-- This port does not belong to a power_flag, because real components can never be.
									power_flag	=> no,
									
									style		=> element (port_cursor).style, -- port style

									-- We also set the port appearance (by taking it from the schematic component begin processed).
									-- Later when writing the netlist, this property
									-- serves to tell real from virtual ports.
									appearance	=> element (component_cursor_sch).appearance,
									
									-- schematic defined properties:
									coordinates	=> port_coordinates,

									-- if to be left open intentionally, the list of no-connection-flags must be looked up
									intended_open => left_open,
									
									connected	=> no -- used by netlist generator (procedure make_netlists)
									));

					end case;
							
					log (text => to_string (type_ports.last_element (ports).direction), level => log_threshold + 3);
					log_indentation_up;
					-- CS: other port properties
					log (text => to_string (position => type_ports.last_element (ports).coordinates), level => log_threshold + 3);
					log_indentation_down;
				end add;
				
			begin -- add_port
				-- We update the portlist of the component in container portlists.
				-- The cursor to the portlist was set when the element got inserted (see below in procedure build_portlists).
				type_portlists.update_element (
					container	=> portlists,
					position	=> component_cursor_portlists,
					process		=> add'access);
			end add_port;

			procedure ports_of_global_unit is
			-- Searches in the component (indicated by component_cursor_lib) for units
			-- with the "global" flag set.
			-- Sets the port_cursor for each port and leaves the rest of the work to procedure add_port.
				unit_cursor : type_units_library.cursor;
				use type_units_library;
			begin
				-- Loop in list of internal units:
				unit_cursor := first_unit (component_cursor_lib);
				while unit_cursor /= type_units_library.no_element loop
					log_indentation_up;

					if element (unit_cursor).global then
						--log (text => "global unit " & to_string (key (unit_cursor)));

						-- NOTE: One could think of exiting the loop here once the global unit
						-- has been found. If it were about KiCad only, this would make sense
						-- as there can be only one global unit per component.
						-- As for other CAE tools there might be more global units, so there
						-- is no early exit here.

						-- Loop in port list of the unit:						
						port_cursor := first_port (unit_cursor); -- port in library
						while port_cursor /= type_ports_library.no_element loop

							--log (text => "port " & type_port_name.to_string (key (port_cursor))
							log (text => "port " & to_string (element (port_cursor).name),
									--& " pin/pad " & to_string (element (port_cursor).pin),
								 level => log_threshold + 2);

							-- Build a new port and append port to portlist of the 
							-- current component (indicated by component_cursor_portlists).
							add_port;
							
							port_cursor := next (port_cursor);
						end loop;
					end if;

					log_indentation_down;
					unit_cursor := next (unit_cursor);
				end loop;

			end ports_of_global_unit;
			
		begin -- extract_ports
			-- Loop in unit list of the component (indicated by component_cursor_lib).
			-- unit_cursor_internal points to the unit in the library.
			-- Frequently, not all units of a component are deployed in the schematic.
			-- If a unit is not deployed it is ignored. Otherwise the coordinates of the
			-- unit in the schematic are stored in unit_position.

			-- Init the unit cursor of the current component:
			unit_cursor := first_unit (component_cursor_lib);

			-- Loop in list of internal units:
			--while unit_cursor_internal /= type_units_library.no_element loop
			while type_units_library."/=" (unit_cursor, type_units_library.no_element) loop
				log_indentation_up;

				-- get the unit name
				unit_name_lib := type_units_library.key (unit_cursor);

				-- Now the unit name serves as key into the unit list we got from the schematic (unit_sch).
				-- If the unit is deployed in the schematic, we load unit_position. 
				-- unit_position holds the position of the unit in the schematic.
				if unit_exists (name => unit_name_lib, units => units_sch) then -- if unit deployed in schematic
					log (text => "unit " & to_string (unit_name_lib), level => log_threshold + 1);
					unit_position := position_of_unit (name => unit_name_lib, units => units_sch); -- pos. in schematic
					log_indentation_up;
					log (text => to_string (position => unit_position), level => log_threshold + 2);

					-- Get the ports of the current unit. Start with the first port of the unit.
					-- The unit_position plus the relative port position (in library) yields the absolute
					-- position of the port (in schematic).

					-- Init port cursor
					port_cursor := first_port (unit_cursor); -- port in library

					-- Loop in port list of the unit:
					while port_cursor /= type_ports_library.no_element loop
						log_indentation_up;
						--log (text => "port " & type_port_name.to_string (key (port_cursor))
						log (text => "port " & to_string (element (port_cursor).name),
								--& " pin/pad " & to_string (element (port_cursor).pin),
							 level => log_threshold + 2);
						
						-- Build a new port and append port to portlist of the 
						-- current component (indicated by component_cursor_portlists).
						add_port;
						
						log_indentation_down;
						port_cursor := next (port_cursor);
					end loop;

					-- SEARCH FOR PORTS OF GLOBAL UNITS. 
					
					-- NOTE: Have a break before trying to understand the following:
					
					-- The problem with ports that are "common to all units" (KiCad terminology) is:
					--  The unit they belong to, does not appear in the schematic, whereas their ports
					--  are visible on each unit (kicad button "show hidden pins").
					-- Solution: We assume all "common to all units" ports belong to all units of the 
					-- component, thus inheriting the unit_name_lib and the unit_position.
					-- The the unit_name_lib and unit_position of the current unit are applied
					-- to the global units.
					ports_of_global_unit;
					
					log_indentation_down;
				end if;

				log_indentation_down;
				unit_cursor := type_units_library.next (unit_cursor);
			end loop;
			
		end extract_ports;

		-- Verifies appearance of schematic component against library component.
		procedure check_appearance_sch_vs_lib is
			use et_symbols;
		begin
-- 			if et_schematic.component_appearance (component_cursor_sch) = 
-- 			   et_libraries.component_appearance (component_cursor_lib) then
-- 			   null; -- fine
			if element (component_cursor_sch).appearance =
			   element (component_cursor_lib).appearance then
			   null; -- fine			   
			else
				-- this should never happen
				log_indentation_down;
				log (ERROR, "comonent appearance mismatch !", console => true);
				-- CS: provide more details on the affected component
				raise constraint_error;
			end if;
		end check_appearance_sch_vs_lib;

		procedure save_portlists is
		-- Save the portlists in the module (indicated by module_cursor).
		-- module_cursor points already there.
			use type_modules;
		
			procedure save (
				module_name	: in type_submodule_name.bounded_string;
				module 		: in out type_module) is
			begin
				module.portlists := portlists;
			end save;
			
		begin -- save_portlists
			log (text => "saving portlists ...", level => log_threshold + 1);
			update_element (
				container	=> modules,
				position	=> module_cursor,
				process 	=> save'access);
		end save_portlists;
		
	begin -- build_portlists
		log_indentation_up;
		log (text => "building portlists ...", level => log_threshold);
		log_indentation_up;

		-- The library contains the coordinates of the ports whereas
		-- the schematic provides the coordinates of the units of a component.
		-- The library coordinates are regarded as relative to the coordinates
		-- provided by the schematic.
		-- These coordinates summed up yield the absolute position of the ports.
		
		-- Loop in component list of schematic. component_cursor_sch points to the 
		-- particular component. 

		-- ALL schematic components are addressed. No distinction between real or virtual parts.

		-- For each component, store a list of its units in units_sch.
		-- This list contains the units found in the schematic with their coordinates.
		-- These coordinates plus the port coordinates (extracted in 
		-- procedure (extract_ports) will later yield the absolute positions of the ports.
		reset_component_cursor (component_cursor_sch);
		while component_cursor_sch /= type_components_schematic.no_element loop
		
			-- log component by its reference		
			component_reference := schematic.component_reference (component_cursor_sch);
			log (text => "reference " & to_string (component_reference), level => log_threshold + 1);
			
			-- Insert component in portlists. for the moment the portlist of this component is empty.
			-- After that the component_cursor_portlists points to the component. This cursor will
			-- later be used to add a port to the portlists.
			type_portlists.insert (
				container	=> portlists,
				key			=> component_reference, -- like R44
				new_item	=> type_ports.empty_list,
				inserted	=> component_inserted, -- obligatory, no further meaning
				position	=> component_cursor_portlists -- points to the portlist being built
				);
			
			-- get the units of the current schematic component (indicated by component_cursor_sch)
			units_sch := units_of_component (component_cursor_sch);

			log_indentation_up;			

			-- log particular library to be searched in.
			log (text => "generic name " 
				& enclose_in_quotes (to_string (element (component_cursor_sch).generic_name)) 
				& " in " 
				& enclose_in_quotes (et_devices.to_string (element (component_cursor_sch).library_name)),
				level => log_threshold + 2);

			-- Set cursor of the generic model in library. If cursor is empty, the component
			-- is not there -> error and abort.
			-- Otherwise cursor points to a matching component -> extract ports
			-- of that component. Procedure extract_ports uses component_cursor_lib.
			component_cursor_lib := find_component (
				library		=> element (component_cursor_sch).library_name, -- like ../lib/transistors.lib
				component	=> element (component_cursor_sch).generic_name); -- like TRANSISTOR_PNP
				
			if component_cursor_lib = type_components_library.no_element then
				-- component not found
				no_generic_model_found (
					reference => key (component_cursor_sch),
					-- like T12					   
											
					library => element (component_cursor_sch).library_name,
					-- like ../lib/transistors.lib
					
					generic_name => element (component_cursor_sch).generic_name);
					-- like TRANSISTOR_PNP or LED
				else
					-- As a safety measure we make sure that the appearance of the component
					-- in the schematic equals that in the library.
					check_appearance_sch_vs_lib;

					extract_ports; -- uses component_cursor_lib
				end if;

			log_indentation_down;
			
			next (component_cursor_sch); -- advance to next component
		end loop;

		log_indentation_down;
-- 		log (text => text => "portlists complete", level => log_threshold);
		log_indentation_down;

		-- Save portlists in the module (indicated by module_cursor).
		-- Why ? The portlists are later essential for netlist generation and ERC.
		save_portlists;
		
		return portlists;
	end build_portlists;
	
	procedure check_open_ports (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about unintentionally left open ports. That are ports without a no_connection_flag.
	-- Must be called AFTER make_netlists !

		use et_symbols;
		use type_modules;
		use et_string_processing;

		procedure query_portlists (
			module_name	: in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_portlists;
			portlist_cursor : type_portlists.cursor := module.portlists.first;

			procedure query_ports (
				component	: in type_device_name;
				ports 		: in type_ports.list) is
				port_cursor : type_ports.cursor := ports.first;
				use type_ports;
				use et_import;
				
-- NOTE: DO NOT REMOVE THE FOLLWING. MIGHT BE REQUIRED SOME DAY.
				
-- 				function no_connection_flag_here return boolean is
-- 				-- returns true once a no_connection_flag has been found at the port coordinates
-- 					no_connection_flag_found : boolean := false;
-- 				
-- 					procedure query_no_connect_flags (
-- 						module_name : in type_submodule_name.bounded_string;
-- 						module : in type_module) is
-- 						use type_no_connection_flags;
-- 						no_connection_flag_cursor : type_no_connection_flags.cursor := module.no_connections.first;
-- 					begin -- query_no_connect_flags
-- 						log (text => "quering no_connection_flags ...", log_threshold + 1);
-- 						while no_connection_flag_cursor /= type_no_connection_flags.no_element loop
-- 
-- 							-- Compare coordinates of port and no_connection_flag. 
-- 							-- On match exit prematurely.
-- 							if element (port_cursor).coordinates = element (no_connection_flag_cursor).coordinates then
-- 								log (text => "match", log_threshold + 1);
-- 								no_connection_flag_found := true;
-- 								exit;
-- 							end if;
-- 
-- 							next (no_connection_flag_cursor);	
-- 						end loop;
-- 					end query_no_connect_flags;
-- 						
-- 				begin -- query_no_connect_flags
-- 					query_element (
-- 						position => module_cursor,
-- 						process => query_no_connect_flags'access);
-- 
-- 					return no_connection_flag_found;
-- 				end no_connection_flag_here;

-- 				function segment_here return boolean is
-- 				-- Returns true if a net segment is placed at the coordinates of the port.
-- 					segment_found : boolean := false; -- to be returned
-- 				
-- 					procedure query_strands (
-- 					-- Query net segments. Exits prematurely once a segment is found.
-- 						module_name : in type_submodule_name.bounded_string;
-- 						module : in type_module) is
-- 						use type_strands;
-- 						strand_cursor : type_strands.cursor := module.strands.first;
-- 
-- 						procedure query_segments (
-- 							strand : in type_strand) is
-- 							use type_net_segments;
-- 							segment_cursor : type_net_segments.cursor := strand.segments.first;
-- 						begin
-- 							while segment_cursor /= type_net_segments.no_element loop
-- 						
-- 								-- Compare the coordinates of the port with the coordinates of the segment:
-- 								if	element (port_cursor).coordinates = element (segment_cursor).coordinates_start or
-- 									element (port_cursor).coordinates = element (segment_cursor).coordinates_end then
-- 
-- 									segment_found := true;
-- 									exit;
-- 								end if;
-- 
-- 								next (segment_cursor);
-- 							end loop;
-- 						end query_segments;
-- 						
-- 					begin -- query_strands
-- 						while (not segment_found) and strand_cursor /= type_strands.no_element loop
-- 
-- 							type_strands.query_element (
-- 								position => strand_cursor,
-- 								process => query_segments'access);
-- 
-- 							next (strand_cursor);
-- 						end loop;
-- 					end query_strands;
-- 				
-- 				begin -- segment_here
-- 					type_modules.query_element (
-- 						position => module_cursor,
-- 						process => query_strands'access);
-- 
-- 					return segment_found;
-- 				end segment_here;
-- DO NOT REMOVE ! END OF BLOCK.

				function connected_by_other_unit return boolean is
				-- Searches the portlist for another port with same name.
				-- Tests if the port is NOT intentionally left open
				-- AND if the port is connected to any net segment. When positive, exits 
				-- prematurely with a return value "true". If no suitable port found, 
				-- returns "false".
					port_cursor_secondary : type_ports.cursor := ports.first;
					use type_port_name;
					use et_schematic;
				begin
					-- search the portlist but skip the port of origin
					while port_cursor_secondary /= type_ports.no_element loop
						if port_cursor_secondary /= port_cursor then -- skip original port
							if element (port_cursor_secondary).name = element (port_cursor).name then

								if 	element (port_cursor_secondary).intended_open = false and
									element (port_cursor_secondary).connected = YES then
									return true;
								end if;
									
							end if;
						end if;
						next (port_cursor_secondary);
					end loop;

					-- no other connected port found
					return false;
				end connected_by_other_unit;

			begin -- query_ports
				-- Test the port if it is NOT intentionally left open AND
				-- if it is not connected to any net segment.
				-- The easiest is to evaluate the flags "intended_open" and "connected".
				-- Those flags have been set while portlist and netlist generation
				-- (See procedures build_portlists and make_netlists).
				-- This method requires those procedures executed previously. Otherwise
				-- the code in comments (see above) can be used to detect no_connection_flags and 
				-- net segments attached to the port.
				while port_cursor /= type_ports.no_element loop

					if element (port_cursor).intended_open = FALSE and -- port intentionally not open
						element (port_cursor).connected = NO then -- port not connected to any net segment

						-- for kicad_v4 we must do something special:
						if et_import.cad_format = kicad_v4 then

							-- Special threatment for ports of global units (power supply ports) requried.
							-- Such ports may be not connected at certain units, yet connected at other units
							-- of the same component. So we search for other ports (in the portlist of the component)
							-- bearing the same name. If one of them is connected things are fine. Otherwise
							-- the power supply port is indeed not connected -> raise alarm and abort.
							-- For ports with other directions, issue a warning.
							if element (port_cursor).direction = POWER_IN then
								if not connected_by_other_unit then
									log (ERROR, "power supply not connected at" 
										& to_string (element (port_cursor).coordinates, et_kicad_coordinates.MODULE)
										& " nor via other units of this component !");
									raise constraint_error;
								end if;
							else
								log (WARNING, "port not connected at" 
									& to_string (element (port_cursor).coordinates, et_kicad_coordinates.MODULE));
							end if;

						else
							log (WARNING, "port not connected at" 
								& to_string (element (port_cursor).coordinates, et_kicad_coordinates.MODULE));
						end if;
					end if;
				
					next (port_cursor);
				end loop;
			end query_ports;
			
		begin -- query_portlists
			-- Search in the portlists for a port that has neither a no_connection_flag attached
			-- nor any net connected.
			while portlist_cursor /= type_portlists.no_element loop
				query_element (
					position	=> portlist_cursor,
					process		=> query_ports'access);
				next (portlist_cursor);
			end loop;
		end query_portlists;

	begin -- check_open_ports
		log (text => "searching unintentionally left open ports ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		module_cursor := type_modules.first (modules);

		-- Process one module after another.
		-- module_cursor points to the module in the modules.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			-- query no_connection_flags of current module and test if any of them
			-- is not placed at a port
			query_element (
				position	=> module_cursor,
				process 	=> query_portlists'access);

			log_indentation_down;
			next (module_cursor);
		end loop;

		log_indentation_down;

	end check_open_ports;

	procedure check_non_deployed_units (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about not deployed units and open ports thereof.
		use et_string_processing;
		use type_modules;

		procedure query_schematic_components (
		-- Queries the schematic components one after another.
		-- Opens the library where the generic model is stored.
		-- The library name is provided by the schematic component.
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is

			use type_components_schematic;
			component_sch : type_components_schematic.cursor := module.components.first;
			library_cursor : type_device_libraries.cursor;

			use type_libraries;
			
			procedure query_library_components (
			-- Queries the generic models stored in the library.
				library		: in et_kicad_general.type_device_library_name.bounded_string;
				components	: in type_components_library.map) is
				use type_components_library;
				component_lib : type_components_library.cursor := components.first;

				procedure query_units_lib (
					component_name	: in type_component_generic_name.bounded_string;
					component 		: in type_component_library) is
					use type_units_library;
					unit : type_units_library.cursor := component.units.first;

					procedure query_units_sch (
						component_name	: in type_device_name;
						component 		: in type_component_schematic) is
						use type_units_schematic;
						unit_cursor : type_units_schematic.cursor := component.units.first;
						unit_deployed : boolean := false;
						use pac_unit_name;
						use et_import;

						function unit_not_deployed return string is begin
							return to_string (key (component_sch)) 
								& " unit " & et_devices.to_string (key (unit))
								& " not deployed !";
						end unit_not_deployed;
		
					begin
						while unit_cursor /= type_units_schematic.no_element loop
							if key (unit_cursor) = key (unit) then
								unit_deployed := true;
								exit;
							end if;
							next (unit_cursor);
						end loop;

						-- If a unit is not deployed we issue warnings or errors depending 
						-- on the add level of the unit. 
						
						-- CS: show not-connected inputs
						
						if not unit_deployed then
-- 							case element (unit).add_level is
-- 
-- 								-- request units usually harbor power supply 
-- 								when et_libraries.request =>
-- 
-- 									-- For CAD formats other thatn kicad_v4 we raise alarm here.
-- 									-- If a unit with add level "request"
-- 									-- is not deployed, we have a serious design error. 
-- 									-- NOTE: kicad_v4 schematics do not contain "request" units as they are 
-- 									-- "common to all units" of a component. So in order to avoid a 
-- 									-- false alarm, seemingly missing "request" are ignored here.
-- 									-- Procdure check_open_ports detects such design errors. 
-- 
-- 									if et_import.cad_format /= kicad_v4 then
-- 										log (ERROR, unit_not_deployed
-- 											& et_schematic.show_danger (et_schematic.no_power_supply));
-- 										raise constraint_error;
-- 									end if;
-- 
-- 								-- raise alarm if "must" unit is missing. there are numerous reasons
-- 								-- for such a unit to be there. So no further advise possible.
-- 								when et_libraries.must =>
-- 									log (ERROR, unit_not_deployed);
-- 									raise constraint_error;
-- 
-- 								-- "can" units may be left non-deployed
-- 								when et_libraries.can =>
-- 									null;
-- 									
-- 								when et_libraries.next | et_libraries.always =>
									log (WARNING, unit_not_deployed
										& et_schematic.show_danger (et_schematic.floating_input));
-- 
-- 								-- CS: special threatment for "always" ?
-- 									
-- 							end case;
						end if;
							
					end query_units_sch;
					
				begin -- query_units_lib
					while unit /= type_units_library.no_element loop
						log (text => "unit " & et_devices.to_string (key (unit)) 
							 --& et_libraries.to_string (element (unit).add_level), level => log_threshold + 2);
							 , level => log_threshold + 2);

						-- For each generic unit the units of the schematic component must be inquired
						-- if any of them matches the current generic unit name:
						query_element (
							position	=> component_sch,
							process		=> query_units_sch'access);
						
						next (unit);
					end loop;
				end query_units_lib;

				use type_component_generic_name;
				generic_model_found : boolean := false; -- goes true once the generic model was found
				
			begin -- query_library_components
				-- Loop in components of library. Once the generic model name matches 
				-- the name provided by the schematic component, the units of the
				-- library component must be queried. Exit prematurely after that
				-- because the same component won't (should not) occur again in the library.
				-- If the component could not be found, raise error and abort.

				log_indentation_up;
				
				while component_lib /= type_components_library.no_element loop
					-- component_lib points to the generic model in the library

					-- Sometimes the generic name in the libarary starts with a tilde.
					-- It must be removed before testing the name.
					if strip_tilde (key (component_lib)) = element (component_sch).generic_name then
				
						query_element (
							position => component_lib,
							process => query_units_lib'access);

						generic_model_found := true;
						exit;
					end if;
						
					--query_element 
					next (component_lib);
				end loop;

				log_indentation_down;
				
				if not generic_model_found then
					no_generic_model_found (
						reference		=> key (component_sch),
						library			=> element (component_sch).library_name,
						generic_name	=> element (component_sch).generic_name);
				end if;
					
			end query_library_components;
			
		begin -- query_schematic_components
			while component_sch /= type_components_schematic.no_element loop

				log (text => to_string (key (component_sch)) & " in " 
					& to_string (element (component_sch).library_name), level => log_threshold + 1);

				-- Set library cursor so that it points to the library of the generic model.
				library_cursor := type_device_libraries.find (
					container	=> tmp_component_libraries, -- the collection of project libraries with generic models
					key 		=> element (component_sch).library_name); -- lib name provided by schematic component
				
				-- Query the library components.
				type_device_libraries.query_element (
					position 	=> library_cursor,
					process 	=> query_library_components'access);
				
				next (component_sch);
			end loop;
		end query_schematic_components;

	begin -- check_non_deployed_units
		log (text => "detecting non-deployed units ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		module_cursor := type_modules.first (modules);

		-- Process one module after another.
		-- module_cursor points to the module in the modules.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			-- query components in schematic
			query_element (
				position	=> module_cursor,
				process		=> query_schematic_components'access);

			log_indentation_down;
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_non_deployed_units;

	function net_count return count_type is
	-- Returns the number of nets of the current module as string.
		count : count_type := 0;
	
		procedure count_nets (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
		begin
			count := type_netlist.length (module.netlist);
		end count_nets;

	begin -- net_count
		type_modules.query_element (
			position	=> module_cursor,
			process		=> count_nets'access);

		return count;
	end net_count;

	function junction_count return count_type is
	-- Returns the number of junctions of the current module as string.
		count : count_type := 0;
	
		procedure count_junctions (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_junctions;
		begin
			count := type_junctions.length (module.junctions);
		end count_junctions;

	begin -- junction_count
		type_modules.query_element (
			position	=> module_cursor,
			process		=> count_junctions'access);

		return count;
	end junction_count;


	function module_count return natural is
	-- Returns the number of modules in the module collection.
		use type_modules;
	begin
		return natural (length (modules));
	end module_count;

	procedure validate_module (
		module_name : in type_submodule_name.bounded_string) is
	-- Tests if the given module exists. Raises error if not existent.
		module_cursor : type_modules.cursor;
		use type_modules;
		use et_string_processing;
	begin
		if find (modules, module_name) = type_modules.no_element then
			log (ERROR, "module " & to_string (module_name)
				 & " does not exist !",
				console => true);
			raise constraint_error;
		end if;
	end validate_module;

	function compare_hierarchic_sheets (left, right : in type_hierarchic_sheet_name) return boolean is
	-- Returns true if left comes before right. If left equals right, the return is false.
		use type_schematic_file_name;
		use type_submodule_name;
	begin
		-- first compare file names
		if left.file > right.file then
			return true;
		elsif left.file < right.file then
			return false;
		else 
		-- if file names are equal, compare sheet names
			if left.name > right.name then
				return true;
			elsif left.name < right.name then
				return false;
			else 
				return false;
			end if;
		end if;

	end compare_hierarchic_sheets;
	
	procedure add_hierarchic_sheet (
	-- Inserts a hierachic sheet in the module (indicated by module_cursor)
		name		: in type_hierarchic_sheet_name;
		gui_sub_mod	: in type_hierarchic_sheet) is

		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_hierarchic_sheets.cursor;

			use et_string_processing;
			
		begin
			module.hierarchic_sheets.insert (
				key			=> name,
				new_item	=> gui_sub_mod,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then
				if log_level >= 1 then
                    null; -- CS: write this procedure:
                    --log (text => "hierachic sheet", console => true);
					--et_schematic.write_gui_submodule_properties (gui_sub_mod => cursor);
				end if;
			else -- not inserted. net already in module -> abort
				null; -- CS: 
				raise constraint_error;
			end if;
		end add;
			
	begin -- add_hierarchic_sheet
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_hierarchic_sheet;


	procedure add_sheet_header (
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string) is

		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			--use et_string_processing;

			-- A sheet header might exist already (due to multiple instancing of a sheet).
			-- The flag header_inserted would assume a false state.
			-- The given header would then not be inserted in the module.sheet_headers list.
			header_inserted : boolean;
			sheet_header_cursor : type_sheet_headers.cursor;
			
		begin -- add
			type_sheet_headers.insert (
				container	=> module.sheet_headers,
				key			=> sheet,
				new_item	=> header,
				inserted	=> header_inserted,
				position	=> sheet_header_cursor
				);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_header
			end if;

		end add;

	begin
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_sheet_header;


	procedure add_frame (
	-- Inserts a drawing frame in the the module (indicated by module_cursor).
	-- As drawing frames are collected in a simple list, the same frame
	-- can be added multiple times.
		frame : in type_frame) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.frames.append (
				new_item	=> frame);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_frame_properties
			end if;

		end add;
	begin
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_frame;


	procedure add_note (note : in type_text) is
	-- Inserts a note in the the module (indicated by module_cursor).
	-- As notes are collected in a simple list, the same note
	-- can be added multiple times.
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.notes.append (
				new_item	=> note);

			if log_level >= 1 then
				null; -- CS: 
				--et_schematic.write_note_properties
			end if;

		end add;
	begin
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_note;

	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
	-- If a component is already in the list, nothing happens.
	-- Components may occur multiple times in the schematic if they 
	-- consist of more than one unit.
	-- CS: This assumption may not apply for all CAE systems. Currently we
	-- consider only kicad. In other cases the "inserted" check (see below) 
	-- must be enabled via an argument.
		reference		: in type_device_name;
		component		: in type_component_schematic;
		log_threshold 	: in et_string_processing.type_log_level) is
		
		procedure add (
			name	: in type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_components_schematic.cursor;

			use et_string_processing;
		begin
			module.components.insert (
				key			=> reference,
				new_item	=> component,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

-- 			if inserted then -- first occurence of component
				write_component_properties (component => cursor, log_threshold => log_threshold + 1);
-- 			else -- not inserted
-- 				null; -- CS: see comment above
				--raise constraint_error;
-- 			end if;
		end add;
	begin
		modules.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_component;
	
	procedure add_unit (
	-- Adds a unit to the given commponent.
		reference		: in type_device_name;
		unit_name		: in et_devices.pac_unit_name.bounded_string;
		unit 			: in type_unit_schematic;
		log_threshold	: in et_string_processing.type_log_level) is

		procedure add (
			reference	: in type_device_name;
			component	: in out type_component_schematic) is

			inserted	: boolean := false;
			cursor		: type_units_schematic.cursor;

			use et_string_processing;
		begin
			component.units.insert (
				key			=> unit_name,
				new_item	=> unit,
				position	=> cursor, -- updates unit_cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then -- fine. unit was inserted successfully
				write_unit_properties (unit => cursor, log_threshold => log_threshold + 1);
			else -- not inserted, unit already in component -> failure
				log (ERROR, "component " & to_string (reference) &
					 " unit " & to_string (unit_name) & " used multiple times !" &
					 " Make sure " & to_string (reference) & " exists ONLY ONCE !",
					console	=> true);

				raise constraint_error;
			end if;
		end add;
		
		procedure locate_component (
			name	: in type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			cursor : type_components_schematic.cursor;
		begin
			cursor := module.components.find (reference);
			-- CS: do something if reference not found
			
			module.components.update_element (
				position	=> cursor,
				process		=> add'access
				);
		end locate_component;
		
	begin
		modules.update_element (
			position	=> module_cursor,
			process		=> locate_component'access
			);
	end add_unit;

	procedure check_junctions (log_threshold : in et_string_processing.type_log_level) is
	-- Verifies that junctions are placed where net segments are connected with each other.
	-- NOTE: make_netlist detects if a junction is missing where a port is connected with a net.
	-- Warns about orphaned junctions.
		use et_string_processing;
		use type_modules;

		procedure query_strands_prim (
		-- Query strands of module.
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_strands;
			strand_cursor_prim : type_strands.cursor := module.strands.first;

			procedure query_segments_prim (
			-- Query segments of strand
				strand : in type_strand) is
				use type_net_segments;
				segment_cursor_prim : type_net_segments.cursor := strand.segments.first;

				type type_junction is record
					expected : boolean := false;
					position : et_kicad_coordinates.type_position;
				end record;

				junction : type_junction;
				
				function find_position_of_expected_junction return type_junction is
				-- Queries strands and segments of module. Tests if start or end point of the 
				-- the primary segment (indicated by segment_cursor_prim) meets 
				-- another segment (port_cursor_secondary) BETWEEN
				-- its start and end point.Exits prematurely when positive. Returns the composite
				-- junction_position.
					junction_position : type_junction;
				
					use type_strands;

					-- start strand query with the first strand of the module.
					strand_cursor_sec : type_strands.cursor := module.strands.first;

					procedure query_segments_sec (
						strand : in type_strand) is
						segment_cursor_sec : type_net_segments.cursor := strand.segments.first;
						use et_schematic.pac_shapes;
						dist : type_distance_point_line;
					begin -- query_segments_sec
						log_indentation_up;
						log (text => "quering segments ...", level => log_threshold + 4);
						log_indentation_up;
						
						while segment_cursor_sec /= type_net_segments.no_element loop
						
							log (text => to_string (type_net_segment_base (element (segment_cursor_sec))), level => log_threshold + 4);
						
							-- Test segments that are on the same path and sheet. It is sufficient
							-- to compare the start coordinates of the segments.
							if same_path_and_sheet (
								element (segment_cursor_prim).coordinates_start,
								element (segment_cursor_sec).coordinates_start) then

								-- CS this is a workaround in order to provide a line for function distance_point_line:
								declare
									type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> type_point (element (segment_cursor_sec).coordinates_start), 
										end_point	=> type_point (element (segment_cursor_sec).coordinates_end));
								begin
									-- If START point of primary segment sits BETWEEN start and end point of secondary segment,
									-- exit prematurely and return the coordinates of the expected junction.
									dist := distance_point_line (
										point 		=> type_point (element (segment_cursor_prim).coordinates_start),
										line		=> line,
										line_range	=> BETWEEN_END_POINTS);
								
									if (not out_of_range (dist)) and distance (dist) = zero then
										junction_position.expected := true;
										junction_position.position := element (segment_cursor_prim).coordinates_start;
										exit;
									end if;

									-- If END point of primary segment sits BETWEEN start and end point of secondary segment,
									-- exit prematurely and return the coordinates of the expected junction.

									dist := distance_point_line (
										point 		=> type_point (element (segment_cursor_prim).coordinates_end),
										line		=> line,
										line_range	=> BETWEEN_END_POINTS);

									if (not out_of_range (dist)) and distance (dist) = zero then
										junction_position.expected := true;
										junction_position.position := element (segment_cursor_prim).coordinates_end;
										exit;
									end if;
								end;
								
							end if;

							next (segment_cursor_sec);
						end loop;

						log_indentation_down;	
						log_indentation_down;
					end query_segments_sec;

				begin -- find_position_of_expected_junction
					log_indentation_up;
					log (text => "quering strands ...", level => log_threshold + 3);
					log_indentation_up;

					-- Query secondary net segments until a junction is expected or until all secondary segments 
					-- are tested. If no junction is expected return junction_position.expected false.
					while (not junction_position.expected) and strand_cursor_sec /= type_strands.no_element loop

						log (text => et_general.to_string (element (strand_cursor_sec).name)
							& " at " 
							& to_string (element (strand_cursor_sec).position, scope => et_kicad_coordinates.MODULE),
							level => log_threshold + 3);
					
						query_element (
							position	=> strand_cursor_sec,
							process		=> query_segments_sec'access);

						next (strand_cursor_sec);
					end loop;

					log_indentation_down;
					log_indentation_down;
					return junction_position;
				end find_position_of_expected_junction;

				function junction_here return boolean is
				-- Returns true if a junction exits at the expected position (junction.position).
					junction_found : boolean := false; -- to be returned
				
					procedure query_junctions (
					-- Query junctions. Exits prematurely once a junction is found.
						module_name : in type_submodule_name.bounded_string;
						module 		: in type_module) is
						use type_junctions;
						junction_cursor : type_junctions.cursor := module.junctions.first;
					begin -- query_junctions
						junction_found := false;
						while junction_cursor /= type_junctions.no_element loop
							-- compare coordinates of junction and expected junction position
							if element (junction_cursor).coordinates = junction.position then
								junction_found := true;
								exit; -- no further search required
							end if;
							next (junction_cursor);	
						end loop;
					end query_junctions;
				
				begin -- junction_here
					type_modules.query_element (
						position	=> module_cursor,
						process 	=> query_junctions'access);

					return junction_found;
				end junction_here;

			begin -- query_segments_prim
				log_indentation_up;
				log (text => "quering segments ...", level => log_threshold + 2);
				log_indentation_up;
				
				while segment_cursor_prim /= type_net_segments.no_element loop
					log (text => to_string (
							type_net_segment_base (element (segment_cursor_prim))), 
						 level => log_threshold + 2);
				
					junction := find_position_of_expected_junction;

					if junction.expected then
						if not junction_here then
							log (WARNING, "missing net junction at " 
							 & to_string (junction.position, et_kicad_coordinates.MODULE));
						end if;
					end if;
					
					next (segment_cursor_prim);
				end loop;

				log_indentation_down;
				log_indentation_down;
			end query_segments_prim;
			
		begin -- query_strands_prim
			log (text => "quering strands ...", level => log_threshold + 1);
			log_indentation_up;
			
			while strand_cursor_prim /= type_strands.no_element loop
			
				log (text => et_general.to_string (element (strand_cursor_prim).name)
					& " at " 
					& to_string (element (strand_cursor_prim).position, scope => et_kicad_coordinates.MODULE),
					level => log_threshold + 1);
			
				-- query segments of current strand
				query_element (
					position	=> strand_cursor_prim,
					process		=> query_segments_prim'access);

				next (strand_cursor_prim);
			end loop;

			log_indentation_down;	
		end query_strands_prim;
		
	begin -- check_junctions
		log (text => "detecting missing net junctions ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			-- query strands of current module
			query_element (
				position	=> module_cursor,
				process 	=> query_strands_prim'access);

			log_indentation_down;
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_junctions;

	procedure check_orphaned_junctions (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about orphaned junctions.
		use type_modules;
		use et_string_processing;

		procedure query_junctions (
		-- Query junctions.
			module_name	: in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_junctions;
			junction_cursor : type_junctions.cursor := module.junctions.first;

			function segment_here return boolean is
			-- Returns true if a net segment is found where the junction sits on.
				segment_found : boolean := false; -- to be returned
			
				procedure query_strands (
				-- Query net segments. Exits prematurely once a strand is found where the junction
				-- sits on.
					module_name : in type_submodule_name.bounded_string;
					module 		: in type_module) is
					use type_strands;
					strand_cursor : type_strands.cursor := module.strands.first;

					procedure query_segments (
					-- Query net segments. Sets the flag segment_found and exits prematurely 
					-- once a segment is found where the junction sits on.
						strand : in type_strand) is
						use type_net_segments;
						segment_cursor : type_net_segments.cursor := strand.segments.first;
					begin
						while segment_cursor /= type_net_segments.no_element loop

							-- Make sure junction and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (element (segment_cursor).coordinates_start, element (junction_cursor).coordinates) then

								declare
									type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> type_point (element (segment_cursor).coordinates_start), 
										end_point	=> type_point (element (segment_cursor).coordinates_end));
								begin
									if on_line (type_point (element (junction_cursor).coordinates), line) then
										segment_found := true;
										exit;
									end if;
								end;

							end if;
								
							next (segment_cursor);
						end loop;
					end query_segments;
					
				begin -- query_strands
					-- Probe strands until a segment has been found or all strands have been processed:
					while (not segment_found) and strand_cursor /= type_strands.no_element loop

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);

						next (strand_cursor);
					end loop;
				end query_strands;
			
			begin -- segment_here
				--log (text => "probing for other segment at " & to_string (port.coordinates, et_coordinates.module));
			
				type_modules.query_element (
					position	=> module_cursor,
					process		=> query_strands'access);

				return segment_found;
			end segment_here;

		begin -- query_junctions
			while junction_cursor /= type_junctions.no_element loop

				if not segment_here then
					log (WARNING, "orphaned net junction at " 
						 & to_string (element (junction_cursor).coordinates, et_kicad_coordinates.MODULE));
				end if;
					
				next (junction_cursor);	
			end loop;
		end query_junctions;
	
	begin -- check_orphaned_junctions
		log (text => "detecting orphaned net junctions ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
		
			-- query strands of current module
			query_element (
				position	=> module_cursor,
				process		=> query_junctions'access);
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_orphaned_junctions;

	procedure check_misplaced_junctions (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about misplaced junctions. A junction is considered as "misplaced" if:
	-- - it is placed at the end of a net segment where no another segment meets 
	-- - it is placed between two net segments where no port sits
	-- - it is placed where no segment is (means somewhere in the void)
		use type_modules;
		use et_string_processing;

		procedure query_junctions (
		-- Query junctions and test net segments and ports at the junction coordinates.
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_junctions;
			junction_cursor : type_junctions.cursor := module.junctions.first;

			function segment_count_here return natural is
			-- Returns the number of segments that meet at the junction coordinates.
				segment_counter : natural := 0; -- to be returned
			
				procedure query_strands (
				-- Query net segments. Exits prematurely once a strand is found where the junction
				-- sits on.
					module_name : in type_submodule_name.bounded_string;
					module 		: in type_module) is
					use type_strands;
					strand_cursor : type_strands.cursor := module.strands.first;

					procedure query_segments (
					-- Query net segments. Sets the flag segment_found and exits prematurely 
					-- once a segment is found where the junction sits on.
						strand : in type_strand) is
						use type_net_segments;
						segment_cursor : type_net_segments.cursor := strand.segments.first;
					begin
						while segment_cursor /= type_net_segments.no_element loop

							-- Make sure junction and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (element (segment_cursor).coordinates_start, element (junction_cursor).coordinates) then

								declare
									type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> type_point (element (segment_cursor).coordinates_start), 
										end_point	=> type_point (element (segment_cursor).coordinates_end));
								begin
									-- count segments
									if on_line (type_point (element (junction_cursor).coordinates), line) then
										segment_counter := segment_counter + 1;
									end if;
								end;

							end if;
								
							next (segment_cursor);
						end loop;
					end query_segments;
					
				begin -- query_strands
					-- Probe strands.
					-- There is no need to probe other strands once a segment was found. For this reason
					-- this loop also tests the segment_counter.
					while segment_counter = 0 and strand_cursor /= type_strands.no_element loop

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);

						next (strand_cursor);
					end loop;
				end query_strands;
			
			begin -- segment_count_here
				type_modules.query_element (
					position	=> module_cursor,
					process		=> query_strands'access);

				return segment_counter;
			end segment_count_here;

			function port_here return boolean is
			-- Return true if a port exists at the position of the junction.
				port_found : boolean := false;

				procedure query_portlists (
				-- Query portlists. Exits prematurely once any port was found.
					module_name : in type_submodule_name.bounded_string;
					module 		: in type_module) is
					use type_portlists;
					portlist_cursor : type_portlists.cursor := module.portlists.first;
					
					procedure query_ports (
					-- Query ports. Exit prematurely once a port was found.
						component	: in type_device_name;
						ports 		: in type_ports.list) is
						port_cursor : type_ports.cursor := ports.first;
						use type_ports;
					begin
						while port_cursor /= type_ports.no_element loop

							if element (port_cursor).coordinates = element (junction_cursor).coordinates then
								port_found := true; -- this would cancel the portlist query loop
								exit;
							end if;
								
							next (port_cursor);
						end loop;
					end query_ports;
					
				begin -- query_portlists. exit prematurely once a port was found 
					while (not port_found) and portlist_cursor /= type_portlists.no_element loop
						query_element (
							position	=> portlist_cursor,
							process		=> query_ports'access);
						next (portlist_cursor);
					end loop;
				end query_portlists;

			begin -- port_here
				query_element (
					position	=> module_cursor,
					process		=> query_portlists'access);
				return port_found;
			end port_here;

			procedure log_misplaced_junction is
			begin
				log (WARNING, "misplaced net junction at " 
					& to_string (element (junction_cursor).coordinates, et_kicad_coordinates.MODULE));
			end log_misplaced_junction;
			
		begin -- query_junctions
			while junction_cursor /= type_junctions.no_element loop

				-- Get the number of net segments at the junction coordinates.
				case segment_count_here is
					when 0 | 1 => log_misplaced_junction;
						-- junction in the void or at the end of a single segment
					
					when 2 =>
						-- junction between two segments -> there should be a port
						if not port_here then 
							log_misplaced_junction;
						end if;

					when others => null;
						-- more than two segments here. nothing wrong.
				end case;
					
				next (junction_cursor);	
			end loop;
		end query_junctions;
		
	begin -- check_misplaced_junctions
		log (text => "detecting misplaced net junctions ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
		
			-- query strands of current module
			query_element (
				position	=> module_cursor,
				process		=> query_junctions'access);
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_misplaced_junctions;
	
	procedure check_misplaced_no_connection_flags (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about no_connection_flags placed at nets.
		use et_string_processing;
		use type_modules;
		
		procedure query_strands (
		-- Query strands and test if no_connection_flags are placed on any segment of the strand.
			module_name : in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_strands;
			strand_cursor : type_strands.cursor := module.strands.first;

			procedure query_segments (
				strand : in type_strand) is
				use type_net_segments;
				segment_cursor : type_net_segments.cursor := strand.segments.first;

				procedure find_no_connection_flag is
				-- Issues a warning if a no_connection_flag sits at the segment.
				
					procedure query_no_connect_flags (
					-- Query junctions. Exits prematurely once a junction is found.
						module_name : in type_submodule_name.bounded_string;
						module 		: in type_module) is
						use type_no_connection_flags;
						no_connection_flag_cursor : type_no_connection_flags.cursor := module.no_connections.first;
					begin -- query_no_connect_flags
						log (text => "quering no_connection_flags ...", level => log_threshold + 4);
						log_indentation_up;
						
						while no_connection_flag_cursor /= type_no_connection_flags.no_element loop

							log (text => to_string (element (no_connection_flag_cursor).coordinates, 
													scope => et_kicad_coordinates.MODULE),
								level => log_threshold + 4);
						
							-- now we have element (segment_cursor) 
							-- and element (no_connection_flag_cursor) to work with

							-- Make sure no_connection_flag and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (
								element (no_connection_flag_cursor).coordinates,
								element (segment_cursor).coordinates_start) then

								declare
									type type_line_scratch is new et_schematic.pac_shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> type_point (element (segment_cursor).coordinates_start), 
										end_point	=> type_point (element (segment_cursor).coordinates_end));
								begin
									if on_line (type_point (element (no_connection_flag_cursor).coordinates), line) then
										log (WARNING, "no-connection-flag misplaced on a net at " 
											& to_string (element (no_connection_flag_cursor).coordinates, et_kicad_coordinates.MODULE));
									end if;

								end;
							end if;

							next (no_connection_flag_cursor);	
						end loop;

						log_indentation_down;
					end query_no_connect_flags;
				
				begin -- find_no_connection_flag
					log_indentation_up;
				
					--log (text => "searching no_connection_flags ...", level => log_threshold + 3);
					-- query no_connection_flags of the module
					type_modules.query_element (
						position	=> module_cursor,
						process 	=> query_no_connect_flags'access);

					log_indentation_down;
				end find_no_connection_flag;
				
			begin -- query_segments
				log_indentation_up;
				log (text => "quering segments ...", level => log_threshold + 2);
				log_indentation_up;
				
				while segment_cursor /= type_net_segments.no_element loop
					log (text => to_string (type_net_segment_base (element (segment_cursor))), level => log_threshold + 2);
				
					-- test if there are any no_connection_flags placed on the segment
					find_no_connection_flag;
					next (segment_cursor);
				end loop;

				log_indentation_down;
				log_indentation_down;
					
			end query_segments;
			
		begin -- query_strands
			log (text => "quering strands ...", level => log_threshold + 1);
			log_indentation_up;
			
			while strand_cursor /= type_strands.no_element loop

				log (text => et_general.to_string (element (strand_cursor).name)
					& " at " 
					& to_string (element (strand_cursor).position, scope => et_kicad_coordinates.MODULE),
					level => log_threshold + 1);
			
				-- query segments of current strand
				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);

				next (strand_cursor);
			end loop;

			log_indentation_down;
		end query_strands;

	begin -- check_misplaced_no_connection_flags
		log (text => "detecting misplaced no-connection-flags ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			-- query strands of current module and check of any misplaced no_connection_flags
			query_element (
				position	=> module_cursor,
				process		=> query_strands'access);

			log_indentation_down;
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_misplaced_no_connection_flags;

	procedure check_orphaned_no_connection_flags (log_threshold : in et_string_processing.type_log_level) is
	-- Warns about orphaned no_connection_flags.

		use type_modules;
		use et_string_processing;
	
		procedure query_no_connect_flags (
		-- Query junctions. Exits prematurely once a junction is found.
			module_name : in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_no_connection_flags;
			no_connection_flag_cursor : type_no_connection_flags.cursor := module.no_connections.first;

			procedure query_portlists (
			-- Query junctions. Exits prematurely once a junction is found.
				module_name : in type_submodule_name.bounded_string;
				module 		: in type_module) is
				use type_portlists;
				portlist_cursor : type_portlists.cursor := module.portlists.first;

				-- As long as no port is detected, we consider the flag as orphaned.
				flag_orphaned : boolean := true;
				
				procedure query_ports (
					component 	: in type_device_name;
					ports 		: in type_ports.list) is
					port_cursor : type_ports.cursor := ports.first;
					use type_ports;
				begin -- query_ports
					-- query ports of component and test if the no_connection_flag is attached to any of them
					while port_cursor /= type_ports.no_element loop

						-- if port and no_connection_flag have the same coordinates then the 
						-- flag is considered as not orphaned -> exit prematurely
						if element (no_connection_flag_cursor).coordinates = element (port_cursor).coordinates then
							flag_orphaned := false;
							exit;
						end if;
							
						next (port_cursor);
					end loop;
				end query_ports;
				
			begin -- query_portlists
				-- Search in the portlists for a port that has the no_connection_flag attached.
				-- The search ends prematurely once such a port was found. As long as
				-- the flag is considered as orphaned the search continues until all portlists
				-- have been searched.
				while flag_orphaned and portlist_cursor /= type_portlists.no_element loop
					query_element (
						position	=> portlist_cursor,
						process		=> query_ports'access);
					next (portlist_cursor);
				end loop;

				-- If the flag is still orphaned issue a warning.
				if flag_orphaned then
					-- no_connection_flag is not placed at any port
					log (WARNING, "orphaned no_connection_flag found at " 
						 & to_string (element (no_connection_flag_cursor).coordinates, et_kicad_coordinates.MODULE));
				end if;
					
			end query_portlists;
			
		begin -- query_no_connect_flags
			log (text => "quering no_connection_flags ...", level => log_threshold + 1);
			while no_connection_flag_cursor /= type_no_connection_flags.no_element loop

				query_element (
					position 	=> module_cursor,
					process 	=> query_portlists'access);

				next (no_connection_flag_cursor);	
			end loop;
		end query_no_connect_flags;

	begin -- check_orphaned_no_connection_flags
		log (text => "detecting orphaned no-connection-flags ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module in the modules.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			-- query no_connection_flags of current module and test if any of them
			-- is not placed at a port
			query_element (
				position	=> module_cursor,
				process 	=> query_no_connect_flags'access);

			log_indentation_down;
			next (module_cursor);
		end loop;

		log_indentation_down;
	end check_orphaned_no_connection_flags;

	function simple_name (net_name : in type_net_name.bounded_string)
		return type_net_name.bounded_string is
	-- Returns the simple name of the given net name.
	-- Example: If the given name is "MOTOR_DRIVER/CLOCK" then the return is "CLOCK".
		position_of_last_separator : natural := 0;
		use et_schematic;
		name : type_net_name.bounded_string;
	begin
		-- Detect position of last hierarchy separator.
		position_of_last_separator := index (net_name, hierarchy_separator, backward);

		-- If the given net name is a simple name already, return it as it is.
		-- Otherwise extract the simple net name from position of hierarchy 
		-- separator until end of net_name.
		if position_of_last_separator > 0 then
			name := to_bounded_string (
				slice (net_name, position_of_last_separator + 1, length (net_name)));
		else
			name := net_name;
		end if;
		
		return name;
	end simple_name;


	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_schematic;
		log_threshold : type_log_level := 2;
	begin
		log_indentation_up;
		
		case label.label_appearance is
			when SIMPLE =>
			log (text => "simple label " & 
				 et_general.to_string (label.text) & " at " & 
				 --to_string (position => label.coordinates)); -- CS log_threshold ?
				 to_string (point => label.coordinates)); -- CS log_threshold ?
				
			when TAG =>
				if label.hierarchic then
					log (text => "hierarchic label " & 
					et_general.to_string (label.text) & " at " &
					--to_string (position => label.coordinates));  -- CS log_threshold ?
					to_string (point => label.coordinates));  -- CS log_threshold ?
				end if;
					
				if label.global then
					log (text => "global label " & et_general.to_string (label.text) & " at " &
					--to_string (position => label.coordinates));  -- CS log_threshold ?
					to_string (point => label.coordinates));  -- CS log_threshold ?
				end if;
					
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log (text => to_string (label.rotation), level => log_threshold + 1);
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_down;
		log_indentation_down;

	end write_label_properties;

	function to_string (label : in type_net_label) return string is
	-- Returns the coordinates of the given label as string.
	begin
		--return (to_string (position => label.coordinates, scope => scope));
		return to_string (point => label.coordinates);
	end to_string;

	function to_string (
		junction	: in type_net_junction;
		scope		: in et_kicad_coordinates.type_scope) 
		return string is
		-- Returns the position of the given junction as string.
	begin	
		return (to_string (position => junction.coordinates, scope => scope));
	end to_string;

	function length (segment : in type_net_segment_base) 
		return et_coordinates.type_distance is
	-- Returns the length of the given net segment.
		len : type_distance;
		use et_string_processing;
	begin
		len := distance_total (segment.coordinates_start, segment.coordinates_end);
		--log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
		return len;
	end length;
	
	function to_string (
		segment	: in type_net_segment_base'class;
		scope 	: in et_kicad_coordinates.type_scope := et_kicad_coordinates.SHEET)
		return string is
	-- Returns the start and end coordinates of the given net segment.
		use et_kicad_coordinates;
	begin
		return (" start"
			& to_string (position => segment.coordinates_start, scope => scope)
			& " end" 
			& to_string (position => segment.coordinates_end, scope => XY));
	end to_string;

	
	function to_string (scope : in type_strand_scope) return string is
	-- Retruns the given scope as string.
	begin
		--return to_lower (type_scope_of_net'image (scope));
		return type_strand_scope'image (scope);
	end to_string;

	
	procedure net_test (log_threshold : in et_string_processing.type_log_level) is
	-- Tests nets for number of inputs, outputs, bidirs, ...
	-- CS: improve test coverage by including component categories like connectors, jumpers, testpads, ...
		use et_string_processing;
		use type_modules;
		use et_symbols;

		procedure query_nets (
			module_name : in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_netlist;
			net_cursor : type_netlist.cursor := module.netlist.first;

			procedure query_ports (
				net_name	: in type_net_name.bounded_string;
				ports 		: in type_ports_with_reference.set) is
				use type_ports_with_reference;
				port_cursor : type_ports_with_reference.cursor := ports.first;

				-- for counting ports by direction
				input_count 	: natural := 0;
				output_count 	: natural := 0;
				power_out_count	: natural := 0;
				bidir_count		: natural := 0;
				weak1_count		: natural := 0;
				weak0_count		: natural := 0;
				-- CS ? power_in_count	: natural := 0; -- CS: test if power_in name matches net name ?

				-- for counting ports by component category
				use et_conventions;
				connector_count	: natural := 0;
				testpoint_count	: natural := 0;
				jumper_count	: natural := 0;
				switch_count	: natural := 0;
				resistor_count	: natural := 0;
				ic_count		: natural := 0;
				others_count	: natural := 0;

				function sum_connectives return natural is begin
					return connector_count + testpoint_count + jumper_count + switch_count;
				end sum_connectives;

				function sum_drivers return natural is begin
					return output_count + bidir_count + weak0_count + weak1_count;
				end sum_drivers;
				
				procedure increment (count : in out natural) is
				begin count := count + 1; end increment;
				
				function show_net return string is begin
					return "net " & et_general.to_string (key (net_cursor));
					-- CS: show coordinates directly ?
				end show_net;

				-- CS: procedure (input parameter port-direction) that loops through the ports 
				-- and outputs them as requested by the input parameter.

			begin -- query_ports

				while port_cursor /= type_ports_with_reference.no_element loop
			
					-- log reference, port and direction (all in one line)
					log (text => "reference " & to_string (element (port_cursor).reference)
						& " port " & to_string (element (port_cursor).name)
						& to_string (element (port_cursor).direction, preamble => true),
						level => log_threshold + 3);

					-- count ports by direction
					case element (port_cursor).direction is
						when INPUT		=> input_count := input_count + 1; -- CS: use increment (see above)
						when OUTPUT		=> output_count := output_count + 1;
						when BIDIR		=> bidir_count := bidir_count + 1;
						when WEAK0		=> weak0_count := weak0_count + 1;
						when WEAK1		=> weak1_count := weak1_count + 1;
						when POWER_OUT	=> power_out_count := power_out_count + 1;
						
						when UNKNOWN	=> -- CS: verification required
							log (ERROR, show_net & " has a port with unknown direction at " 
								& to_string (element (port_cursor).coordinates, scope => et_kicad_coordinates.MODULE)
								& et_schematic.show_danger (et_schematic.not_predictable),
								console => true
								);
							raise constraint_error;

							when others		=> null; -- CS: TRISTATE, PASSIVE, POWER_IN
					end case;

					-- Count ports by component category (address real components only)
					--if element (port_cursor).appearance = et_libraries.sch_pcb then
					--if et_libraries."=" (element (port_cursor).appearance, et_libraries.sch_pcb) then
					if element (port_cursor).appearance = et_symbols.PCB then
						case category (element (port_cursor).reference) is

							-- count "connectives"
							when CONNECTOR			=> increment (connector_count);
							when TESTPOINT			=> increment (testpoint_count);
							when JUMPER				=> increment (jumper_count);
							when SWITCH				=> increment (switch_count);

							-- count resistors
							when RESISTOR			=> increment (resistor_count);
							when RESISTOR_NETWORK	=> increment (resistor_count);
							
							when INTEGRATED_CIRCUIT	=> increment (ic_count);
							
							when others 			=> increment (others_count);
						end case;
					end if;
						
					next (port_cursor);
				end loop;

				-- Test if net has zero OR one single port. Warn about floating inputs:
				case length (ports) is
					when 0 =>
						log (WARNING, "net " & et_general.to_string (key (net_cursor)) & " has no ports !"
							& " See import report for coordinates.");
					
					when 1 =>
						log (WARNING, "net " & et_general.to_string (key (net_cursor)) 
							& " has only one port at "
							& to_string (element (ports.first).coordinates, scope => et_kicad_coordinates.MODULE));

						-- warn about single inputs
						if input_count = 1 then
							log (WARNING, show_net & " has only one input !" & 
							 et_schematic.show_danger (et_schematic.floating_input));
							-- CS: show affected ports and their coordinates. use a loop in ports and show inputs.
						end if;
						
					when others =>
						if sum_drivers = 0 then -- no kind of driver
							if input_count > 0 then -- one or more inputs
								if others_count = 0 then
									log (WARNING, show_net & " has no energy sources !" &
										et_schematic.show_danger (et_schematic.floating_input));
									-- CS: show affected ports and their coordinates. use a loop in ports and show inputs.
								end if;
							end if;
						end if;
				end case;

				-- Test if outputs drive against each other:
				if output_count > 1 then
					log (WARNING, show_net & " has more than one output !" &
						et_schematic.show_danger (et_schematic.contention));
					-- CS: show affected ports and their coordinates. use a loop in ports and show outputs
				end if;

				-- Test if no pull-resistors are connected with bidirs: -- CS: verification required
				if bidir_count > 1 then
					-- For the moment we warn if there are as many bidir as ports. this implies there is nothing that
					-- could pull the net to a definite level:
					if length (ports) = count_type (bidir_count) then
						log (WARNING, show_net & " has no pull resistors !" &
							et_schematic.show_danger (et_schematic.floating_input));
					end if;
				end if;

				-- Test if no pull-down-resistors are connected with weak0 outputs: -- CS: verification required
				if weak0_count > 0 then
					-- For the moment we warn if there are as many weak0 outputs as ports. this implies there is nothing that
					-- could pull the net to a definite level:
					if length (ports) = count_type (weak0_count) then
						log (WARNING, show_net & " has no pull-down resistors !" & 
							et_schematic.show_danger (et_schematic.floating_input));
					end if;
				end if;

				-- Test if no pull-up-resistors are connected with weak1 outputs: -- CS: verification required
				if weak1_count > 0 then
					-- For the moment we warn if there are as many weak1 outputs as ports. this implies there is nothing that
					-- could pull the net to a definite level:
					if length (ports) = count_type (weak1_count) then
						log (WARNING, show_net & " has no pull-up resistors !" &
							et_schematic.show_danger (et_schematic.floating_input));
					end if;
				end if;
				
				-- Test contending weak0 against weak1 outputs -- CS: verification required
				if weak0_count > 0 and weak1_count > 0 then
					log (WARNING, show_net & " has weak0 and weak1 outputs !" &
						et_schematic.show_danger (et_schematic.contention));
				end if;
				
				-- Test if any outputs are connected with a power source
				if power_out_count > 0 then
					if output_count > 0 then -- CS: or bidir_count pull_high pull_low
						log (ERROR, show_net & " has outputs connected with power sources !" &
							 et_schematic.show_danger (et_schematic.short_circuit));
						-- CS: show affected ports and their coordinates. use a loop in ports and show outputs and power outs.
						raise constraint_error;
					end if;
				end if;
				
				-- Test contenting power sources. 
				-- CS: depends on port names
				-- CS: mind power flags
-- 				if power_out_count > 1 then
-- 					log (ERROR, show_net & " has more than one power source !" & show_danger (contention));
-- 					-- CS: show affected ports and their coordinates
-- 					raise constraint_error;
-- 				end if;
						
			end query_ports;
				
		begin -- query_nets
			log_indentation_up;
		
			while net_cursor /= type_netlist.no_element loop
				log (text => et_general.to_string (key (net_cursor)), level => log_threshold + 2);

				log_indentation_up;
				
				query_element (
					position	=> net_cursor,
					process		=> query_ports'access);

				log_indentation_down;
				
				next (net_cursor);
			end loop;
				
			log_indentation_down;
		end query_nets;
		
	begin -- net_test
		log (text => "net test ...", level => log_threshold);
		log_indentation_up;

		-- We start with the first module of the modules.
		--first_module;
		module_cursor := modules.first;

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
		
			-- query nets in netlist
			query_element (
				position	=> module_cursor,
				process 	=> query_nets'access);
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end net_test;

	function connected_net (
		port			: in type_port_of_module; -- contains something like nucleo_core_1 X701 port 4
		log_threshold	: in et_string_processing.type_log_level)
		return type_net_name.bounded_string is
	-- Returns the name of the net connected with the given port.
	-- Searches the netlist of the given module for the given port. 
	-- The net which is connected with the port is the net whose name
	-- is to be returned.
	-- If no net connected with the given port, an empty string is returned.

		use et_string_processing;
		use type_modules;
		use et_symbols;

		module_cursor : type_modules.cursor; -- points to the module being searched in

		net_name_to_return : type_net_name.bounded_string; -- to be returned

		procedure query_nets (
			module_name	: in type_submodule_name.bounded_string;
			module		: in type_module) is
			net_cursor	: type_netlist.cursor;
		
			net_found : boolean := false; -- goes true once a suitable net found (should be only one)
			
			procedure query_ports (
				net_name	: in type_net_name.bounded_string;
				ports		: in type_ports_with_reference.set) is
				port_cursor : type_ports_with_reference.cursor;
				use type_port_name;
				use type_ports_with_reference;
			begin -- query_ports
				log (text => "querying ports ...", level => log_threshold + 2);
				log_indentation_up;

				-- If the net has any ports, search for the given port.
				-- Flag net_found goes true on match which terminates the
				-- loop that picks up the nets (see main of procedure query_nets).
				if not type_ports_with_reference.is_empty (ports) then
					port_cursor := ports.first;
					while port_cursor /= type_ports_with_reference.no_element loop
						log (text => to_string (element (port_cursor)), level => log_threshold + 3); -- show port name

						if element (port_cursor).reference = port.reference then
							if element (port_cursor).name = port.name then
								net_found := true;
								net_name_to_return := net_name;
								exit;
							end if;
						end if;

						next (port_cursor);
					end loop;
				end if;

				log_indentation_down;	
			end query_ports;
			
		begin -- query_nets
			log (text => "querying nets ...", level => log_threshold + 1);
			log_indentation_up;
			
			if not type_netlist.is_empty (module.netlist) then

				-- Loop in nets of module and query ports. Once the given port
				-- was found this loop exits prematurely. Otherwise, the port
				-- is considered as not connected -> issue warning
				net_cursor := module.netlist.first;
				--while not net_found and net_cursor /= et_schematic.type_netlist.no_element loop
				while not net_found and type_netlist."/=" (net_cursor, type_netlist.no_element) loop
				--while not net_found and net_cursor /= type_netlist.no_element loop
					log (text => et_general.to_string (type_netlist.key (net_cursor)), level => log_threshold + 2); -- show net name
					log_indentation_up;
					
					type_netlist.query_element (
						position	=> net_cursor,
						process 	=> query_ports'access);

					log_indentation_down;
					type_netlist.next (net_cursor);
				end loop;

				-- If no port was found, issue warning.
				if not net_found then
					log (WARNING, "module " & to_string (module_name) 
						& " port " & to_string (port.name) & " is not connected with any net !");
				end if;
					
			else
				log (WARNING, "module " & to_string (module_name) & " does not have any nets !");
			end if;

			log_indentation_down;
		end query_nets;
		
	begin -- connected_net
		log (text => "locating in module " & to_string (port.module) & " net connected with " 
			& to_string (port.reference) & " port " & to_string (port.name) & " ...",
			level => log_threshold);
		log_indentation_up;

		module_cursor := find (modules, port.module); -- set the cursor to the module

		-- If module exists, locate the given net in the module.
		-- Otherwise raise alarm and exit.
		if module_cursor /= type_modules.no_element then
			--log (text => to_string (key (module_cursor)), level => log_threshold + 1);
			query_element (
				position	=> module_cursor, 
				process		=> query_nets'access);
			
		else -- module not found
			log (ERROR, "module " & to_string (port.module) & " not found !", console => true);
			raise constraint_error;
		end if;
		
		log_indentation_down;
	
		return net_name_to_return;
	end connected_net;


	
	procedure make_netlists (log_threshold : in et_string_processing.type_log_level) is
	-- Builds the netlists of all modules. 
	-- Currently there is only one module. kicad does not support multiple modules at the same time.
	-- Addresses ALL components both virtual and real. Virtual components are things like GND or VCC symbols.
	-- Virtual components are filtered out on exporting the netlist in a file.
	-- Bases on the portlists and nets/strands information of the module.
	-- Detects if a junction is missing where a port is connected with a net.
	
		use et_string_processing;
		use et_symbols;
		use type_modules;

		function make_netlist return type_netlist.map is
		-- Generates the netlist of the current module (indicated by module_cursor).
		-- module.portlists provide the port coordinates. 
		-- module.nets provides the strands and nets.
		-- With this information we make the netlist of the current module.
		
			-- the netlist being built. it is returnd to the calling unit.
			netlist : type_netlist.map;

			procedure query_nets (
			-- Tests if a net of the given module is connected to any component port.
			-- Creates a net in the netlist (type_module.netlist) with the same name 
			-- as the net being examined (type_module.nets).
			-- Component ports connected with the net are collected in portlist of the 
			-- net being built (see procedure add_port below).
				module_name	: in type_submodule_name.bounded_string;
				module		: in type_module) is

				use type_nets;
				net_cursor 		: type_nets.cursor := module.nets.first; -- points to the net being read
				net_in_netlist	: type_netlist.cursor; -- points to the net being built in the netlist
				net_created		: boolean := false; -- goes true once the net has been created in the netlist
				
				procedure query_strands (
				-- Tests if a strand of the given net is connected to any component port.
					net_name	: in type_net_name.bounded_string;
					net			: in type_net) is
					use type_strands;
					strand_cursor : type_strands.cursor := net.strands.first; -- points to the first strand of the net

					procedure query_segments (strand : in type_strand) is
					-- Tests the net segments of the given strand if they are connected with any component ports.
					-- For every segment, all component ports must be tested.
						use type_net_segments;
						segment : type_net_segments.cursor := strand.segments.first; -- points to the segment being read
						use type_portlists;
						component_cursor : type_portlists.cursor; -- points to the component being read

						procedure query_ports (
						-- Tests the ports of the given component if they sit on the current net segment.
							component	: in type_device_name;
							ports		: in type_ports.list) is
							use type_ports;
							port_cursor : type_ports.cursor := ports.first; -- points to the first port of the component

							procedure mark_port_as_connected is
							-- mark port in portlist as connected
							
								procedure locate_component (
								-- Locates the component within the portlist of the submodule
									module_name	: in type_submodule_name.bounded_string;
									module 		: in out type_module) is
	
									procedure locate_port (
									-- Locates the port of the component
										component	: in type_device_name;
										ports		: in out type_ports.list) is

										procedure mark_it (port : in out type_port) is
										begin
											port.connected := YES;
										end mark_it;
											
									begin -- locate_port
										update_element (
											container	=> ports,
											position	=> port_cursor,
											process		=> mark_it'access);
									end locate_port;
										
								begin -- locate_component 
									type_portlists.update_element (
										container	=> module.portlists,
										position	=> component_cursor,
										process 	=> locate_port'access);
								end locate_component;
									
							begin -- mark_port_as_connected
								-- locate the submodule in the rig
								update_element (
									container	=> modules,
									position	=> module_cursor,
									process		=> locate_component'access);
							end mark_port_as_connected;
							
							procedure add_port (
							-- Adds the port (indicated by cursor "port" to the portlist of the net being built.
								net_name	: in type_net_name.bounded_string;
								ports		: in out type_ports_with_reference.set) is
								inserted : boolean;
								cursor : type_ports_with_reference.cursor;
							begin -- add_port
								-- If a port sits on the point where two segments meet, the same port should be inserted only once.
								-- Thus we have the obligatory flag "inserted". 
								type_ports_with_reference.insert (
									container	=> ports,
									position	=> cursor,
									inserted	=> inserted,
									-- We add the port and extend it with the component reference.
									new_item	=> (element (port_cursor) with component));

								if not inserted then -- port already in net
									log_indentation_up;
									log (text => "already processed -> skipped", level => log_threshold + 3);
									log_indentation_down;
								end if;
							end add_port;

						begin -- query_ports
							while port_cursor /= type_ports.no_element loop

								-- Probe only those ports (in the portlists) which are in the same 
								-- path and at the same sheet as the port.
								-- Probing other ports would be a waste of time.
								if same_path_and_sheet (
									left => strand.position, 
									right => element (port_cursor).coordinates ) then

									--if et_schematic."=" (element (port_cursor).connected, et_schematic.NO) then
									if element (port_cursor).connected = NO then
									
										log_indentation_up;
										log (text => "probing " & to_string (component) 
											& " port " & to_string (element (port_cursor).name)
											& latin_1.space
											& to_string (position => element (port_cursor).coordinates, scope => et_kicad_coordinates.MODULE),
											level => log_threshold + 5);

										-- test if port sits on segment
										if port_connected_with_segment (element (port_cursor), element (segment)) then
											log_indentation_up;
										
											log (text => "connected with " & to_string (component) 
												& " port " & to_string (element (port_cursor).name)
												& latin_1.space
												& to_string (position => element (port_cursor).coordinates, scope => et_kicad_coordinates.MODULE),
												level => log_threshold + 3);
											
											log_indentation_down;

											-- add port to the net being built
											type_netlist.update_element (
												container	=> netlist,
												position	=> net_in_netlist,
												process		=> add_port'access);

											-- Mark the port (in the portlists) as connected.
											-- Why ? A port can be connected to ONLY ONE net. So once it is
											-- detected here, it would be a wast of computing time to 
											-- test if the port is connected to other nets.
											mark_port_as_connected;
										end if;
											
										log_indentation_down;
									end if;
								end if;

								next (port_cursor);
							end loop;
						end query_ports;
						
					begin -- query_segments
						log_indentation_up;
					
						while segment /= type_net_segments.no_element loop

							log (text => "segment " & to_string (
									type_net_segment_base (element (segment))), 
								 level => log_threshold + 4);

							-- reset the component cursor, then loop in the component list 
							component_cursor := module.portlists.first;	-- points to the component being read
							while component_cursor /= type_portlists.no_element loop

								-- query the ports of the component
								type_portlists.query_element (
									position	=> component_cursor,
									process		=> query_ports'access);

								next (component_cursor);
							end loop;
							
							next (segment);
						end loop;
							
						log_indentation_down;	
					end query_segments;

				begin -- query_strands
					log_indentation_up;
				
					while strand_cursor /= type_strands.no_element loop

						-- log strand coordinates
						log (text => "strand " & to_string (element (strand_cursor).position, 
									scope => et_kicad_coordinates.MODULE),
							 level => log_threshold + 3);

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);
				
						next (strand_cursor);
					end loop;
						
					log_indentation_down;
				end query_strands;

			begin -- query_nets
				log_indentation_up;
			
				while net_cursor /= type_nets.no_element loop

					-- log the name of the net being built
					log (text => et_general.to_string (key (net_cursor)), level => log_threshold + 2);
				
					-- create net in netlist
					type_netlist.insert (
						container	=> netlist,
						key 		=> key (net_cursor),
						new_item 	=> type_ports_with_reference.empty_set,
						position 	=> net_in_netlist,
						inserted 	=> net_created);

					-- CS: evaluate flag net_created ?

					-- search for ports connected with the net being built
					query_element (
						position	=> net_cursor,
						process		=> query_strands'access);

					next (net_cursor);
				end loop;

				log_indentation_down;	
			end query_nets;
					
		begin -- make_netlist (NOTE: singluar !)
			query_element (
				position 	=> module_cursor,
				process 	=> query_nets'access);

			return netlist;
		end make_netlist;

		procedure add_netlist (
			module_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
		begin
			module.netlist := make_netlist;
		end add_netlist;

	begin -- make_netlists (note plural !)
		log (text => "building netlists ...", level => log_threshold);
		log_indentation_up;
		
		-- We start with the first module of the modules.
		--first_module;
		module_cursor := type_modules.first (modules);

		-- Process one module after another.
		-- module_cursor points to the module.
		while module_cursor /= type_modules.no_element loop
			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
			log_indentation_up;
			
			update_element (
				container	=> modules,
				position	=> module_cursor,
				process		=> add_netlist'access);

			log (text => "net count total" & count_type'image (net_count), level => log_threshold + 1);
			log_indentation_down;
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;
	

	
	function terminal_count (
		reference		: in type_device_name;
		log_threshold	: in et_string_processing.type_log_level)
		return et_devices.type_terminal_count is
	-- Returns the number of terminals of the given component reference.
	-- Requires module_cursor (global variable) to point to the current module.

		use type_modules;
		use et_string_processing;
		terminals : et_devices.type_terminal_count; -- to be returned

		procedure locate_component_in_schematic (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_components_schematic;
		
			component_cursor: type_components_schematic.cursor;
			
			library_name	: et_kicad_general.type_device_library_name.bounded_string;
			generic_name	: type_component_generic_name.bounded_string;
			package_variant	: pac_package_variant_name.bounded_string;

			library_cursor	: type_device_libraries.cursor;

			procedure locate_component_in_library (
				library_name 	: in et_kicad_general.type_device_library_name.bounded_string;
				components 		: in type_components_library.map) is
				use type_components_library;

				component_cursor : type_components_library.cursor;

				procedure query_variants (
				-- Looks up the list of variants of the component.
					name 		: in type_component_generic_name.bounded_string;
					component 	: in type_component_library) is
					use et_devices.pac_variants;
					use et_import;

					variant_cursor : et_devices.pac_variants.cursor;
				begin -- query_variants
					log (text => "locating variant " & pac_package_variant_name.to_string (package_variant)
						& " ...", level => log_threshold + 3);
					log_indentation_up;

					-- The variant should be found (because the component has been inserted in the library earlier).
					-- Otherwise an exception would occur here:
					variant_cursor := component.variants.find (package_variant);

					-- Get the number of terminals of this package variant
					-- This is achieved by looking up the library (full name provided in package_variant)
					-- and the package name.
					-- If a kicad_v4 project is imported, the library name must be extended by the "pretty" extension.
-- 					if cad_format = kicad_v4 then
-- 						terminals := et_kicad_pcb.terminal_count (
-- 									library_name	=> et_libraries.to_full_library_name (
-- 														et_libraries.to_string (element (variant_cursor).packge.library) -- ../lbr/bel_ic
-- 														& et_kicad_pcb.package_library_directory_extension), -- .pretty
-- 									package_name	=> element (variant_cursor).packge.name);	-- S_SO14
-- 					else
						terminals := et_kicad.pcb.terminal_count (
-- 									library_name	=> element (variant_cursor).packge.library,	-- ../lbr/bel_ic
-- 									package_name	=> element (variant_cursor).packge.name);	-- S_SO14
									element (variant_cursor).package_model);
-- 					end if;
						
					log_indentation_down;	

					exception
						when event:
							others =>
								log_indentation_reset;
								log (text => ada.exceptions.exception_message (event), console => true);
								raise;
					
				end query_variants;
				
			begin -- locate_component_in_library
				log (text => "locating generic component " & to_string (generic_name) 
						& " in library " & et_devices.to_string (library_name) 
						& " ...", level => log_threshold + 2);
				log_indentation_up;

				-- Set the component_cursor right away to the position of the generic component
				component_cursor := components.find (generic_name); -- search for generic name NETCHANGER

				-- If we are importing a kicad_v4 project, the generic name might have not been found.
				-- Why ? The generic component name might have a tilde prepended. So the search must
				-- be started over with a tilde prepended to the generic_name.
				if component_cursor = type_components_library.no_element then
					case et_import.cad_format is
						when et_import.KICAD_V4 =>
							-- search for generic name ~NETCHANGER
							component_cursor := components.find (prepend_tilde (generic_name));
						when others => null; -- CS kicad_v5 ?
					end case;
				end if;
					
				type_components_library.query_element (
					position	=> component_cursor,
					process		=> query_variants'access);

				log_indentation_down;
			end locate_component_in_library;
			
		begin -- locate_component_in_schematic
			log (text => "locating component in schematic ...", level => log_threshold + 1);
			log_indentation_up;

			-- The component cursor is set according to the position of the reference:
			-- NOTE: Assumption is that there is a component with this reference.
			component_cursor := module.components.find (reference);
		
			library_name := element (component_cursor).library_name; -- get library name where the symbol is stored in
			generic_name := element (component_cursor).generic_name; -- get generic component name in the library
			package_variant := element (component_cursor).variant; -- get the package variant name of the component

			-- set library cursor. NOTE: assumption is that there IS a library with this name
			library_cursor := tmp_component_libraries.find (library_name); 

			type_device_libraries.query_element (
				position	=> library_cursor,
				process		=> locate_component_in_library'access);

			log_indentation_down;
		end locate_component_in_schematic;
		
	begin -- terminal_count
		log (text => "fetching terminal count of " & to_string (reference) & " ...", level => log_threshold);
		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> locate_component_in_schematic'access);

		log_indentation_down;

		return terminals;
	end terminal_count;


	function to_terminal (
		port 			: in type_port_with_reference;
		module			: in type_submodule_name.bounded_string; -- the name of the module
		log_threshold 	: in et_string_processing.type_log_level) -- see et_libraries spec
		return et_devices.type_terminal is
	-- Returns the terminal and unit name of the given port in a composite type.
	-- Raises error if given port is of a virtual component (appearance sch).

	-- NOTE: In contrast to Kicad, the terminal name is stored in a package variant. The package variant in
	-- turn is maintained in the symbol component library.
	-- General workflow:
	-- 1. The given port provides the component reference like IC34
	-- 2. look up IC34 in module.components
	-- 3. get library name where the symbol is stored in, generic name like 7400, package name like S_SOT23
	-- 4. look up the library, locate 7400 in library
	-- 5. get package variant
	-- 6. look up given port name and return terminal/unit name

		use et_symbols;
		use type_modules;
		use et_string_processing;
		terminal : et_devices.type_terminal; -- to be returned

		procedure locate_component_in_schematic (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
		
			use type_components_schematic;
			component_cursor: type_components_schematic.cursor;
			
			library_name	: et_kicad_general.type_device_library_name.bounded_string;
			generic_name	: type_component_generic_name.bounded_string;
			package_variant	: pac_package_variant_name.bounded_string;

			--use type_libraries;
			library_cursor	: type_device_libraries.cursor;

			procedure locate_component_in_library (
				library_name 	: in et_kicad_general.type_device_library_name.bounded_string;
				components 		: in type_components_library.map) is
				use type_components_library;

				component_cursor : type_components_library.cursor;

				procedure query_variants (
				-- Looks up the list of variants of the component.
					name 		: in type_component_generic_name.bounded_string;
					component 	: in type_component_library) is
					use et_devices.pac_variants;

					variant_cursor : et_devices.pac_variants.cursor;

					procedure locate_terminal (
						variant_name 	: in pac_package_variant_name.bounded_string;
						variant 		: in et_devices.type_variant) is
						use et_devices.type_terminal_port_map;
						use type_port_name;
						terminal_cursor : et_devices.type_terminal_port_map.cursor := variant.terminal_port_map.first;
						terminal_found : boolean := false;
					begin
						-- Search in terminal_port_map for the given port name.
						-- On first match load the terminal which is a composite of
						-- terminal name and unit name (see spec of type_port_in_port_terminal_map)
						while terminal_cursor /= et_devices.type_terminal_port_map.no_element loop
							if element (terminal_cursor).name = port.name then
								terminal.name := key (terminal_cursor); -- to be returned
								terminal.unit := element (terminal_cursor).unit; -- to be returned
								terminal.port := element (terminal_cursor).name; -- to be returned
								
								terminal_found := true;
								exit;
							end if;

							-- CS: if not terminal_found then ...
							next (terminal_cursor);
						end loop;
					end locate_terminal;
					
				begin -- query_variants
					log (text => "locating variant " & et_devices.to_string (package_variant)
						& " ...", level => log_threshold + 3);
					log_indentation_up;

					-- The variant should be found (because the component has been inserted in the library earlier).
					-- CS Otherwise an exception would occur here:
					variant_cursor := component.variants.find (package_variant);

					et_devices.pac_variants.query_element (
						position	=> variant_cursor,
						process		=> locate_terminal'access);

					log_indentation_down;	
				end query_variants;
				
			begin -- locate_component_in_library
				log (text => "locating generic component " & to_string (generic_name) 
						& " in library " & et_devices.to_string (library_name) 
						& " ...", level => log_threshold + 2);
				log_indentation_up;

				-- Set the component_cursor right away to the position of the generic component
				component_cursor := components.find (generic_name); -- search for generic name NETCHANGER

				-- If we are importing a kicad_v4 project, the generic name might have not been found.
				-- Why ? The generic component name might have a tilde prepended. So the search must
				-- be started over with a tilde prepended to the generic_name.
				if component_cursor = type_components_library.no_element then
					case et_import.cad_format is
						when et_import.kicad_v4 =>
							-- search for generic name ~NETCHANGER
							component_cursor := components.find (prepend_tilde (generic_name));
						when others => null;
					end case;
				end if;
					
				type_components_library.query_element (
					position	=> component_cursor,
					process		=> query_variants'access);

				log_indentation_down;
			end locate_component_in_library;
			
		begin -- locate_component_in_schematic
			log (text => "locating component in schematic ...", level => log_threshold + 1);
			log_indentation_up;

			-- The component cursor is set according to the position of the reference:
			-- NOTE: Assumption is that there is a component with this reference.
			component_cursor := module.components.find (port.reference);
		
			library_name := element (component_cursor).library_name; -- get library name where the symbol is stored in
			generic_name := element (component_cursor).generic_name; -- get generic component name in the library
			package_variant := element (component_cursor).variant; -- get the package variant name of the component

			-- set library cursor. NOTE: assumption is that there IS a library with this name
			library_cursor := tmp_component_libraries.find (library_name); 

			type_device_libraries.query_element (
				position	=> library_cursor,
				process		=> locate_component_in_library'access);

			log_indentation_down;
		end locate_component_in_schematic;
	
	begin -- to_terminal
		log (text => "locating in module " & to_string (module) & " terminal (according to package variant) for " 
			& to_string (port.reference) 
			& " port " & to_string (port.name) & " ...", level => log_threshold);
		log_indentation_up;

		-- Abort if given port is not a real component.
		--if et_libraries."=" (port.appearance, et_libraries.sch_pcb) then -- real component
		if port.appearance = et_symbols.PCB then -- real component			

			query_element (
				position	=> find (modules, module), -- sets indirectly the cursor to the module
				process		=> locate_component_in_schematic'access);
			
		else -- abort
			log (ERROR, to_string (port.reference) 
				& " is a virtual component and thus has no package !");
			raise constraint_error;
		end if;
			
		log_indentation_down;
		return terminal; 
	end to_terminal;

	function connected_net (
	-- Returns the name of the net connected with the given component and terminal.
		module			: in type_submodule_name.bounded_string; -- nucleo_core
		reference		: in type_device_name;	-- IC45
		terminal		: in et_terminals.type_terminal_name.bounded_string; -- E14
		log_threshold	: in et_string_processing.type_log_level)		
		return type_net_name.bounded_string is

		net : type_net_name.bounded_string; -- to be returned

		-- As an intermediate storage place here the module name, the component reference and the port name are stored.
		-- Selector port contains the port name associated with the given terminal name (acc. to. package variant).
		-- Once the port name has been found, this variable is set (see procedure locate_terminal):
		port : type_port_of_module; 

		use et_string_processing;
		use et_symbols;
		use type_modules;

		module_cursor : type_modules.cursor; -- points to the module being searched in

		procedure query_components (
		-- Searches the components of the module for the given reference.
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_components_schematic;
			component_cursor_schematic : type_components_schematic.cursor := module.components.first;

			--package_name : type_component_package_name.bounded_string;

			library_name	: et_kicad_general.type_device_library_name.bounded_string;
			generic_name	: type_component_generic_name.bounded_string;
			package_variant	: pac_package_variant_name.bounded_string;

			library_cursor	: type_device_libraries.cursor;

			procedure locate_component_in_library (
			-- Locates the given component by its generic name in the library.
				library_name 	: in et_kicad_general.type_device_library_name.bounded_string;
				components 		: in type_components_library.map) is

				use type_components_library;
				component_cursor : type_components_library.cursor;

				procedure query_variants (
				-- Looks up the list of variants of the component.
					name 		: in type_component_generic_name.bounded_string;
					component 	: in type_component_library) is
				
					use et_devices.pac_variants;
					variant_cursor : pac_variants.cursor;

					procedure locate_terminal (
					-- Locates the given terminal in the package variant.
						variant_name 	: in pac_package_variant_name.bounded_string;
						variant 		: in et_devices.type_variant) is
						use type_terminal_port_map;
						use type_port_name;
						terminal_cursor : type_terminal_port_map.cursor;
					begin -- locate_terminal
						terminal_cursor := variant.terminal_port_map.find (terminal);
						if terminal_cursor /= type_terminal_port_map.no_element then -- given terminal found

							-- set the intermediate variable "port". see declarations above.
							port.module := connected_net.module; -- the name of the given module
							port.reference := reference; -- the given component reference
							port.name := element (terminal_cursor).name; -- the port name
							
							log (text => "port name " & et_symbols.to_string (port.name), level => log_threshold + 4);
						else
							log (ERROR, "terminal " & et_terminals.to_string (terminal) & " not found !",
								 console => true);
							raise constraint_error;
						end if;
					end locate_terminal;
					
				begin -- query_variants
					log (text => "locating variant " & to_string (package_variant) & " ...", level => log_threshold + 3);
					log_indentation_up;

					variant_cursor := component.variants.find (package_variant);

					-- Locate the given terminal in the variant.
					-- The variant should be found (because the component has been inserted in the library earlier).
					if variant_cursor /= pac_variants.no_element then

						-- locate the given terminal in the package variant
						query_element (
							position 	=> variant_cursor,
							process 	=> locate_terminal'access);

					else
						log (ERROR, "package variant " & to_string (key (variant_cursor)) &
							" not found !", console => true);
						raise constraint_error;
					end if;
					log_indentation_down;	
				end query_variants;
				
			begin -- locate_component_in_library
				log (text => "locating generic component " & to_string (generic_name) 
						& " in library " & to_string (library_name) & " ...", level => log_threshold + 2);
				log_indentation_up;

				-- Set the component_cursor right away to the position of the generic component
				component_cursor := components.find (generic_name); -- search for generic name NETCHANGER

				-- If we are importing a kicad_v4 project, the generic name might have not been found.
				-- Why ? The generic component name might have a tilde prepended. So the search must
				-- be started over with a tilde prepended to the generic_name.
				if component_cursor = type_components_library.no_element then
					case et_import.cad_format is
						when et_import.KICAD_V4 =>
							-- search for generic name ~NETCHANGER
							component_cursor := components.find (prepend_tilde (generic_name));
						when others => null; -- CS kicad_v5 ?
					end case;
				end if;

				if component_cursor /= type_components_library.no_element then
					-- Query the variants of the component in the library.
					type_components_library.query_element (
						position 	=> component_cursor,
						process 	=> query_variants'access);
					
				else -- generic model not found -> abort
					log (ERROR, "generic model for " & to_string (reference) & " not found !", console => true);
					raise constraint_error;
				end if;
					
				log_indentation_down;
			end locate_component_in_library;
				
		begin -- query_components
			log (text => "querying components in schematic ...", level => log_threshold + 1);
			log_indentation_up;

			-- find component with given reference in schematic
			component_cursor_schematic := module.components.find (reference);
			if component_cursor_schematic /= type_components_schematic.no_element then

				library_name := element (component_cursor_schematic).library_name; -- get library name where the symbol is stored in
				generic_name := element (component_cursor_schematic).generic_name; -- get generic component name in the library
				package_variant := element (component_cursor_schematic).variant; -- get the package variant name of the component
				
				-- set library cursor. NOTE: assumption is that there IS a library with this name.
				-- CS: Otherwise an exception would occur here.
				library_cursor := tmp_component_libraries.find (library_name); 

				if type_device_libraries."/=" (library_cursor, type_device_libraries.no_element) then
					type_device_libraries.query_element (
						position	=> library_cursor,
						process		=> locate_component_in_library'access);
				else -- library not found -> abort
					log (ERROR, "library " & et_devices.to_string (library_name) & " not found !", console => true);
					raise constraint_error;
				end if;

			else -- component nof found in schematic -> abort
				log (ERROR, "component " & to_string (reference) & " not found !", console => true);
				raise constraint_error;
			end if;
				
			log_indentation_down;
		end query_components;

	begin -- connected_net
		log (text => "locating in module " & to_string (module) & " net connected with " 
			& to_string (reference) & " terminal " & et_terminals.to_string (terminal) & " ...", level => log_threshold);
		log_indentation_up;

		module_cursor := find (modules, module); -- set the cursor to the module

		-- If module exists, locate the given component in the module.
		-- Otherwise raise alarm and exit.
		if module_cursor /= type_modules.no_element then

			query_element (
				position 	=> module_cursor, 
				process 	=> query_components'access);
			
		else -- module not found
			log (ERROR, "module " & to_string (module) & " not found !", console => true);
			raise constraint_error;
		end if;

		-- There is another function connected_net. It returns the net name 
		-- connected with "port". (Port contains the module name, reference and port name)
		net := connected_net (port, log_threshold + 1);
		
		log_indentation_down;
		
		return net;
	end connected_net;

	function components_in_net (
		module 			: in type_submodule_name.bounded_string; -- nucleo_core
		net				: in type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return type_ports_with_reference.set is
	-- Returns a list of component ports that are connected with the given net.

		use et_string_processing;
		use et_symbols;
		use type_modules;

		module_cursor : type_modules.cursor;
		
		ports : type_ports_with_reference.set; -- to be returned

		procedure locate_net (
		-- Locates the given net in the netlist of the given module.
		-- The ports connected with the net are copied to variable "ports".
			module_name : in type_submodule_name.bounded_string;
			module 		: in type_module) is
			net_cursor 	: type_netlist.cursor;
			port_cursor : type_ports_with_reference.cursor;
			port 		: type_port_with_reference;
			terminal 	: et_devices.type_terminal;
			port_count 	: count_type;
		begin
			log (text => "locating net ... ", level => log_threshold + 1);
			log_indentation_up;
			net_cursor := type_netlist.find (module.netlist, net);

			-- If net exists in module load ports with all the ports
			-- connected with the net. Otherwise raise alarm and abort.
			if type_netlist."/=" (net_cursor, type_netlist.no_element) then
				--log (to_string (key (net_cursor)), level => log_threshold + 2);

				-- copy ports of net to "ports" (which is returned to the caller)
				ports := type_netlist.element (net_cursor);
				port_count := type_ports_with_reference.length (ports);

				-- show component ports, units, coordinates and terminal names
				if log_level > log_threshold + 2 then
					log_indentation_up;
					log (text => "listing of" & count_type'image (port_count) & " component ports");
					log_indentation_up;

					-- If there are ports in the given net, set port cursor to first port in net
					-- and log ports one after another.
					-- If no ports in net, issue a warning.
					if not type_ports_with_reference.is_empty (ports) then
						port_cursor := ports.first;
						--while port_cursor /= type_ports_with_reference.no_element loop
						while type_ports_with_reference."/=" (port_cursor, type_ports_with_reference.no_element) loop
							port := type_ports_with_reference.element (port_cursor); -- load the port

							-- Depending on the appearance of the component we output just the 
							-- port name or both the terminal name and the port name.
							case port.appearance is
								when et_symbols.PCB =>
									terminal := to_terminal (port, module_name, log_threshold + 3); -- fetch the terminal
									log (text => to_string (port => port) 
										& et_devices.to_string (terminal, show_unit => true, preamble => true));

								when VIRTUAL =>
									log (text => to_string (port => port));
							end case;
								
							type_ports_with_reference.next (port_cursor);
						end loop;
					else
						log (WARNING, "net " & et_general.to_string (net) & " is not connected with any ports !");
					end if;

					log_indentation_down;
					log_indentation_down;
				end if;
					
			else -- net does not exist -> abort
				log (ERROR, "in module " 
					 & to_string (module_name) & " net " & et_general.to_string (net) 
					 & " not found !", console => true);
				raise constraint_error;
			end if;

			log_indentation_down;
		end locate_net;
			
	begin -- components_in_net
		log (text => "locating components in module " & to_string (module) & " net " &
			 et_general.to_string (net) & " ...",
			level => log_threshold);
		log_indentation_up;

		module_cursor := find (modules, module); -- set the cursor to the module

		-- If module exists, locate the given net in the module.
		-- Otherwise raise alarm and exit.
		if module_cursor /= type_modules.no_element then
			query_element (
				position	=> module_cursor, 
				process		=> locate_net'access);
			
		else -- module not found
			log (ERROR, "module " & to_string (module) & " not found !", console => true);
			raise constraint_error;
		end if;
		
		log_indentation_down;
		return ports;
	end components_in_net;


	function real_components_in_net (
		module 			: in type_submodule_name.bounded_string; -- nucleo_core
		net				: in type_net_name.bounded_string; -- motor_on_off
		log_threshold	: in et_string_processing.type_log_level)
		return type_ports_with_reference.set is
	-- Returns a list of real component ports that are connected with the given net.

		use et_string_processing;
		use et_symbols;
		use type_modules;

		module_cursor : type_modules.cursor;
		
		ports_real : type_ports_with_reference.set; -- to be returned

		procedure locate_net (
		-- Locates the given net in the netlist of the given module.
		-- The ports connected with the net are copied to variable "ports".
			module_name : in type_submodule_name.bounded_string;
			module 		: in type_module) is
			net_cursor	: type_netlist.cursor;
			port_cursor : type_ports_with_reference.cursor;
			ports_all 	: type_ports_with_reference.set; -- all ports of the net
			port 		: type_port_with_reference;
			terminal 	: et_devices.type_terminal;
		begin
			log (text => "locating net ... ", level => log_threshold + 1);
			log_indentation_up;
			net_cursor := type_netlist.find (module.netlist, net);

			-- If net exists in module load ports with all the ports
			-- connected with the net. Otherwise raise alarm and abort.
			if type_netlist."/=" (net_cursor, type_netlist.no_element) then

				-- load all ports of the net
				ports_all := type_netlist.element (net_cursor);

				-- If there are ports in the given net, set port cursor to first port in net,
				-- loop in list of all ports and filter out the real ports.
				if not type_ports_with_reference.is_empty (ports_all) then
					port_cursor := ports_all.first;
					while type_ports_with_reference."/=" (port_cursor, type_ports_with_reference.no_element) loop
						port := type_ports_with_reference.element (port_cursor); -- load the port
					
						if port.appearance = et_symbols.PCB then
							ports_real.insert (port); -- insert real port in list to be returned

							-- log terminal
							terminal := to_terminal (port, module_name, log_threshold + 2); -- fetch the terminal
							log (text => to_string (port) & et_devices.to_string (
									terminal, show_unit => true, preamble => true),
								 level => log_threshold + 2);
						end if;
							
						type_ports_with_reference.next (port_cursor);
					end loop;
				else
					log (WARNING, "net " & et_general.to_string (net) & " is not connected with any ports !");
				end if;
					
			else -- net does not exist -> abort
				log (ERROR, "in module " 
					 & to_string (module_name) & " net " & et_general.to_string (net) 
					 & " not found !", console => true);
				raise constraint_error;
			end if;

			log_indentation_down;
		end locate_net;
			
	begin -- real_components_in_net
		log (text => "locating real components in module " & to_string (module) & " net " &
			 et_general.to_string (net) & " ...",
			 level => log_threshold);
		log_indentation_up;

		module_cursor := find (modules, module); -- set the cursor to the module

		-- If module exists, locate the given net in the module.
		-- Otherwise raise alarm and exit.
		if module_cursor /= type_modules.no_element then
			query_element (
				position	=> module_cursor, 
				process		=> locate_net'access);
			
		else -- module not found
			log (ERROR, "module " & to_string (module) & " not found !", console => true);
			raise constraint_error;
		end if;
		
		log_indentation_down;
		return ports_real;
	end real_components_in_net;

-- 	procedure multiple_purpose_warning ( -- CS move to et_schematic or et_project
-- 	-- Outputs a warning message on multiple usage of a purpose of a component category.
-- 		category		: in et_conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose 		: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		
-- 		use et_string_processing;
-- 		use et_libraries;
-- 		use et_kicad.type_modules;
-- 		use et_conventions;
-- 		
-- 		procedure locate_component (
-- 		-- Searches the component list of the module for a connector with the given purpose.
-- 			module_name : in type_submodule_name.bounded_string;
-- 			module		: in et_kicad.type_module) is
-- 			use et_kicad.type_components_schematic;
-- 			use type_component_purpose;
-- 			component : et_kicad.type_components_schematic.cursor := module.components.first;
-- 		begin
-- 			--log (text => "purpose already used by component");
-- 			log_indentation_up;
-- 
-- 			while component /= et_kicad.type_components_schematic.no_element loop
-- 				if element (component).appearance = sch_pcb then -- it must be a real component
-- 					if et_conventions.category (key (component)) = category then -- category must match
-- 						if element (component).purpose = purpose then -- purpose must match
-- 							log (text => "purpose already used by component " &
-- 								 et_libraries.to_string (key (component)));
-- 						end if;
-- 					end if;
-- 				end if;
-- 				next (component);
-- 			end loop;
-- 
-- 			log_indentation_down;
-- 		end locate_component;
-- 			
-- 	begin -- multiple_purpose_warning
-- -- 		log (ERROR, "There must be ONLY ONE" 
-- -- 			 & to_string (category) 
-- -- 			 & " with purpose " 
-- -- 			 & enclose_in_quotes (et_libraries.to_string (purpose)) & " !",
-- -- 			 console => true);
-- 
-- 		log (message_warning & "There must be ONLY ONE" 
-- 			 & to_string (category) 
-- 			 & " with purpose " 
-- 			 & enclose_in_quotes (et_libraries.to_string (purpose)) & " !");
-- 
-- 		query_element (
-- 			position	=> et_kicad.module_cursor,
-- 			process		=> locate_component'access);
-- 
-- 		--raise constraint_error;
-- 	end multiple_purpose_warning;
		
	
-- 	function multiple_purpose ( -- CS move to et_schematic or et_project
-- 	-- Returns the number of occurences of components with the given purpose and category.
-- 	-- Example: If there are two connectors with purpose "PWR_IN" the return is 2.
-- 		category 		: in et_conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose 		: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold 	: in et_string_processing.type_log_level)
-- 		return natural is
-- 
-- 		occurences : natural := 0; -- to be returned
-- 
-- 		use et_kicad.type_modules;
-- 		use et_libraries;
-- 		use et_conventions;
-- 		
-- 		procedure locate_component (
-- 		-- Searches the component list of the module for a connector with the given purpose.
-- 		-- Exits on the first matching connector. There should not be any others.
-- 			module_name : in type_submodule_name.bounded_string;
-- 			module 		: in et_kicad.type_module) is
-- 			use et_kicad.type_components_schematic;
-- 			use type_component_purpose;
-- 			component : et_kicad.type_components_schematic.cursor := module.components.first;
-- 		begin -- locate_component
-- 			log_indentation_up;		
-- 			log ("detecting multiple usage of purpose " 
-- 				 & enclose_in_quotes (et_libraries.to_string (purpose)) 
-- 				 & " in component category " & to_string (category) 
-- 				 & " ...", level => log_threshold);
-- 			log_indentation_up;
-- 
-- 			while component /= et_kicad.type_components_schematic.no_element loop
-- 				if element (component).appearance = sch_pcb then -- it must be a real component
-- 					if et_conventions.category (key (component)) = category then -- category must match
-- 						if element (component).purpose = purpose then -- purpose must match
-- 							log (et_libraries.to_string (key (component)), level => log_threshold + 1);
-- 							occurences := occurences + 1;
-- 						end if;
-- 					end if;
-- 				end if;
-- 				next (component);
-- 			end loop;
-- 
-- 			log_indentation_down;
-- 			log_indentation_down;
-- 		end locate_component;
-- 
-- 	begin -- multiple_purpose
-- 
-- 		query_element (
-- 			position	=> et_kicad.module_cursor,
-- 			process		=> locate_component'access);
-- 
-- 		-- Show the result of the search:
-- 		if occurences = 0 then
-- 			log_indentation_up;
-- 			log ("none found. very good.", level => log_threshold + 1);
-- 			log_indentation_down;
-- 		else
-- 			log (message_warning & "for component category" 
-- 				& to_string (category) 
-- 				& " the purpose " 
-- 				& enclose_in_quotes (et_libraries.to_string (purpose)) 
-- 				& " is used multiple times !");
-- 			-- CS: show the affected components by reference and coordinates
-- 		end if;
-- 		
-- 		return occurences;
-- 	end multiple_purpose;

	
-- STATISTICS

-- 	procedure make_statistics (log_threshold : in et_string_processing.type_log_level) is
-- 	
-- 		arrow : constant string (1..4) := " -> ";
-- 
-- 		use et_string_processing;		
-- 		
-- 		procedure count_components (
-- 			name	: in type_submodule_name.bounded_string;
-- 			module	: in type_module) is
-- 
-- 			use type_components_schematic;		
-- 			component : type_components_schematic.cursor := module.components.first;
-- 			
-- 			use et_conventions;
-- 
-- 			procedure log_component (mounted : in boolean := true) is
-- 			-- This is for logging mounted or not mounted components.
-- 			begin
-- 				if mounted then
-- 					log (text => et_libraries.to_string (key (component)) 
-- 						& arrow & et_libraries.to_string (element (component).appearance, verbose => true),
-- 						level => log_threshold + 1);
-- 				else
-- 					log (text => et_libraries.to_string (key (component)) 
-- 						& arrow & et_libraries.to_string (element (component).appearance, verbose => true) 
-- 						& arrow & not_mounted,
-- 						level => log_threshold + 1);
-- 				end if;
-- 			end log_component;
-- 			
-- 		begin -- count_components
-- 			-- total number of components
-- 			et_schematic.statistics_set (cat => et_schematic.COMPONENTS_TOTAL, increment => false, number => module.components.length);
-- 
-- 			-- count virtual and real components. real components are separated by
-- 			-- the fact if they are mounted or not.
-- 			while component /= type_components_schematic.no_element loop
-- 
-- 				case type_components_schematic.element (component).appearance is
-- 					when et_libraries.sch => -- virtual
-- 						log_component;
-- 						et_schematic.statistics_set (et_schematic.COMPONENTS_VIRTUAL);
-- 
-- 					when et_libraries.sch_pcb => -- real
-- 						et_schematic.statistics_set (et_schematic.COMPONENTS_REAL);
-- 
-- 						-- count components by category
-- 						case category (key (component)) is
-- 							when CAPACITOR | CAPACITOR_ADJUSTABLE 	=> et_schematic.statistics_set (et_schematic.CAPACITORS);
-- 							when CONNECTOR 							=> et_schematic.statistics_set (et_schematic.CONNECTORS);
-- 							when DIODE | DIODE_PHOTO 				=> et_schematic.statistics_set (et_schematic.DIODES);
-- 							when INDUCTOR | INDUCTOR_ADJUSTABLE 	=> et_schematic.statistics_set (et_schematic.INDUCTORS);
-- 							when INTEGRATED_CIRCUIT 				=> et_schematic.statistics_set (et_schematic.INTEGRATED_CIRCUITS);
-- 							when JUMPER 							=> et_schematic.statistics_set (et_schematic.JUMPERS);
-- 							when LIGHT_EMMITTING_DIODE | LIGHT_EMMITTING_DIODE_ARRAY 
-- 																	=> et_schematic.statistics_set (et_schematic.LEDS);
-- -- 							when NETCHANGER 						=> et_schematic.statistics_set (et_schematic.NETCHANGERS);
-- 							when RELAY 								=> et_schematic.statistics_set (et_schematic.RELAYS);
-- 							when RESISTOR | RESISTOR_ADJUSTABLE | RESISTOR_NETWORK | RESISTOR_PHOTO | POTENTIOMETER 
-- 																	=> et_schematic.statistics_set (et_schematic.RESISTORS);
-- 							when TESTPOINT 							=> et_schematic.statistics_set (et_schematic.TESTPOINTS);
-- 							when TRANSISTOR | TRANSISTOR_PHOTO 		=> et_schematic.statistics_set (et_schematic.TRANSISTORS);
-- 							
-- 							when others => null;
-- 						end case;
-- 
-- 						-- count mounted components
-- 						log_component;
-- 						et_schematic.statistics_set (et_schematic.COMPONENTS_MOUNTED);
-- 				end case;
-- 
-- 				next (component);
-- 			end loop;
-- 				
-- 		end count_components;
-- 
-- 		procedure count_ports (
-- 			name	: in type_submodule_name.bounded_string;
-- 			module	: in type_module) is
-- 
-- 			use type_portlists;
-- 			portlist : type_portlists.cursor := module.portlists.first;
-- 
-- 			procedure count (
-- 				component	: in type_device_name;
-- 				ports		: in type_ports.list) is
-- 				port : type_ports.cursor := ports.first;
-- 				use type_ports;
-- 			begin
-- 				-- loop through the ports of the given component
-- 				-- and count those which are connected.
-- 				while port /= type_ports.no_element loop
-- 					--if et_schematic."=" (element (port).connected, et_schematic.YES) then
-- 					if element (port).connected = YES then
-- 						et_schematic.statistics_set (et_schematic.PORTS_TOTAL);
-- 						-- CS: log port
-- 					end if;
-- 					next (port);
-- 				end loop;
-- 			end count;
-- 				
-- 		begin -- count_ports
-- 			while portlist /= type_portlists.no_element loop
-- 				query_element (
-- 					position	=> portlist,
-- 					process		=> count'access);
-- 				
-- 				next (portlist);
-- 			end loop;
-- 		end count_ports;
-- 
-- 	begin -- make_statistics
-- 
-- 		-- count components
-- 		type_modules.query_element (
-- 			position	=> module_cursor,
-- 			process		=> count_components'access
-- 			);
-- 
-- 		-- count ports
-- 		type_modules.query_element (
-- 			position	=> module_cursor,
-- 			process		=> count_ports'access
-- 			);
-- 
-- 		-- count nets
-- 		et_schematic.statistics_set (cat => et_schematic.NETS_TOTAL, number => net_count, increment => false);
-- 
-- 		-- count junctions
-- 		et_schematic.statistics_set (cat => et_schematic.JUNCTIONS, number => junction_count, increment => false);
-- 		
-- 		--return statistics;
-- 	end make_statistics;


-- 	procedure write_statistics (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Generates the statistics on components and nets of the modules.
-- 	-- Distinguishes between CAD and CAM related things.
-- 		statistics_file_name_cad	: et_schematic.type_statistic_file_name.bounded_string;
-- 		statistics_file_name_cam	: et_schematic.type_statistic_file_name.bounded_string;
-- 		
-- 		statistics_handle_cad	: ada.text_io.file_type;
-- 		statistics_handle_cam	: ada.text_io.file_type;
-- 
-- 		--statistics : type_statistics;
-- 		
-- 		use ada.directories;
-- 		use et_general;
-- 		use type_modules;
-- 		use et_string_processing;
-- 		use et_export;
-- 
-- 	begin -- write_statistics
-- 		--first_module;
-- 		module_cursor := first (modules);
-- 		
-- 		log (text => "writing statistics ...", level => log_threshold);
-- 		log_indentation_up;
-- 		
-- 		while module_cursor /= type_modules.no_element loop
-- 			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
-- 			log_indentation_up;
-- 
-- 			-- CAD
-- 			create_project_directory (to_string (key (module_cursor)), log_threshold + 2);			
-- 			-- compose the CAD statistics file name and its path like "../ET/motor_driver/motor_driver.stat"
-- 			statistics_file_name_cad := et_schematic.type_statistic_file_name.to_bounded_string 
-- 				(
-- 				compose (
-- 					containing_directory	=> compose (work_directory, to_string (key (module_cursor))),
-- 					name 					=> to_string (key (module_cursor)),
-- 					extension 				=> et_schematic.extension_statistics)
-- 				);
-- 
-- 			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
-- 			log (text => "creating CAD statistics file " &
-- 				 et_schematic.type_statistic_file_name.to_string (statistics_file_name_cad), level => log_threshold + 1);
-- 			
-- 			create (
-- 				file => statistics_handle_cad,
-- 				mode => out_file, 
-- 				name => et_schematic.type_statistic_file_name.to_string (statistics_file_name_cad));
-- 
-- 			log_indentation_up;
-- 			put_line (statistics_handle_cad, comment_mark & " " & et_general.system_name & " CAD statistics");
-- 			put_line (statistics_handle_cad, comment_mark & " " & date);
-- 			put_line (statistics_handle_cad, comment_mark & " module " & to_string (key (module_cursor)));
-- 			put_line (statistics_handle_cad, comment_mark & " " & row_separator_double);
-- 
-- 			make_statistics (log_threshold + 1);
-- 			
-- 			-- components
-- 			put_line (statistics_handle_cad, "components");
-- 			put_line (statistics_handle_cad, " total      " & et_schematic.statistics_query (et_schematic.COMPONENTS_TOTAL));
-- 			put_line (statistics_handle_cad, " real       " & et_schematic.statistics_query (et_schematic.COMPONENTS_REAL)); 
-- 			put_line (statistics_handle_cad, " mounted    " & et_schematic.statistics_query (et_schematic.COMPONENTS_MOUNTED));
-- 			put_line (statistics_handle_cad, " virtual    " & et_schematic.statistics_query (et_schematic.COMPONENTS_VIRTUAL));
-- 
-- 			-- As for the total number of ports, we take all ports into account (inc. virtual ports 
-- 			-- of virtual components like GND symbols).
-- 			new_line (statistics_handle_cad);
-- 			put_line (statistics_handle_cad, "ports       " & et_schematic.statistics_query (et_schematic.PORTS_TOTAL));
-- 			-- nets
-- 			put_line (statistics_handle_cad, "nets        " & et_schematic.statistics_query (et_schematic.NETS_TOTAL));
-- 			put_line (statistics_handle_cad, "junctions   " & et_schematic.statistics_query (et_schematic.JUNCTIONS));
-- 			new_line (statistics_handle_cad);
-- 			
-- 			-- components by category
-- 			put_line (statistics_handle_cad, "capacitors  " & et_schematic.statistics_query (et_schematic.CAPACITORS));
-- 			put_line (statistics_handle_cad, "connectors  " & et_schematic.statistics_query (et_schematic.CONNECTORS));
-- 			put_line (statistics_handle_cad, "diodes      " & et_schematic.statistics_query (et_schematic.DIODES));
-- 			put_line (statistics_handle_cad, "inductors   " & et_schematic.statistics_query (et_schematic.INDUCTORS));
-- 			put_line (statistics_handle_cad, "ICs         " & et_schematic.statistics_query (et_schematic.INTEGRATED_CIRCUITS));
-- 			put_line (statistics_handle_cad, "jumpers     " & et_schematic.statistics_query (et_schematic.JUMPERS));
-- 			put_line (statistics_handle_cad, "LEDs        " & et_schematic.statistics_query (et_schematic.LEDS));
-- -- 			put_line (statistics_handle_cad, "netchangers " & et_schematic.statistics_query (et_schematic.NETCHANGERS));
-- 			put_line (statistics_handle_cad, "relays      " & et_schematic.statistics_query (et_schematic.RELAYS));
-- 			put_line (statistics_handle_cad, "resistors   " & et_schematic.statistics_query (et_schematic.RESISTORS));
-- 			put_line (statistics_handle_cad, "testpoints  " & et_schematic.statistics_query (et_schematic.TESTPOINTS));
-- 			put_line (statistics_handle_cad, "transistors " & et_schematic.statistics_query (et_schematic.TRANSISTORS));
-- 			
-- 
-- 			-- finish statistics			
-- 			new_line (statistics_handle_cad);
-- 			put_line (statistics_handle_cad, comment_mark & " " & row_separator_single);
-- 			put_line (statistics_handle_cad, comment_mark & " end of list");
-- 			log_indentation_down;
-- 			close (statistics_handle_cad);
-- 
-- 
-- 
-- 			-- CAM
-- 			-- compose the CAM statistics file name and its path like "../ET/motor_driver/CAM/motor_driver.stat"
-- 			statistics_file_name_cam := et_schematic.type_statistic_file_name.to_bounded_string 
-- 				(
-- 				compose (
-- 					containing_directory => compose 
-- 						(
-- 						containing_directory	=> compose (work_directory, to_string (key (module_cursor))),
-- 						name					=> et_export.directory_cam
-- 						),
-- 					name				=> to_string (key (module_cursor)),
-- 					extension			=> et_schematic.extension_statistics)
-- 				);
-- 
-- 			-- create the statistics file (which inevitably and intentionally overwrites the previous file)
-- 			log (text => "CAM statistics file " &
-- 				 et_schematic.type_statistic_file_name.to_string (statistics_file_name_cam), level => log_threshold + 2);
-- 			
-- 			create (
-- 				file => statistics_handle_cam,
-- 				mode => out_file, 
-- 				name => et_schematic.type_statistic_file_name.to_string (statistics_file_name_cam));
-- 
-- 			log_indentation_up;
-- 			put_line (statistics_handle_cam, comment_mark & " " & et_general.system_name & " CAM statistics");
-- 			put_line (statistics_handle_cam, comment_mark & " " & date);
-- 			put_line (statistics_handle_cam, comment_mark & " module " & to_string (key (module_cursor)));
-- 			put_line (statistics_handle_cam, comment_mark & " " & row_separator_double);
-- 
-- 			-- components
-- 			put_line (statistics_handle_cam, "components");
-- 			put_line (statistics_handle_cam, " total   " & et_schematic.statistics_query (et_schematic.COMPONENTS_MOUNTED));
-- 
-- 			-- As for the total number of real component ports, we take all ports into account for which a physical
-- 			-- pad must be manufactured. Here it does not matter if a component is to be mounted or not, if a pin is connected or not.
-- 			-- CS: THT/SMD
-- 			-- CS: THT/SMD/pins/pads
-- 			-- CS: ressitors, leds, transitors, ...
-- 			new_line (statistics_handle_cam);
-- 
-- 			-- nets
-- 			put_line (statistics_handle_cam, "nets");
-- 			put_line (statistics_handle_cam, " total   " & et_schematic.statistics_query (et_schematic.NETS_TOTAL));
-- 			-- CS: ports of mounted components ? Could be useful for test generation like FPT, ICT, BST, ...
-- 
-- 			-- finish statistics
-- 			put_line (statistics_handle_cam, comment_mark & " " & row_separator_single);
-- 			put_line (statistics_handle_cam, comment_mark & " end of list");
-- 			log_indentation_down;
-- 			close (statistics_handle_cam);
-- 			
-- 			log_indentation_down;
-- 			next (module_cursor);
-- 		end loop;
-- 		
-- 		log_indentation_down;
-- 	end write_statistics;

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

	
end et_kicad.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
