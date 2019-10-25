------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                KICAD                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;
with ada.environment_variables;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_geometry;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_project;
with et_pcb;
with et_pcb_coordinates;
with kicad_coordinates;			use kicad_coordinates;
with et_kicad_general;			use et_kicad_general;
with et_kicad_pcb;
with et_import;
with et_export;
with et_csv;
with conventions;
with et_text;

with et_packages;
with et_symbols;				use et_symbols;
with et_devices;

package body et_kicad is

	use et_general.type_net_name;
	
	function to_submodule_name (file_name : in type_schematic_file_name.bounded_string)
		return type_submodule_name.bounded_string is
	-- Returns the base name of the given schematic file name as submodule name.
		use ada.directories;
	begin
		-- CS: test if given submodule has an extension. if not return
		-- submodule as it is.
		--return to_bounded_string (base_name (et_coordinates.to_string (submodule)));
		return to_submodule_name (base_name (kicad_coordinates.to_string (file_name)));
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
	
	
	function unit_exists (
	-- Returns true if the unit with the given name exists in the given list of units.
		name	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
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
		name 	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
		units 	: in type_units_schematic.map) -- the list of units
		return kicad_coordinates.type_position is
		unit_cursor : type_units_schematic.cursor;
	begin
		unit_cursor := type_units_schematic.find (container => units, key => name);
		return type_units_schematic.element (unit_cursor).position;
	end position_of_unit;

	function mirror_style_of_unit (
	-- Returns the mirror style of the given unit.
	-- It is assumed, the unit in question exists.
	-- The unit is an element in the given list of units.
		name 	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
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
		name	: in et_devices.type_unit_name.bounded_string; -- the unit being inquired
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
		use et_coordinates;
		use et_coordinates.geometry;
		use et_symbols;
		use et_devices;
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
				when SCH_PCB =>
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

	function to_string (style : in type_port_style) return string is begin
		return latin_1.space & to_lower (type_port_style'image (style));
	end to_string;

	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string is
	-- Returns the given port direction as string.
	begin
		if preamble then
			return " direction " & to_lower (type_port_direction'image (direction));
		else
			return latin_1.space & to_lower (type_port_direction'image (direction));
		end if;
	end to_string;
	
	function to_string (fill : in type_fill) return string is begin
		return latin_1.space & to_lower (type_fill_border'image (fill.border))
		& latin_1.space & "pattern" & latin_1.space 
		& to_lower (type_fill_pattern'image (fill.pattern));
	end to_string;
	
	function units_of_component (component_cursor : in type_components_schematic.cursor) return type_units_schematic.map is
	-- Returns the units of the given component.
		u : type_units_schematic.map;

		procedure locate (
			name		: in et_libraries.type_device_name;
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
		prefix 		: in et_devices.type_device_name_prefix.bounded_string;
		characters	: in character_set) is
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use et_devices.type_device_name_prefix;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> prefix,
			set		=> characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (ERROR, "component prefix " & to_string (prefix) 
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
		) return et_libraries.type_device_name is
		
		use et_libraries;
		use et_devices;

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := text_in;
	
		r : type_device_name := (
				prefix 		=> type_device_name_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : type_device_name_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (ERROR, latin_1.lf & "invalid component reference " & enclose_in_quotes (text_in_justified),
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_devices.type_device_name_prefix;
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
		return et_libraries.type_device_name is
	-- Returns the component reference where cursor points to.
	begin
		return type_components_schematic.key (cursor);
	end component_reference;


	procedure write_component_properties (
	-- Writes the properties of the component indicated by the given cursor.
		component 		: in type_components_schematic.cursor;
		log_threshold 	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_libraries;
		use et_symbols;
		use et_devices;
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
			when SCH_PCB =>

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

	
	function to_package_name (
		library_name	: in et_kicad_general.type_device_library_name.bounded_string; -- ../libraries/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string; -- TRANSISTOR_PNP
		package_variant	: in et_devices.type_component_variant_name.bounded_string) -- N, D
		return et_libraries.type_component_package_name.bounded_string is
	-- Returns the package name for of the given component.
		package_name : et_libraries.type_component_package_name.bounded_string; -- to be returned
	begin -- to_package_name
		-- CS
		return package_name;
	end to_package_name;

	function to_string (
		no_connection_flag	: in type_no_connection_flag;
		scope				: in kicad_coordinates.type_scope) return string is
	-- Returns the position of the given no-connection-flag as string.
		use et_coordinates;
	begin	
		return (to_string (position => no_connection_flag.coordinates, scope => scope));
	end to_string;

	function to_string (port : in type_port_with_reference) return string is
	-- Returns the properties of the given port as string.
		use et_coordinates;
	begin
		return "reference " & et_libraries.to_string (port.reference) 
			& " port " & et_symbols.to_string (port.name)
			& " coordinates " & to_string (position => port.coordinates, scope => module);
	end to_string;

	
	function compare_ports (left, right : in type_port_with_reference) return boolean is
	-- Returns true if left comes before right. Compares by component reference and port name.
	-- If left equals right, the return is false.	
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries;
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

	
	function lowest_xy (
	-- Returns the lowest x/y position of the given strand.
		strand			: in type_strand;
		log_threshold	: in et_string_processing.type_log_level
		) return et_coordinates.geometry.type_point is
		
		point_1, point_2 : et_coordinates.geometry.type_point;
		segment : type_net_segments.cursor;
	
		use type_net_segments;
		use et_string_processing;
		use et_coordinates;
		use geometry;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance
	begin
		log_indentation_up;
		log (text => "calculating the point nearest to drawing origin ...", level => log_threshold + 1);

		-- init point_1 as the farest possible point from drawing origin
		-- set_x (point_1, type_distance_xy'last);
		set (X, type_distance_xy'last, point_1);
		-- set_y (point_1, type_distance_xy'last);
		set (Y, type_distance_xy'last, point_1);
		
		-- loop through segments and keep the nearest point to origin
		segment := strand.segments.first;
		while segment /= type_net_segments.no_element loop

			-- check start point of segment
			-- if closer to orign than point_1 keep start point
			point_2	:= type_point (element (segment).coordinates_start);
			if distance (point_2, origin) < distance (point_1, origin) then
				log (text => " start", level => log_threshold + 2);
				point_1 := point_2;
			end if;

			-- check start point of segment
			-- if closer to orign than point_1 keep end point
			point_2	:= type_point (element (segment).coordinates_end);
			if distance (point_2, origin) < distance (point_1, origin) then
				log (text => " end", level => log_threshold + 2);
				point_1 := point_2;
			end if;
			
			next (segment);
		end loop;

		log_indentation_down;
		
		return point_1;
	end lowest_xy;



	procedure no_generic_model_found (
		reference		: in et_libraries.type_device_name; -- IC303
		library			: in et_kicad_general.type_device_library_name.bounded_string; -- ../lib/transistors.lib
		generic_name	: in type_component_generic_name.bounded_string) -- TRANSISTOR_NPN
		is
		use et_string_processing;
		use et_libraries;
		use et_devices;
	begin
		log (ERROR, "component " & to_string (reference) -- CS: output coordinates
			& " has no generic model " & to_string (generic_name)
			& " in library " & to_string (library), console => true);
		raise constraint_error;
	end no_generic_model_found;

	
	function component_appearance (cursor : in type_components_library.cursor)
	-- Returns the component appearance where cursor points to.
		return et_symbols.type_device_appearance is
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
			name : in et_devices.type_unit_name.bounded_string;
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
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		use type_component_datasheet;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => datasheet,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (ERROR, "URL to datasheet " & to_string (datasheet) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				 console => true
				);
			raise constraint_error;
		end if;
	end check_datasheet_characters;

	
	
	
	procedure clear (lines : in out type_lines.list) is -- CS no paramter required
	-- CS procedure clear is
	begin
		type_lines.clear (lines);
	end clear;

	procedure add (line : in type_fields_of_line) is
	begin
		lines.append (line);
	end add;
	
	function first (lines : in type_lines.list) return type_lines.cursor is
	begin
		return type_lines.first (lines);
	end first;

	procedure next (line : in out type_lines.cursor) is
	begin
		type_lines.next (line);
	end next;

	function line return et_string_processing.type_fields_of_line is
	begin
		return type_lines.element (line_cursor);
	end line;
	
	procedure invalid_field (line : in type_fields_of_line) is
	begin
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

	procedure validate_prefix (prefix : in et_devices.type_device_name_prefix.bounded_string) is
	-- Tests if the given prefix is a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_string_processing;
		use et_devices.type_device_name_prefix;
	begin
		if to_string (prefix) = power_flag_prefix or to_string (prefix) = power_symbol_prefix then
			null;
		else
			log (ERROR, "invalid prefix "
				 & to_string (prefix) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;

	procedure validate_prefix (reference : in et_libraries.type_device_name) is
	-- Tests if the given reference has a power_flag_prefix or a power_symbol_prefix.
	-- Raises exception if not.
		use et_libraries;
		use et_devices.type_device_name_prefix;
	begin
		if to_string (reference.prefix) = power_flag_prefix or to_string (reference.prefix) = power_symbol_prefix then
			null;
		else
			log (ERROR, "invalid prefix in component reference "
				 & et_libraries.to_string (reference) & " !"
				 & " Expected " 
				 & power_flag_prefix & " or "
				 & power_symbol_prefix & " !",
				console => true
				);
			raise constraint_error;
		end if;
	end validate_prefix;
			
	function to_point (x_in, y_in : in string) return et_coordinates.geometry.type_point is
		point : et_coordinates.geometry.type_point;
		x, y : et_coordinates.type_distance_xy;

		use et_coordinates;
		use geometry;
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
			et_string_processing.field (
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
	
	function package_name (text : in string) return et_libraries.type_component_package_name.bounded_string is
	-- extracts from a string like "bel_ic:S_SO14" the package name "S_SO14"
	begin
		return et_libraries.type_component_package_name.to_bounded_string (
			et_string_processing.field (
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
				if et_string_processing.field (line,1) = component_field_identifier then

					-- Then we test the field id.
					-- The field id must be mapped to the actual field meaning:
					case type_component_field_id'value (et_string_processing.field (line,2)) is -- "0.."
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
				if strip_id (et_string_processing.field (line,1)) = component_field_identifier then
				
					case type_component_field_id'value (strip_f (et_string_processing.field (line,1))) is
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
			when V => return 90.0;
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

	function to_text_style (
	-- Converts a vertical kicad text style to type_text_style.
	-- The given style_in is something like CNN or "Italic" (if it is about a text field or a simple text).
	-- We are interested in the 2nd and 3rd character only.
		style_in : in string;
		text : in boolean -- true if it is about the style of a text, false if it is about the style of a field
		-- Explanation: The style of a text is something like "~" or "Italic".
		-- The style of a field comes with the letters 2 and 3 of a string like CNN.
		) return type_text_style is
		
		a : type_text_style;
		s_field : string (1..2);
	
		procedure invalid_style is
		begin
			log (ERROR, "invalid text style '" & style_in & "' !");
			raise constraint_error;
		end invalid_style;
		
	begin -- to_text_style
		case text is
			when true =>
				if style_in = text_schematic_style_normal then
					a := type_text_style'first;
				elsif style_in = text_schematic_style_italic then
					a := ITALIC;
				else
					invalid_style;
				end if;
				
			when false =>
				s_field := style_in (style_in'first + 1 .. style_in'last);
				
				if    s_field = field_style_default then 		a := type_text_style'first;
				elsif s_field = field_style_bold then 			a := BOLD;
				elsif s_field = field_style_italic then 		a := ITALIC;
				elsif s_field = field_style_italic_bold then 	a := ITALIC_BOLD;
				else
					invalid_style;
				end if;
		end case;

		return a;
		
		exception
			when constraint_error =>
				invalid_style;

				return a; -- CS: never reached
				
	end to_text_style;
	
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
		return type_device_appearance is
		
		comp_app	: type_device_appearance;
		lca			: type_library_component_appearance;

		procedure invalid_appearance is
		begin
			log (ERROR, et_string_processing.affected_line (line) 
				 & "invalid visibility flag !", console => true);
			raise constraint_error;
		end invalid_appearance;	

		use et_libraries;

	begin -- to_appearance
		case schematic is

			when true =>
				-- If it is about a schematic component we just test if the first
				-- character of the 3rd subfield is a hash sign.
				if et_string_processing.field (line,3) (et_string_processing.field (line,3)'first) 
					= schematic_component_power_symbol_prefix then
					comp_app := sch;
				else
					comp_app := sch_pcb;
				end if;
				
			when false =>
				-- If it is about a library component we test the whole letter
				-- in subfield #10.
				lca := type_library_component_appearance'value (et_string_processing.field (line,10));

				-- Evaluate lca and set comp_app accordingly.
				case lca is
					when N =>
						comp_app := sch_pcb;
					when P => 
						comp_app := sch;
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
		rep_in := type_alternative_representation'value (et_string_processing.field (line,3));

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
		use et_libraries;

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

		-- CS: exception handler
	end to_degrees;

	function to_power_flag (reference : in et_libraries.type_device_name) 
		return type_power_flag is
	-- If the given component reference is one that belongs to a "power flag" returns YES.
		use et_devices;
		use type_device_name_prefix;
	begin
		--log (text => et_schematic.to_string (reference));
		if et_libraries.prefix (reference) = power_flag_prefix then
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
	-- Prepends a heading tilde character to a generic component name.
	-- example: TRANSISTOR_NPN becomes ~TRANSISTOR_NPN
	-- The leading tilde marks a component whose value is set to "invisible".
		use et_import;
		use type_component_generic_name;
	begin
		return '~' & generic_name;
	end prepend_tilde;

	procedure validate_component_package_name 
		(name : in et_libraries.type_component_package_name.bounded_string) is
	-- Tests if the given component package name meets certain conventions.
		use et_libraries.type_component_package_name;
		use et_string_processing;
		
		procedure no_package is
		begin
			log (ERROR, "no package associated !", 
				console => true);
			raise constraint_error;
		end no_package;
			
	begin -- validate_component_package_name
		if length (name) > 0 then
			et_libraries.check_package_name_characters (name, component_package_name_characters);
		else
			no_package;
		end if;
	end validate_component_package_name;
	
	procedure read_components_libraries (log_threshold : in type_log_level) is
	-- Reads component libraries.

		use et_packages;
		use et_devices;
        use et_libraries; -- most of the following stuff is specified there
		use type_full_library_names;

		-- This is the library cursor. It points to the library being processed (in the list tmp_component_libraries):
		lib_cursor		: type_libraries.cursor;

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
			tmp_prefix				: type_device_name_prefix.bounded_string; -- IC -- CS: rename to prefix
			tmp_appearance			: type_device_appearance; -- CS: rename to appearance

			tmp_port_name_visible		: type_port_name_visible;
			tmp_terminal_name_visible	: type_terminal_name_visible;
			tmp_port_name_offset		: et_coordinates.type_distance; -- CS: rename to port_name_offset
			tmp_terminal_name			: type_terminal_name.bounded_string;
			
			tmp_units_total		: type_units_total; -- see spec for range -- CS rename to units_total	
			tmp_unit_id			: type_unit_id; -- assumes 0 if all units are affected, -- see spec	-- CS rename to unit_id

			tmp_unit_swap_level	: type_unit_swap_level := unit_swap_level_default; -- CS: rename to unit_swap_level
			tmp_unit_add_level	: type_unit_add_level := type_unit_add_level'first; -- CS: rename to unit_add_level
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
			tmp_terminal_port_map : type_terminal_port_map.map;
			
			procedure init_temp_variables is
			-- Resets "field found flags".
			begin
				-- CS: init other variables
				extra_unit_available := false;
				tmp_unit_add_level := type_unit_add_level'first;
				tmp_unit_global := false;

				field_prefix_found			:= false;
				field_value_found			:= false;
				field_package_found			:= false;
				field_datasheet_found		:= false;

				-- clear terminal-port map for the new component
				type_terminal_port_map.clear (tmp_terminal_port_map);
			end init_temp_variables;

			function to_swap_level (swap_in : in string)
			-- Converts the kicad interchangeable flag to the et swap level.
			-- Since Kicad has only one swap level (interchangeable yes or no) 
			-- we convert to the lowest swap level available.
			-- Used when reading component libraries.	
				return type_unit_swap_level is
				
				i : type_symbol_interchangeable;
				s : type_unit_swap_level;
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

			function to_unit_name (id : in type_unit_id) return type_unit_name.bounded_string is
			-- returns the given unit id as type_unit_name
			begin
				return type_unit_name.to_bounded_string (trim (type_unit_id'image (id), left));
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
				point		: et_coordinates.geometry.type_point;

				use et_coordinates;
				use geometry;
			begin -- to_polyline

				-- read total number of points
				total := positive'value (et_string_processing.field (line, pos));
				
				-- read line width (field #5)
				pos := 5;
				polyline.width := type_line_width'value (et_string_processing.field (line, pos));

				-- From the next field (#6) on, we find the coordinates of the 
				-- start point, the bend point(s) and the end point:
				pos := 6;
				loop exit when pos > end_point;
					--set_x (point, mil_to_distance (mil => et_string_processing.field (line, pos))); -- set x
					set (X, mil_to_distance (mil => et_string_processing.field (line, pos)), point); -- set x
				
					--set_y (point, mil_to_distance (mil => et_string_processing.field (line, pos+1))); -- set y (right after the x-field)
					set (Y, mil_to_distance (mil => et_string_processing.field (line, pos + 1)), point); -- set y (right after the x-field)

					-- For some unknown reason, kicad saves the y position of library objects inverted.
					-- It is probably a bug. However, when importing objects we must invert y. 
					mirror (point => point, axis => x);
					
					polyline.points.append (point); -- append this point to the list of points
					pos := pos + 2; -- advance field pointer to x coordinate of next point
				end loop;

				-- read fill style from last field
				polyline.fill := to_fill (et_string_processing.field (line, pos));				
				
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

				use et_coordinates;
				use geometry;
			begin -- to_rectangle
				--set_x (rectangle.corner_A, mil_to_distance (mil => et_string_processing.field (line,2)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,2)), rectangle.corner_A);
				
				--set_y (rectangle.corner_A, mil_to_distance (mil => et_string_processing.field (line,3)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,3)), rectangle.corner_A);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.corner_A, axis => x);
				
				--set_x (rectangle.corner_B, mil_to_distance (mil => et_string_processing.field (line,4)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,4)), rectangle.corner_B);

				--set_y (rectangle.corner_B, mil_to_distance (mil => et_string_processing.field (line,5)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,5)), rectangle.corner_B);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => rectangle.corner_B, axis => x);
				
				rectangle.width	:= type_line_width'value (et_string_processing.field (line,8));
				rectangle.fill := to_fill (et_string_processing.field (line,9));

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

				use et_coordinates;
				use geometry;
			begin -- to_circle
				--set_x (circle.center, mil_to_distance (mil => et_string_processing.field (line,2)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,2)), circle.center);
				
				--set_y (circle.center, mil_to_distance (mil => et_string_processing.field (line,3)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,3)), circle.center);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => circle.center, axis => x);
	
				circle.radius	:= mil_to_distance (mil => et_string_processing.field (line,4));
				circle.width	:= type_line_width'value (et_string_processing.field (line,7));
				circle.fill		:= to_fill (et_string_processing.field (line,8));

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

				use et_coordinates;
				use geometry;
			begin -- to_arc
				--set_x (arc.center, mil_to_distance (mil => et_string_processing.field (line,2)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,2)), arc.center);
				
				--set_y (arc.center, mil_to_distance (mil => et_string_processing.field (line,3)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,3)), arc.center);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.center, axis => x);

				arc.radius		:= mil_to_distance (mil => et_string_processing.field (line,4));

				arc.start_angle	:= to_degrees (et_string_processing.field (line,5));
				arc.end_angle	:= to_degrees (et_string_processing.field (line,6));
				
				arc.width		:= type_line_width'value (et_string_processing.field (line,9));
				arc.fill		:= to_fill (et_string_processing.field (line,10));
				
				--set_x (arc.start_point, mil_to_distance (mil => et_string_processing.field (line,11)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,11)), arc.start_point);
				
				--set_y (arc.start_point, mil_to_distance (mil => et_string_processing.field (line,12)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,12)), arc.start_point);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => arc.start_point, axis => x);

				--set_x (arc.end_point, mil_to_distance (mil => et_string_processing.field (line,13)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,13)), arc.end_point);
				
				--set_y (arc.end_point, mil_to_distance (mil => et_string_processing.field (line,14)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,14)), arc.end_point);

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

				function to_style ( style_in : in string; bold_in : in string) return type_text_style is
				-- Composes from style_in and bold_in a type_text_style
					a : type_text_style;

					procedure invalid_style is begin
						log (ERROR, "invalid text style '" & style_in & "' !");
						raise constraint_error;
					end invalid_style;

				begin -- to_style
					if bold_in = library_text_bold_off then -- "0" -- bold disabled
						
						if style_in = text_library_style_normal then
							a := normal;
						elsif style_in = text_library_style_italic then
							a := italic;
						else
							invalid_style;
						end if;

					elsif bold_in = library_text_bold_on then -- "1" -- bold enabled

						if style_in = text_library_style_normal then
							a := bold;
						elsif style_in = text_library_style_italic then
							a := italic_bold;
						else
							invalid_style;
						end if;

					else -- "bold" flag invalid
						raise constraint_error; -- CS : write message on invaid "bold" flag
					end if;

					return a;
				end to_style;

				function to_content (text_in : in string) return et_text.type_text_content.bounded_string is
				-- Replaces tildss in given string by space and returns a type_text_content.bounded_string.
					t : string (1..text_in'length) := text_in; -- copy given text to t
				begin
					-- replace tildes in given text by spaces.
					translate (t, et_string_processing.tilde_to_space'access);
					return et_text.type_text_content.to_bounded_string (t);
				end to_content;

				use et_coordinates;
				use geometry;
			begin -- to_text
				text.rotation := to_degrees (et_string_processing.field (line,2));
-- 				if text.rotation not in type_rotation_text then
				if text.rotation'valid then
					warning_angle_greater_90_degrees;
				end if;
				
				--set_x (text.position, mil_to_distance (mil => et_string_processing.field (line,3)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,3)), text.position);
				
				--set_y (text.position, mil_to_distance (mil => et_string_processing.field (line,4)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,4)), text.position);

				-- For some unknown reason, kicad saves the y position of library objects inverted.
				-- It is probably a bug. However, when importing objects we must invert y. 
				mirror (point => text.position, axis => x);

				text.size := mil_to_distance (mil => et_string_processing.field (line,5));

				-- compose from fields 10 and 11 the text style
				text.style := to_style (et_string_processing.field (line,10), et_string_processing.field (line,11));

				-- compose alignment
				text.alignment.horizontal	:= to_alignment_horizontal (et_string_processing.field (line,12));
				text.alignment.vertical		:= to_alignment_vertical (et_string_processing.field (line,13));

				-- read text content and replace tildes by spaces
				text.content := to_content (et_string_processing.field (line,9));

				-- CS: log properties
				return text;
			end to_text;

			function to_port (line : in et_string_processing.type_fields_of_line) return type_port_library is
			-- Converts the given line to a type_port.
				use conventions;
			
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

				function to_rotation (orientation : in string) return et_coordinates.type_rotation is
				-- Translates orientation up/down/left/right (U/D/L/R) to angle.
					use et_coordinates;
					orient : constant character := orientation (orientation'first);
					rot : et_coordinates.type_rotation := 0.0;
				begin
					case orient is
						when 'D' => rot := 90.0; -- to be connected with a net from top
						when 'U' => rot := 270.0; -- below
						when 'R' => rot := 180.0; -- left
						when 'L' => rot := 0.0; -- right
						when others => 
							log (ERROR, "invalid port orientation !", console => true);
							raise constraint_error;
					end case;
					return rot;
				end to_rotation;
				
				use et_coordinates;
				use geometry;
				
			begin -- to_port
				log_indentation_up;

				-- port name. to be taken from field #2 of the given line
				port.name := type_port_name.to_bounded_string (et_string_processing.field (line,2)); -- GND, GPIO2
				
				-- compose terminal name. must be stored temporarily. will be inserted in default package variant
				tmp_terminal_name := type_terminal_name.to_bounded_string (et_string_processing.field (line,3)); -- H5, 14

				-- compose position
				--set_x (port.position, mil_to_distance (mil => et_string_processing.field (line,4)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,4)), port.position);
				
				--set_y (port.position, mil_to_distance (mil => et_string_processing.field (line,5)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,5)), port.position);
				mirror (point => port.position, axis => x);

				-- compose length
				port.length := mil_to_distance (mil => et_string_processing.field (line,6));

				-- compose rotation
				port.rotation := to_rotation (et_string_processing.field (line,7));

				-- port and termnal name text size
				port.terminal_name_size := mil_to_distance (mil => et_string_processing.field (line,8));
				check_schematic_text_size (category => TERMINAL_NAME, size => port.terminal_name_size);

				port.port_name_size	:= mil_to_distance (mil => et_string_processing.field (line,9));
				check_schematic_text_size (category => PORT_NAME, size => port.port_name_size);

				-- direction
				port.direction := to_direction (et_string_processing.field (line,12));

				-- port style (optional, to be composed if field #13 present)
				if field_count (line) = 13 then
					port.style := to_style (et_string_processing.field (line,13));
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
				use et_coordinates;
				use geometry;
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

				check_text_content_length (strip_quotes (et_string_processing.field (line,2)));
				text.content := type_text_content.to_bounded_string (strip_quotes (et_string_processing.field (line,2)));
					
				-- check content vs. meaning. 
				case meaning is

					when NAME =>
						check_prefix_length (content (text));
						check_prefix_characters (
							prefix => type_device_name_prefix.to_bounded_string (content (text)),
							characters => et_kicad.component_prefix_characters);
					
					when VALUE =>
						declare
							value : type_value.bounded_string;
						begin
							value := to_value (
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
							packge => type_component_package_name.to_bounded_string (content (text)),
							characters => et_kicad.component_package_name_characters);

					when others => null; -- CS

				end case;
				
				--set_x (text.position, mil_to_distance (mil => et_string_processing.field (line,3)));
				set (X, mil_to_distance (mil => et_string_processing.field (line,3)), text.position);

				--set_y (text.position, mil_to_distance (mil => et_string_processing.field (line,4)));
				set (Y, mil_to_distance (mil => et_string_processing.field (line,4)), text.position);
				
				text.size := mil_to_distance (mil => et_string_processing.field (line,5));

				text.rotation := to_field_orientation (et_string_processing.field  (line,6));
				
				--text.visible := to_field_visible (
				--	vis_in		=> et_string_processing.field (line,7),
				--	schematic	=> false);
			
				text.alignment.horizontal := to_alignment_horizontal (et_string_processing.field (line,8));

				text.alignment.vertical   := to_alignment_vertical (et_string_processing.field (line,9));

				text.style := to_text_style (style_in => et_string_processing.field (line,9), text => false);
				
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

				use conventions;
				
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
					when sch_pcb =>
						-- This is a real component.

						-- Since this is a real component. we do the prefix character check 
						-- against the default character set for prefixes as specified in et_libraries.
						-- Afterward we validate the prefix. The prefixes for real components are specified
						-- in the et configuration file (see conventions).						
						log (text => "prefix", level => log_threshold + 1);
						if not field_prefix_found then
							missing_field (field_reference.meaning);
						else
							check_prefix_characters (
								prefix 		=> tmp_prefix,
								characters	=> device_name_prefix_characters);
							
							check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
						end if;

						log (text => "package/footprint", level => log_threshold + 1);
						if not field_package_found then
							missing_field (field_package.meaning);
						else
							validate_component_package_name (
								type_component_package_name.to_bounded_string (et_string_processing.field (
									line => read_line ( -- CS use function package_name
										line			=> content (field_package), -- bel_ic:S_SO14
										comment_mark	=> comment_mark,
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

					when sch =>
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

					when pcb => null; --CS
						
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
					when sch =>

						-- we insert into the given components list a new component
						type_components_library.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like #PWR, #FLG 
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> sch,

								-- Whether the component is a power flag can be reasoned by the prefix.
								-- At library level there is no indexed prefix. Power flags have just 
								-- the prefix "#FLG". So we can provide an arbitrary index for the conversion
								-- function "to_power_flag".
								power_flag		=> to_power_flag (to_component_reference (
											text_in			=> to_string (tmp_prefix) & "0", -- #FLG0
											leading_hash	=> true)),

								prefix			=> tmp_prefix,
								value			=> to_value (
													value => content (field_value),
													error_on_invalid_character => false
													),
												-- For the operators convenice no error is raised if invalid
												-- character found. This was the design gets imported but with
												-- (lots of) warnings.
								
								units			=> type_units_library.empty_map
								)
							);
						
					when sch_pcb =>

						-- we insert into the given components list a new component
						type_components_library.insert(
							container	=> components,
							key			=> tmp_component_name, -- generic name like 74LS00
							position	=> comp_cursor,
							inserted	=> comp_inserted,
							new_item	=> (
								appearance		=> sch_pcb,
								prefix			=> tmp_prefix,
								value			=> to_value (content (field_value)),
								units			=> type_units_library.empty_map,

								package_filter	=> type_package_filter.empty_set,
								datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),
								variants		=> type_component_variants.empty_map
								)
							);


					when others =>
						null; -- CS -- should not happen at all
						raise constraint_error;
						
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

			
			procedure set_unit_cursor (libraries : in out type_libraries.map) is
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
					type_libraries.query_element (lib_cursor, locate_component'access);
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
					key		: in type_unit_name.bounded_string;
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

							type_terminal_port_map.insert (
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
					key		: in type_unit_name.bounded_string;
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
							style		=> field_reference.style,
							rotation	=> field_reference.rotation,
							size		=> field_reference.size,
							line_width	=> field_reference.line_width,
							alignment	=> field_reference.alignment);

					unit.symbol.value := (
							meaning		=> VALUE,
							position	=> field_value.position,
							style		=> field_value.style,
							rotation	=> field_value.rotation,
							size		=> field_value.size,
							line_width	=> field_value.line_width,
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
				
				case type_library_draw'value (et_string_processing.field (line,1)) is
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,3));
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,6));
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,5));
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,7));
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,7));
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
						
						tmp_unit_id := to_unit_id (et_string_processing.field (line,10));
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

				fp := type_package_proposal.to_bounded_string (et_string_processing.field (line,1));
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
						if strip_quotes (et_string_processing.field (line,2)) = type_device_name_prefix.to_string (tmp_prefix) then
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

						use type_component_variants;
						use type_terminal_port_map;

						tmp_variant_name : type_component_variant_name.bounded_string; -- temporarily used for building the variant name
						tmp_variants : type_component_variants.map; -- temporarily used for building the variant

						full_package_library_name : type_package_library_name.bounded_string;
					begin
						case component.appearance is
							when sch_pcb => -- real component

								-- The name of the default variant is the package
								-- name itself (instead of an empty string or a string like "default"):
								check_variant_name_length (to_string (package_name (content (field_package)))); -- S_SO14
								tmp_variant_name := to_component_variant_name (to_string (package_name (content (field_package)))); -- S_SO14
								check_variant_name_characters (tmp_variant_name);

								-- Find the library where the given package is stored in.
								full_package_library_name := et_kicad_pcb.full_library_name ( -- ../lbr_dir_1/bel_ic.pretty
									library_name	=> library_name (content (field_package)), -- bel_ic
									package_name	=> package_name (content (field_package)), -- S_SO14
									log_threshold	=> log_threshold + 1);
								
								-- Test whether library, package and terminal_port_map fit together.
								if et_kicad_pcb.terminal_port_map_fits (
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
				
				type_libraries.update_element ( 
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

			use et_coordinates;
			
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
							
							if et_string_processing.field (line,1) = et_kicad.def then
								component_entered := true;

								init_temp_variables;
								
								-- Since we are reading the fields, we set the active_section to "fields"
								active_section := fields;

								-- The commponent header provides the first component properties:
								tmp_component_name := type_component_generic_name.to_bounded_string (
														et_string_processing.field (line,2)); -- 74LS00

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

								tmp_prefix := type_device_name_prefix.to_bounded_string (et_string_processing.field (line,3)); -- U

								-- Detect invalid characters in tmp_prefix:
								-- NOTE: we test against the kicad specific character set that allows a #
								check_prefix_characters (
									prefix		=> tmp_prefix,
									characters	=> et_kicad.component_prefix_characters);

								-- The unknown field #4 is always a zero
								if et_string_processing.field (line, 4) /= "0" then
									log (WARNING, "expect 0 in field #4 !");
								end if;
								
								tmp_port_name_offset := geometry.mil_to_distance (mil => et_string_processing.field (line,5)); -- relevant for supply pins only
								tmp_terminal_name_visible := to_pin_visibile (et_string_processing.field (line,6));
								tmp_port_name_visible := to_port_visibile (et_string_processing.field (line,7));
								
								-- Get number of units and set swap level as specified in field #9.
								-- Swap level assumes default if only one unit available.
								tmp_units_total := type_units_total'value (et_string_processing.field (line,8));
								if tmp_units_total > 1 then
									log_indentation_up;
									log (text => "with" & type_units_total'image (tmp_units_total) & " units", level => log_threshold + 2);

									-- From the "interchangeable" flag we set the component wide swap level. It applies for 
									-- all units of the component (except extra units):
									tmp_unit_swap_level := to_swap_level (et_string_processing.field (line,9));
									log_indentation_down;
								else
									tmp_unit_swap_level := unit_swap_level_default;
								end if;

								-- read the appearance flag (N/P) in subfield #10
								-- This is about a component in a library -> schematic => false
								tmp_appearance := to_appearance (line => line, schematic => false);

							end if;
						else -- we are inside a component section and process subsections

							-- We wait for the end of component mark (ENDDEF) and clear the component_entered flag accordingly.
							if et_string_processing.field (line,1) = et_kicad.enddef then
								component_entered := false;

								-- Set placeholders (reference, value, ...) in internal units.
								-- The placeholder properties are known from the field-section.
								-- The placeholder properties apply for all units.
								set_text_placeholder_properties;

								-- If this is a real component, build package variant from tmp_terminal_port_map
								if tmp_appearance = sch_pcb then
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
										if et_string_processing.field (line,1) = et_kicad.fplist then
											
											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
												container	=> tmp_component_libraries,
												position	=> lib_cursor,
												process		=> insert_component'access);

											-- Create in the component as many empty units as given in units_total.
											-- They will be filled with content later.
											create_units;
											
											active_section := footprints;
											--log (text => "footprint/package filter begin", level => log_threshold + 1);
											log (text => "footprint/package filter", level => log_threshold + 2);

										elsif et_string_processing.field (line,1) = et_kicad.draw then

											-- Insert the component into the current library (indicated by lib_cursor):
											type_libraries.update_element ( 
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
										if et_string_processing.field (line,1) = et_kicad.endfplist then
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
										if et_string_processing.field (line,1) = et_kicad.enddraw then
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
										if et_string_processing.field (line,1) = et_kicad.draw then
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
					while type_libraries."/=" (lib_cursor, type_libraries.no_element) loop

						-- log library file name
						log (text => to_string (type_libraries.key (lib_cursor)), level => log_threshold + 1);
						
						-- open the same-named file and read it
						open (
							file => library_handle,
							mode => in_file,
							name => to_string (type_libraries.key (lib_cursor)));
							
						-- Now we read the library file and add components
						-- to the library pointed to by lib_cursor:
						set_input (library_handle);
						read_library (log_threshold + 1);

						close (library_handle);

						type_libraries.next (lib_cursor);
					end loop;
					
				else
					log (WARNING, "no component libraries defined in project file !");
				end if;

				
			-- For V5;
			when et_import.KICAD_V5 =>

				-- If tmp_component_libraries is empty -> nothing to do
				if not type_libraries.is_empty (tmp_component_libraries) then

					-- Set lib_cursor to first library and loop in tmp_component_libraries.
					lib_cursor := tmp_component_libraries.first;
					while type_libraries."/=" (lib_cursor, type_libraries.no_element) loop

						-- log library file name
						log (text => to_string (type_libraries.key (lib_cursor)), level => log_threshold + 1);
						
						-- open the same-named file and read it
						open (
							file => library_handle,
							mode => in_file,
							name => to_string (type_libraries.key (lib_cursor)));
							
						-- Now we read the library file and add components
						-- to the library pointed to by lib_cursor:
						set_input (library_handle);
						read_library (log_threshold + 1);

						close (library_handle);

						type_libraries.next (lib_cursor);
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
		package_name 		: in et_libraries.type_component_package_name.bounded_string;	-- S_SO14
		log_threshold		: in et_string_processing.type_log_level)
		return et_devices.type_component_variant_name.bounded_string is 					-- D

		use et_libraries;
		library_cursor : type_libraries.cursor; -- points to the component library
		
		use et_string_processing;

		use et_devices;
		variant : type_component_variant_name.bounded_string; -- variant name to be returned
		
		-- temporarily here the name of the package library is stored:
		use type_package_library_name;
		full_package_library_name : type_package_library_name.bounded_string; -- ../lbr/bel_ic
		
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
				use type_component_variants;
				use type_component_variant_name;

				-- This cursor points to the package variant being queryied.
				variant_cursor : type_component_variants.cursor := component.variants.first;

				-- If a new package variant is to be built, it is temporarily stored here:
				new_variant : type_component_variant;
			
			begin -- query_variants
				log (text => "querying package variants ...", level => log_threshold + 2);
				log_indentation_up;

				-- Loop through package variants:
				while variant_cursor /= type_component_variants.no_element loop

					-- From the library and package name we can reason the variant name.
					-- So if both the given library and package name match, the variant name
					-- is set to be returned.
					
					--if element (variant_cursor).packge.library = full_package_library_name and
					--	element (variant_cursor).packge.name = package_name then 

					if element (variant_cursor).package_model = et_packages.to_file_name (compose (
							containing_directory	=> et_packages.to_string (full_package_library_name),
							name					=> et_libraries.to_string (package_name))) then
						
						log (text => "variant " 
							& to_string (package_variant => key (variant_cursor)) 
							& " used", level => log_threshold + 1);

						variant := key (variant_cursor);
						exit; -- no further search required
					end if;

					next (variant_cursor);
				end loop;

				-- If no suitable package variant has been found, a new one must be created.
				if variant_cursor = type_component_variants.no_element then
					
					-- Package variant not defined in library. Make sure
					-- the terminal_port_map (there is only one) can be applied 
					-- on this package variant.
					log (text => "unknown variant found. validating ...", level => log_threshold + 3);
					log_indentation_up;

					-- Set variant cursor to default variant. Later the terminal_port_map is 
					-- copied from here.
					variant_cursor := component.variants.first; 

					-- Test whether the new variant complies with the terminal_port_map
					if et_kicad_pcb.terminal_port_map_fits (
						library_name 		=> full_package_library_name,
						package_name 		=> package_name,
						terminal_port_map	=> element (variant_cursor).terminal_port_map) then

						-- The new package variant is composed of the given to_full_library_name,
						-- the given package name and the same terminal_port_map as the default variant.
						-- A default package variant is always available.

						log (text => "updating library ...", level => log_threshold + 4);

						-- build the new package variant
						new_variant := (
-- 							packge 				=> (library		=> full_package_library_name,
-- 													name 		=> package_name),

							package_model => et_packages.to_file_name (compose (
								containing_directory	=> et_packages.to_string (full_package_library_name),
								name					=> et_libraries.to_string (package_name))),
							
							terminal_port_map	=> element (variant_cursor).terminal_port_map
							);

						-- insert the new package variant in the component (in library)
						type_component_variants.insert (
							container	=> component.variants,
							key			=> to_component_variant_name (to_string (packge => package_name)),
							new_item	=> new_variant);

					else
						log (ERROR, "terminal-port map does not fit !", console => true); -- CS: more details
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
			log (text => "locating generic component in library ...", level => log_threshold + 1);
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
		full_package_library_name := et_kicad_pcb.full_library_name (
			library_name 	=> package_library, -- bel_ic
			package_name	=> package_name,	-- S_SO14
			log_threshold	=> log_threshold + 1);

		-- locate the given component library
		library_cursor := tmp_component_libraries.find (component_library);

		-- locate the given generic component
		type_libraries.update_element (
			container	=> tmp_component_libraries,
			position	=> library_cursor,
			process		=> locate_component'access);
		
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
		use et_kicad.type_strands;

        net_name : type_net_name.bounded_string;
	
		strand	: type_strands.cursor;
	
		procedure add_net (
		-- Creates a net with the name and the scope (local, global) of the current strand. 
		-- If strand is local, the net name is rendered to a full hierarchic name.
		-- If the net existed already, then strand is appended to the strands of the net.
			mod_name : in type_submodule_name.bounded_string;
			module   : in out type_module) is

			use type_nets;
			
			net_created : boolean;
			net_cursor : et_kicad.type_nets.cursor;

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
						position => element (strand).position, scope => kicad_coordinates.MODULE));
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
								position => element (strand).position, scope => kicad_coordinates.MODULE)
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

			use et_coordinates;

			-- CS this is a workaround in order to provide a line for function on_line:
			type type_line_scratch is new et_schematic.shapes.type_line with null record;
			line : type_line_scratch := (
				start_point	=> geometry.type_point (segment.coordinates_start), 
				end_point	=> geometry.type_point (segment.coordinates_end));
			
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

			use et_coordinates.geometry;
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

								log_indentation_up;
								
								log (text => "strand " & 
										to_string (lowest_xy (element (h_strand), log_threshold + 3)),
										level => log_threshold + 2);
								
								log_indentation_down;

								-- append the strand to the temparily collection of hierarchic strands
								et_kicad.type_strands.append (
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
			use et_coordinates.geometry;
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
			use et_coordinates;
			
			-- for the segment we provide a consequtive number which has no further meaning
			segment_number : count_type := 1;			
		begin
			if log_level >= log_threshold + 2 then
				log_indentation_up;
				while segment /= type_net_segments.no_element loop
					log (text => "segment #" 
						& count_type'image (segment_number) 
						& latin_1.space
						& et_kicad.to_string (
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
			use et_coordinates;
			
			-- for the strand we provide a consequtive number which has no further meaning
			strand_number : count_type := 1;			
		begin -- query_strand
			if log_level >= log_threshold + 1 then
				log_indentation_up;
				while strand /= type_strands.no_element loop
					log (text => "strand #" & trim (count_type'image (strand_number), left) &
						 " at" & to_string (
							position => element (strand).position, scope => kicad_coordinates.MODULE)
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

	
	
	procedure import_design (
		project			: in et_project.type_project_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	-- Imports the design libraries and the actual design as specified by parameter "project".
	-- Inserts the created (sub)module in the module collection (see type_modules).
	-- Leaves the global module_cursor pointing where the module was inserted.
		
		use et_schematic;

		hierarchic_sheet_file_names : type_hierarchic_sheet_file_names_extended;

		current_schematic : type_hierarchic_sheet_file_name_and_timestamp; -- sensor.sch / B7F2F34A
		
		net_id : natural := 0; -- for counting name-less nets (like N$1, N$2, N$3, ...)

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
			
			use et_libraries;
			use et_schematic;

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
									type_libraries.insert (
										container	=> tmp_component_libraries,
										key 		=> et_kicad_general.type_device_library_name.to_bounded_string (compose (
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

			begin -- read_proj_v4
				log (
					text => "reading project file " 
					& compose (
						name		=> et_project.type_project_name.to_string (project), 
						extension	=> file_extension_project) & " ...",
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
								name		=> et_project.type_project_name.to_string (project), 
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
							if et_string_processing.field (line,1) = project_header_eeschema then
								clear_section_entered_flags;
								section_eeschema_entered := true;
							end if;

							-- test header [eeschema/libraries]
							if et_string_processing.field (line,1) = project_header_eeschema_libraries then
								clear_section_entered_flags;
								section_eeschema_libraries_entered := true;
							end if;

						when 2 =>
							if section_eeschema_entered then

								-- Get library directory names 
								if et_string_processing.field (line,1) = project_keyword_library_directory then
									log (text => "library directories " & et_string_processing.field (line,2), level => log_threshold + 2);

									-- The library directories must be
									-- inserted in the search list of library directories (search_list_project_lib_dirs).
									-- These directories assist search operations for both components and packages.
									locate_library_directories (et_string_processing.field (line,2), log_threshold + 3);
								end if;
								
							end if;

							if section_eeschema_libraries_entered then

								-- From a line like "LibName1=bel_supply" get component library names 
								-- (incl. path and extension) and
								-- store them in search_list_component_libraries (see et_kicad.ads).
								-- We ignore the index of LibName. Since we store the lib names in a 
								-- simple list their order remains unchanged anyway.
								if et_string_processing.field (line,1)(1..project_keyword_library_name'length) 
									= project_keyword_library_name then

									-- The component library could have been referenced already. If so,
									-- there is no need to append it again to search_list_component_libraries.
									if not type_library_names.contains (
										container 	=> search_list_component_libraries,
										item		=> type_library_name.to_bounded_string (et_string_processing.field (line,2))) then
										
											type_library_names.append (
												container	=> search_list_component_libraries, 
												new_item	=> type_library_name.to_bounded_string (et_string_processing.field (line,2)));

											-- NOTE: search_list_component_libraries keeps the libraries in the same order as they appear
											-- in the project file. search_list_component_libraries assists search operations.
											-- It applies for the current project only and
											-- is cleared as soon as another kicad project file is read.
											
											-- For the log write something like "LibName bel_connectors_and_jumpers"
											log (text => et_string_processing.field (line,1) & " " & et_string_processing.field (line,2), level => log_threshold + 2);
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
							type_libraries.insert (
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
					
				begin -- locate_package_libraries
					log (text => "locating libraries ...", level => log_threshold + 1);
					log_indentation_up;

					while lib_cursor /= type_lib_table.no_element loop
						uri := element (lib_cursor).lib_uri; -- get full name like /home/user/kicad_libs/bel_ic.pretty
						log (text => et_devices.to_string (uri), level => log_threshold + 2);

						-- Test if library file exists:
						if ada.directories.exists (et_devices.to_string (uri)) then

							-- create empty package/footprint library
							et_kicad_pcb.type_libraries.insert (
								container	=> et_kicad_pcb.package_libraries,
								key 		=> et_packages.to_file_name (et_devices.to_string (uri)),
								new_item	=> et_kicad_pcb.type_packages_library.empty_map
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
					lines : type_lines.list; -- all lines of the table

					-- This cursor points to the line being processed (in the list of lines given in "lines"):
					line_cursor : type_lines.cursor;
					
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

					procedure get_next_line is
					-- Fetches a new line from the container "lines".
						use type_lines;
					begin
						next (line_cursor);
						if line_cursor /= type_lines.no_element then

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

						use type_lines;
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
						use type_lines;

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
						use type_lines;
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
							type_lines.append (lines, line);
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
					current_line := type_current_line.to_bounded_string (to_string (type_lines.element (line_cursor)));
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
			type_libraries.clear (tmp_component_libraries);
			
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
					name		=> et_project.type_project_name.to_string (project), 
					extension	=> file_extension_schematic)
					);
		end read_project_file;

		function to_angle (text_in : in string) return et_coordinates.type_rotation is
		-- Converts the label orientation to type_rotation.
		-- CS: use a dedicated type for input parameter.
			o_in : type_label_orientation := type_label_orientation'value(text_in);
			o_out : et_coordinates.type_rotation;
		begin
			case o_in is
				when 0 => o_out :=   0.0;
				when 1 => o_out :=  90.0;
				when 2 => o_out := 180.0;
				when 3 => o_out := 270.0;
			end case;
			return o_out;
			-- CS: exception handler
		end to_angle;
		
		function to_direction (text_in : in string) return type_net_label_direction is
		-- Converts the direction of a label to a type_label_direction. 
		-- CS: currently case sensitive ! Use dedicated type for input parameter.
			d_out : type_net_label_direction := input;
		begin
			if text_in = schematic_keyword_label_dir_input then
				d_out := input;
			elsif text_in = schematic_keyword_label_dir_output then
				d_out := output;
			elsif text_in = schematic_keyword_label_dir_bidir then
				d_out := bidir;
			elsif text_in = schematic_keyword_label_dir_tristate then
				d_out := tristate;
			elsif text_in = schematic_keyword_label_dir_passive then
				d_out := passive;
			else
				log (ERROR, "Label direction unknown !", console => true);
				raise constraint_error;
			end if;
			
			return d_out;
		end to_direction;		

		-- Prodcedures that set the s,e or picked flag in a wild net segment. 
		-- CS: move them to function search_for_same_coordinates
		procedure set_e (segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
		procedure set_s (segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;

		-- CS: move to procedure add_segment_to_anonymous_strand
		procedure set_picked (segment : in out type_wild_net_segment ) is begin segment.picked := true; end set_picked;
		
		function read_schematic (
		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
        -- they will be returned in hierarchic_sheet_file_names. Otherwise the returned list is empty.
			--current_schematic	: in type_schematic_file_name.bounded_string;
			current_schematic	: in type_hierarchic_sheet_file_name_and_timestamp;
			log_threshold		: in type_log_level)
			return type_hierarchic_sheet_file_names_extended is

			use et_coordinates;
			use geometry;

			hierarchic_sheet_file_names : type_hierarchic_sheet_file_names_extended; -- list to be returned
			name_of_submodule_scratch : type_submodule_name.bounded_string; -- temporarily used before appended to hierarchic_sheet_file_names

			use et_string_processing;
		
			line : et_string_processing.type_fields_of_line; -- the line of the schematic file being processed
		
			sheet_file : type_schematic_file_name.bounded_string;
	
			-- This is the total number of sheets as it is given in the sheet header. 
			-- A line like "Sheet 1 7" gives the sheet number (1), which is meaningless,
			-- and the total number of sheet of the design (7).
			sheet_count_total : type_sheet;

			wild_simple_labels	: type_simple_labels.list;
            wild_tag_labels 	: type_tag_labels.list;
			wild_segments		: type_wild_segments.list;
			wild_junctions		: type_junctions.list;

			-- In the first stage, all net segments of this sheet go into a wild collection of segments.
			-- Later they will be sorted and connected by their coordinates (start and and points)
			segment_count	: count_type := 0; -- holds the total number of segments within a sheet
			
			anonymous_strand : type_anonymous_strand;

			-- The list of anonymous strands. Procedure add_strand_to_anonymous_strands uses 
			-- this container for temporarily storage of anonymous strands.
			anonymous_strands : type_anonymous_strands.list; 

			procedure error_in_schematic_file (line : in type_fields_of_line) is
			begin
				log (ERROR, "in schematic file '" 
					& to_string (current_schematic.sheet.file) & "' " 
					& et_string_processing.affected_line (line)
					& to_string (line),
					console => true);
			end error_in_schematic_file;
		
			procedure add_segment_to_anonymous_strand (segment_cursor : in type_wild_segments.cursor) is
			-- Adds a net segment (indicated by given cursor) to anonymous_strand.
			-- This procedure happens to be called for a certain segment more than once (unavoidable). So the flag "picked" serves
			-- as indicator for a segment already added to the anonymous_strand.
				--scratch : type_net_segment_base;
				scratch : type_net_segment;
			begin
				-- If segment already picked and added to anonymous_strand, do nothing with this segment. 
				-- Otherwise set the "picked" flag of that segment, output the coordinates of the segment, add it to anonymous net.
				if type_wild_segments.element (segment_cursor).picked then
					null;
					-- log (text => "  picked");
				else
					-- log (text => "  segment" & positive'image(id) & ":");
					-- log (text => "segment" & positive'image(id) & ":");
					-- log (text => "  segment" & positive'image(id) & ":");
					
					type_wild_segments.update_element (
						container	=> wild_segments,
						position	=> segment_cursor,
						process		=> set_picked'access);

-- 					write_coordinates_of_segment (segment => 
-- 						type_net_segment (type_wild_segments.element (segment_cursor)));
					
					log (text => to_string (
							segment	=> type_wild_segments.element (segment_cursor),
							scope	=> kicad_coordinates.XY),
						 level => log_threshold + 1);

					scratch := type_net_segment (type_wild_segments.element (segment_cursor));
					type_net_segments.append (anonymous_strand.segments, scratch);
				end if;
			end add_segment_to_anonymous_strand;

			function search_for_same_coordinates (
			-- Starting from a segment indicated by id and the end point (given by side), 
			-- search in wild_segments for a segment with matching start or end point.
			-- In general untouched segments are preferred in the search. "Half" processed segments are of secondary relevance.
			-- Once a suitable segment was found, sc is assigned with neccessary data to be returned to the parent unit. The search for 
			-- a suitable segment excludes fully processed segments and the given segment (id).
				segment_cursor : in type_wild_segments.cursor;
				seg_in : in type_wild_net_segment;
				side : in type_segment_side) 
				return type_same_coord_result is

				sc : type_same_coord_result;
				line_start, line_end : kicad_coordinates.type_position;
				s, e : boolean; -- indicate the end point, that has been processed already
				untouched, half_processed : boolean; -- indicate whether a segment is completely untouched or processed in only one direction

				use type_wild_segments;
				cursor : type_wild_segments.cursor;

			begin -- search_for_same_coordinates
				-- Set E/S flag:
				-- If we start the search from the end_point of a segment, the e-flag is to be set. This indicates the end_point has been processed.
				-- If we start the search from the start_point of a segment, the s-flag is to be set. This indicates the start_point has been processed.				
				case side is
					when end_point =>
						
-- 						log (text => "--> origin of search   (END): " 
-- 							 & type_grid'image(seg_in.coordinates_end.x) & "/" & type_grid'image(seg_in.coordinates_end.y),
-- 							 level => 1);
						
						type_wild_segments.update_element(
								container => wild_segments,
								position => segment_cursor,
								process => set_e'access);
						
					when start_point =>
						
-- 						log (text => "--> origin of search (START): " 
-- 							 & type_grid'image(seg_in.coordinates_start.x) & "/" & type_grid'image(seg_in.coordinates_start.y),
-- 							 level => 1);
						
						type_wild_segments.update_element(
								container => wild_segments,
								position => segment_cursor,
								process => set_s'access);
				end case;

				-- First, search completely untouched segments (they have both e and s flag cleared).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				cursor := wild_segments.first;
				while cursor /= no_element loop
					if cursor /= segment_cursor then -- skip the given segment
						line_start := type_wild_segments.element (cursor).coordinates_start;
						line_end   := type_wild_segments.element (cursor).coordinates_end;
						s  := type_wild_segments.element (cursor).s;
						e  := type_wild_segments.element (cursor).e;
						untouched := not (s or e); -- neither s nor e set

						if untouched then 
							--put(et_import.report_handle,"probe untouched segment: ");
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
									if x (line_start) = x (seg_in.coordinates_end) and y (line_start) = y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
									if x (line_end) = x (seg_in.coordinates_end) and y (line_end) = y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.									
								when start_point =>
									--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
									if x (line_start) = x (seg_in.coordinates_start) and y (line_start) = y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
									if x (line_end) = x (seg_in.coordinates_start) and y (line_end) = y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;

					next (cursor);
				end loop;

				-- No untouched segment found.
				-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				cursor := wild_segments.first;
				while cursor /= no_element loop
					if cursor /= segment_cursor then -- skip the given segment
						line_start := type_wild_segments.element (cursor).coordinates_start;
						line_end   := type_wild_segments.element (cursor).coordinates_end;
						s  := type_wild_segments.element (cursor).s;
						e  := type_wild_segments.element (cursor).e;
						half_processed := s xor e;

						if half_processed then
							--put(et_import.report_handle,"probe half-processed segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_segments.element(wild_segments,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
									if x (line_start) = x (seg_in.coordinates_end) and y (line_start) = y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
									if x (line_end) = x (seg_in.coordinates_end) and y (line_end) = y (seg_in.coordinates_end) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
								when start_point =>
									--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
									if x (line_start) = x (seg_in.coordinates_start) and y (line_start) = y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := start_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;

									--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
									if x (line_end) = x (seg_in.coordinates_start) and y (line_end) = y (seg_in.coordinates_start) then
										sc.valid := true;
										sc.side := end_point;
										sc.cursor := cursor;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;

					next (cursor);
				end loop;
				
				sc.valid := false;
				sc.cursor := cursor;
				return sc;
				
			<<matching_segment_coordinates_found>>
				add_segment_to_anonymous_strand (sc.cursor);
				--log (text => "match", level => 1);
				
				return sc;
			end search_for_same_coordinates;
			
			
			procedure associate_net_labels_with_anonymous_strands (log_threshold : in type_log_level) is
			-- All anonymous strands must be given a name. The name is enforced by net labels.
				
			-- The first label found on the strand dictates the strand name.
			-- Other labels on the strand are checked for their name only. 
			-- If the name differs from the strand name set earlier, an error is output.
			-- If scope of strands are contradicting, error is output.

			-- The kind of net label (simple, hierarchic, global) defines the scope of the strand.
			-- Net labels sitting on a segment, are added to the list of labels of that segment.
			
			-- Strands without label are named by using the notation "N$". 

				use et_coordinates;
			
				ls  	: type_net_label_simple;
				lt  	: type_net_label_tag;				
				anon_strand_a, anon_strand_b : type_anonymous_strand;
				--segment	: type_net_segment_base;
				segment	: type_net_segment;
				lls		: type_simple_labels.list;
				llt		: type_tag_labels.list;
			
				strand 		: type_strand;
				net_name	: type_net_name.bounded_string;
				
				function label_sits_on_segment (
					label	: in type_net_label;
					segment	: in type_net_segment) return boolean is
					use geometry;

					-- CS this is a workaround in order to provide a line for function line:
					type type_line_scratch is new et_schematic.shapes.type_line with null record;
					line : type_line_scratch := (
						start_point	=> geometry.type_point (segment.coordinates_start), 
						end_point	=> geometry.type_point (segment.coordinates_end));
					
				begin
					return on_line (type_point (label.coordinates), line);
				end label_sits_on_segment;

				use type_net_segments;
				-- the segment cursor points to the segment being processed
				segment_cursor : type_net_segments.cursor; 

				use type_anonymous_strands;
				
				-- the strand cursor points to the anonymous strand being processed
				strand_cursor	: type_anonymous_strands.cursor := anonymous_strands.first;
				strand_cursor_b	: type_anonymous_strands.cursor;

				use type_simple_labels;
				simple_label_cursor	: type_simple_labels.cursor; -- points to the simple label being processed

				use type_tag_labels;
				tag_label_cursor	: type_tag_labels.cursor; -- points to the tag label being processed

				procedure output_net_label_conflict is begin
					put_line (standard_output, message_error & "Net label conflict !"); -- CS log ?
				end output_net_label_conflict;
				
			begin -- associate_net_labels_with_anonymous_strands
				log_indentation_up;
				
				-- This does only make sense if there are strands at all:
				if not is_empty (anonymous_strands) then
					log (text => "associating net labels with strands ...", level => log_threshold);
					
					-- Loop in list of anonymous strands, get a (non-processed-yet) strand, loop in list of segments and 
					-- find a (non-processed-yet) net label that sits on the net segment. If label sits on segment:
					--  - assume label text as name of strand (and check other labels of the anonymous strand)
					--  - set scope of strand according to the net label
					--
					--  - mark label as processed
					--  - update/replace label in wild_simple_labels or wild_tag_labels
					--
					--  - Mark anonymous strand as processed. This indicates that the strand has got a name (given by a label).
					--    Non-Processed strands are those without a label.
					--  - update/replace anonymous strand in anonymous_strands
					while strand_cursor /= type_anonymous_strands.no_element loop -- cursor already reset on declaration (see above)
						anon_strand_a := element (strand_cursor); -- get anonymous strand
						
						--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": "); -- CS: log ?
						if not anon_strand_a.processed then -- skip already processed nets

							-- reset segment cursor to begin of segment list of the anonymous net
							segment_cursor := anon_strand_a.segments.first;
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment in anonymous strand anon_strand_a
								segment := anon_strand_a.segments (segment_cursor);
								--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s); -- CS: log ?
								
								-- Loop in list of simple labels:
								if not is_empty (wild_simple_labels) then -- do it if there are simple labels at all
									--put_line(" simple labels ..."); -- CS: log ?
									
									simple_label_cursor := wild_simple_labels.first; -- reset label cursor
									while simple_label_cursor /= type_simple_labels.no_element loop
										ls := element (simple_label_cursor); -- get simple label
										
										if not ls.processed then
											--put(et_import.report_handle, "   probing "); write_coordinates_of_label( type_net_label(ls));  -- CS: log ?
											if label_sits_on_segment (label => type_net_label (ls), segment => segment) then

												if log_level >= log_threshold + 1 then
													log_indentation_up;
													--log (text => "label at" & to_string (label => type_net_label (ls), scope => xy));
													log (text => "label at" & to_string (label => type_net_label (ls)));
													log_indentation_down;
												end if;

												-- Check if the label does not contradict with other labels of this strand.
												-- Otherwise, set scope to local.
												case anon_strand_a.scope is
													when unknown => -- find. no label found so far. set scope of strand
														anon_strand_a.scope := local;

													when local => -- strand has been marked as "local" already. nothing to to
														null;

													when hierarchic => -- strand has been marked as "hierarchic" already. no local label allowed !
														output_net_label_conflict;
														log (ERROR,
															"hierarchic net " & et_general.to_string (anon_strand_a.name) 
															& " has a local label at" 
															--& to_string (position => ls.coordinates) & " !");
															& to_string (point => ls.coordinates) & " !");
														raise constraint_error;

													when global => -- strand has been marked as "global" already. no local label allowed !
														output_net_label_conflict;
														log (ERROR,
															"global net " & et_general.to_string (anon_strand_a.name) 
															& " has a local label at" 
															--& to_string (position => ls.coordinates) & " !");
															& to_string (point => ls.coordinates) & " !");
															raise constraint_error;
												end case;

												
												-- The first matching simple label dictates the strand name. 
												-- If other labels with text differing from strand name found, output warning.
												if type_net_name.length (anon_strand_a.name) = 0 then -- If this is the first matching label

													-- assume the label text as strand name.
													anon_strand_a.name := ls.text; 
												else
													-- If label text is different from previously assigned strand name:
													if not type_net_name."=" (anon_strand_a.name, ls.text) then
														output_net_label_conflict;

														-- for the log, some more information
														log (ERROR, 
															 "Net " & et_general.to_string (anon_strand_a.name) & " has contradicting label " 
															 --& "at" & to_string (position => ls.coordinates) & " !");
															 & "at" & to_string (point => ls.coordinates) & " !");
														raise constraint_error;
													end if;
												end if;

												-- mark simple label as processed and update/replace it in wild_simple_labels
												ls.processed := true;
												type_simple_labels.replace_element (
													container => wild_simple_labels,
													position => simple_label_cursor,
													new_item => ls);

												-- Collect simple label (ls) in temporarily list of simple labels (lls).
												type_simple_labels.append (lls,ls);

												-- Mark anonymous strand as processed.
												anon_strand_a.processed := true;
											end if;
										end if;

										next (simple_label_cursor); -- advance label cursor
									end loop;

									-- Copy list of simple labels (lls) to current segment (s).
									segment.label_list_simple := lls;
									
									-- Update/replace segment in current anonymous strand.
									type_net_segments.replace_element (
										container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
										position => segment_cursor,
										new_item => segment); -- the updated segment
									
									-- Clean up: Purge temporarily list of simple labels for next spin.
									type_simple_labels.clear (lls);

									-- Update/replace anonymous net in anonymous_nets.
									type_anonymous_strands.replace_element (
										container => anonymous_strands, -- the list of anonymous strands
										position => strand_cursor,
										new_item => anon_strand_a); -- the updated anonymous net
								end if;
								
								-- Loop in list of tag labels:
								if not is_empty (wild_tag_labels) then -- do if if there are tag labels at all
									--put_line(" hierarchic and global labels ...");	 -- CS: log ?

									tag_label_cursor := wild_tag_labels.first; -- reset label cursor
									while tag_label_cursor /= type_tag_labels.no_element loop
										lt := element (tag_label_cursor); -- get tag label
										
										if not lt.processed then								
											if label_sits_on_segment (label => type_net_label (lt), segment => segment) then

												if log_level >= log_threshold + 1 then
													log_indentation_up;
													--log (text => "label at" & to_string (label => type_net_label (lt), scope => xy));
													log (text => "label at" & to_string (label => type_net_label (lt)));
													log_indentation_down;
												end if;

												-- Check if the label does not contradict with other labels of this strand.
												-- Otherwise, set scope according to the label just found.
												case anon_strand_a.scope is
													when unknown => -- find. no label found so far. set scope of strand
														if lt.global then 
															anon_strand_a.scope := global;
														end if;
														if lt.hierarchic then 
															anon_strand_a.scope := hierarchic;
														end if;

													when local => -- strand has been marked as "local" already. no hierarchic or global label allowed !
														if lt.global or lt.hierarchic then
															output_net_label_conflict;
															log (ERROR,
																"local net " & et_general.to_string (anon_strand_a.name) 
																& " has a hierarchic or global label at" 
																--& to_string (position => lt.coordinates) & " !");
																& to_string (point => lt.coordinates) & " !");
															raise constraint_error;
														end if;
														
													when hierarchic => -- strand has been marked as "hierarchic" already. no global label allowed !
														if lt.global then
															output_net_label_conflict;
															log (ERROR,
																"hierarchic net " & et_general.to_string (anon_strand_a.name) 
																& " has a global label at" 
																--& to_string (position => lt.coordinates) & " !");
																& to_string (point => lt.coordinates) & " !");
															raise constraint_error;
														end if;

													when global => -- strand has been marked as "global" already. no hierarchic label allowed !
														if lt.hierarchic then
															output_net_label_conflict;
															log (ERROR,
																"global net " & et_general.to_string (anon_strand_a.name) 
																& " has a hierarchic label at" 
																--& to_string (position => lt.coordinates) & " !");
																& to_string (point => lt.coordinates) & " !");
															raise constraint_error;
														end if;
												end case;

												-- The first matching label dictates the net name and scope. 
												-- If other labels with text differing from net name found, output warning.
												if length (anon_strand_a.name) = 0 then -- If this is the first matching label
													anon_strand_a.name := lt.text; -- assume the label text as net name.
												else
													-- If label text is different from previously assigned net name:
													if anon_strand_a.name /= lt.text then 
														log (ERROR, 
															 "Net " & et_general.to_string (anon_strand_a.name) & " has contradicting label " 
															 --& "at" & to_string (position => lt.coordinates) & " !");
															 & "at" & to_string (point => lt.coordinates) & " !");
														raise constraint_error;
													end if;
												end if;

												-- mark tag label as processed and update/replace it in wild_tag_labels
												lt.processed := true;
												type_tag_labels.replace_element (
													container => wild_tag_labels,
													position => tag_label_cursor,
													new_item => lt);

												-- Collect tag label (lt) in temporarily list of simple labels (llt).
												type_tag_labels.append (llt,lt);

												-- Mark anonymous net as processed.												
												anon_strand_a.processed := true;
											end if;
										end if;

										next (tag_label_cursor);
									end loop;

									-- Copy list of tag labels (llt) to current segment (s).
									segment.label_list_tag := llt;
									
									-- Update/replace segment in current anonymous net.
									type_net_segments.replace_element (
										container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
										position => segment_cursor,
										new_item => segment); -- the updated segment

									-- Clean up: Purge temporarily list of tag labels for next spin.
									type_tag_labels.clear (llt);

									-- Update/replace anonymous net in anonymous_nets.
									type_anonymous_strands.replace_element (
										container => anonymous_strands, -- the list of anonymous strands
										position => strand_cursor,
										new_item => anon_strand_a); -- the updated anonymous net
								end if;

								next (segment_cursor); -- advance segment cursor
							end loop;
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					-- Build name-less strands from anonymous strands.
					-- Anonymous strands have no label, hence no name -> "processed" flag is still cleared.
					-- As placeholder for the name we use the notation "N$n" where n is taken from the net_id (counter of name-less strands)
					-- Their scope is strictly "local".
					--
					-- We build a new strand (of type type_strand_named) in an intermediate variable "strand"
					-- (as specified in et_schematic type_strand_named) for transfer to the module netlist.
					--
					-- NOTE: Even if a strand has no name at this stage, it may get a dedicated name later.
					-- Power-out ports may overwrite the strand name.
					log (text => "building name-less strands ...", level => log_threshold);
					log_indentation_up;

					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get anonymous strand

						if not anon_strand_a.processed then

							-- build temporarily strand with a name like N$542
							net_id := net_id + 1; -- increment net id. net_id applies for the whole design. see declarations of procedure import_design
							net_name := to_net_name (
								anonymous_net_name_prefix & trim (natural'image (net_id), left));

							log (text => et_general.to_string (net_name), level => 2);
							
							strand.name := net_name;
							strand.scope := local;

							log_indentation_up;
							log (text => "scope " & to_string (strand.scope) & " with segments", level => 2);
							
							-- fetch net segments from anonymous strand and append them to the new name-less strand:
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous net
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous strand anon_strand_a
								segment := element (segment_cursor); -- get segment
								type_net_segments.append (container => strand.segments, new_item => segment);
								
								if log_level >= 2 then
									--write_coordinates_of_segment (segment => segment);
									log_indentation_up;
									log (text => to_string (segment => segment, scope => xy));
									log_indentation_down;
								end if;
								
								next (segment_cursor);
							end loop;

							log_indentation_down;
							
                            -- assign coordinates
							set_path (strand.position, path_to_sheet);
							set_sheet (strand.position, sheet_number);

							-- set x,y coordinates (lowest available on the sheet)
							--set_xy (strand.position, lowest_xy (strand, log_threshold + 3));
							set (strand.position, lowest_xy (strand, log_threshold + 3));
                            
							-- insert strand in module, then purge strand.segments for next spin
							log (text => "inserting strand in module ...", level => log_threshold + 2);
							add_strand (strand);

							type_net_segments.clear (strand.segments);
						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;
					
					log_indentation_down;
					
					-- Build named strands with label. Those strands have the "processed" flag set.
					-- NOTE: Even if a strand has a dedicated name at this stage, it may get a dedicated name later on netlist generation.
					-- Power-out ports may overwrite the strand name (which would be regarded as design error and is handled on netlist generation)
					log (text => "building named strands ...", level => log_threshold);
					log_indentation_up;
					
					strand_cursor := anonymous_strands.first; -- reset strand cursor
					while strand_cursor /= type_anonymous_strands.no_element loop
						anon_strand_a := element (strand_cursor);  -- get a strand

						if anon_strand_a.processed then -- it must have a name

							log (text => et_general.to_string (anon_strand_a.name), level => 2);
							
							strand.name := anon_strand_a.name;
							strand.scope := anon_strand_a.scope;

							log_indentation_up;
							log (text => "scope " & to_string (strand.scope) & " with segments", level => 2);

							-- fetch net segments from anonymous strand and append them to the new named strand:
							segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous strand
							while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous_strand "a"
								segment := element (segment_cursor); -- get segment
								type_net_segments.append (container => strand.segments, new_item => segment);
								
								if log_level >= 2 then
									--write_coordinates_of_segment (segment => segment);
									log (text => to_string (segment => segment, scope => xy));
								end if;
								
								next (segment_cursor);
							end loop;

							log_indentation_down;

                            -- assign coordinates
                            set_path (strand.position, path_to_sheet);
							set_sheet (strand.position, sheet_number);

							-- set x,y coordinates (lowest available on the sheet)
							--set_xy (strand.position, lowest_xy (strand, log_threshold + 3));
							set (strand.position, lowest_xy (strand, log_threshold + 3));
							
							-- insert strand in module, then purge strand.segments for next spin
							log (text => "inserting strand in module ...", level => log_threshold + 2);
							add_strand (strand);
							type_net_segments.clear (strand.segments);

						end if;

						next (strand_cursor); -- advance strand cursor
					end loop;

					log_indentation_down;
					
				else
					log (NOTE,
						 "The schematic does not contain nets to associate net labels with !");
				end if;

				log_indentation_down;
			end associate_net_labels_with_anonymous_strands;
			
			procedure process_junctions (log_threshold : in type_log_level) is
			-- Breaks down all net segments where a junction sits on. 
			-- In the end, the number of net segments may increase.

			-- NOTE: The junction to be tested is taken from the wild list of net junctions. This
			-- list contains the junction of the current sheet exclusively.
			
			-- Loops in wild_segments and tests if a junction sits on a segment.
			-- Then splits the segment where the junction sits. If there are junctions left on 
			-- the remaining fragments, they will be detected in the next spin. 
			-- The flag segment_smashed indicates there are no more segments left with a junction.
				segment : type_wild_net_segment;
				junction : type_net_junction;
			
				use type_junctions;
				junction_cursor : type_junctions.cursor; -- points to the junction being processed

				procedure change_segment_start_coordinates (segment : in out type_wild_net_segment) is 
				begin
					segment.coordinates_start := junction.coordinates;
				end change_segment_start_coordinates;
				
				segment_smashed : boolean := true; -- indicates whether a segment has been broken down

				use type_wild_segments;
				segment_cursor : type_wild_segments.cursor; -- points to the current segment
				
			begin -- process_junctions
				log_indentation_up;
				
				-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
				-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
				-- CS NOTE: In this process, segments may evolve, which have junctions not sitting at the segment. A clean up would be useful.
				if not is_empty (wild_junctions) then 
					log (text => "processing" & count_type'image (length (wild_junctions)) & " net junctions ...", level => log_threshold);
					log_indentation_up;
					
					-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
					-- does not increase anymore, all segments are processed.
					while segment_smashed loop
						
						segment_cursor := wild_segments.first;
						loop_s:
						while segment_cursor /= type_wild_segments.no_element loop
						
							segment := type_wild_segments.element (segment_cursor); -- get a segment
							log (text => "probing segment" & to_string (segment => segment, scope => xy), level => log_threshold);

							-- loop in wild junction list until a junction has been found that sits on the segment
							junction_cursor := wild_junctions.first; -- reset junction cursor to begin of junction list
							while junction_cursor /= type_junctions.no_element loop

								-- fetch junction from current cursor position
								junction := type_junctions.element (junction_cursor);
								
								if junction_sits_on_segment (junction, type_net_segment_base (segment)) then -- match

									if log_level >= log_threshold + 1 then
										log_indentation_up;
										log (text => "has junction" & to_string (position => junction.coordinates, scope => xy));
										log_indentation_down;
									end if;
									-- NOTE: junctions sitting on a net crossing may appear twice here.

									-- move start coord. of the current segment to the position of the junction
									type_wild_segments.update_element (
										container	=> wild_segments,
										position	=> segment_cursor,
										process		=> change_segment_start_coordinates'access
										);

									-- replace end coord. of segment by pos. of junction
									segment.coordinates_end := junction.coordinates;

									-- If the junction has not been appended to the segment yet, 
									-- append junction to the segment:
									-- NOTE: junctions may be appended twice if they sit on net crossings.
									-- For this reason we first test if the junction has already been appended.
									if not type_junctions.contains (segment.junctions, junction) then
										type_junctions.append (
											container	=> segment.junctions,
											new_item	=> junction);
									end if;
									
									-- append new segment to list of wild segments
									type_wild_segments.append (
										container	=> wild_segments,
										new_item	=> segment
										);

									exit loop_s;
								end if;

								next (junction_cursor);
							end loop;

							next (segment_cursor);
						end loop loop_s;

						-- Test if segment_count has increased. If yes, set segment_smashed flag so that the wild_segments
						-- can be searched again. Otherwise clear segment_smashed -> end of procedure.
						if type_wild_segments.length (wild_segments) > segment_count then
							segment_smashed := true;
							-- update segment_count (should increment by 1)
							segment_count := type_wild_segments.length (wild_segments);
						else
							segment_smashed := false;							
						end if;
					end loop;

					log_indentation_down;
					log (text => "update: net segments total" & count_type'image (segment_count), level => log_threshold);
				end if;

				log_indentation_down;
			end process_junctions;


			procedure build_anonymous_strands (log_threshold : in type_log_level) is
			-- From the wild segments and junctions assemble net segments to anonymous strands.

				procedure add_strand_to_anonymous_strands is
				-- Once an anonymous strand is complete, it gets appended to a list of anonymous strands. 
				-- Afterward the anonymous strand is deleted. It is a list of net segments which must be purged so that the list
				-- "anonymous_strand" can be filled with net segments of the next anonymous strand.
				begin
					type_anonymous_strands.append (anonymous_strands, anonymous_strand);
					type_net_segments.clear (anonymous_strand.segments);
				end add_strand_to_anonymous_strands;

				use type_wild_segments;

				-- primary and secondary segment cursors.
				segment_cursor_a, segment_cursor_b : type_wild_segments.cursor;

				-- node of the segment (end or start point)
				side : type_segment_side;

				-- the result of a segment search
				search_result : type_same_coord_result;
				
			begin -- build_anonymous_strands
				log_indentation_up;
				
				-- Build anonymous nets:
				-- We are processing the net segments of a sheet here. The net segments have been collected in 
				-- a wild collection of net segments earlier.
				-- This wild collection of segments does not reveal the actual nets where the segments belong to.
				-- The segments are inspected
				-- in the following by looking at the coordinates of their start and end points. 
				-- Segments whose start or end points match other segments are considered
				-- as connected to each other (means they belong to the same strand).
				-- The net name is unknown yet. So the outcome of the following is a list of anonymous strands.
				
				-- CS: handle circlular strands, currently they cause a forever-loop here
				
				segment_count := type_wild_segments.length (wild_segments); -- get number of segments on the current sheet

				log (text => "processing" & count_type'image (segment_count) & " net segments ...", level => log_threshold);

				-- It may happen that a sheet has no nets, for example the top level sheet of a design.
				-- If there are no net segments at all, nothing happens.
				if segment_count > 0 then 

					-- Segments where a junction sits on, must be broken down. This results in more segments than calculated earlier.
					-- The outcome of process_junctions might be a greater number of net segments than currently being held in segment_count.
					process_junctions (log_threshold + 1);
					-- segment_count now has been updated

					log_indentation_up;
					
					-- We inspect one segment after another. segment_cursor_a points to the first segment to be processed. 
					-- A segment, whose "e" AND "s" flag has been set, is to be skipped (because this segment has been processed already).
					-- Variable side_scratch points to the side of the segment (start or end point) where another matching segment
					-- is to be searched for.
					-- If a matching segment is found, it gets appended to the current anonymous strand.

					-- set primary segment cursor to begin of wild segment collection
					segment_cursor_a := wild_segments.first; 

					-- The primary segment cursor advances once an anonymous stand is complete (when all connected segments have been found).
					-- Each time a connected segment has been found, the secondary segment cursor points to that segment.
					while segment_cursor_a /= type_wild_segments.no_element loop
						segment_cursor_b := segment_cursor_a;

						-- Already processed segments are skipped. (Processed segments have the "s" and "e" flag set.)
						if not type_wild_segments.element (segment_cursor_b).s and 
						   not type_wild_segments.element (segment_cursor_b).e then 

						    -- We initiate a new strand and start looking for a matching segment on the end_point:
							--put_line(et_import.report_handle," anonymous net" & positive'image(seg) & ":"); 
							log (text => "assembling strand with segments", level => log_threshold + 1);
							log_indentation_up;

							-- The first segment is to be added to the anonymous strand.
							add_segment_to_anonymous_strand (segment_cursor_b); 
							side := end_point;

							loop -- A
								--put_line(et_import.report_handle,"  --> A"); -- CS: log ?
								
								-- Search for a segment connected to the current segment. 
								-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment
								-- to current anonymous strand.
								-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to indicate
								-- which end point has been processed.
								-- If no connected segment found, toggle side and repeat search_for_same_coordinates
								-- on the opposide of the segment.
								search_result := search_for_same_coordinates (
									segment_cursor	=> segment_cursor_b,
									seg_in			=> type_wild_segments.element (segment_cursor_b),
									side			=> side);

								if search_result.valid then
									--put_line(et_import.report_handle,"  --> E"); -- CS: log ?
									null;
								else
									-- Toggle side_scratch depending on the e/s flag of the segment:
									-- D
 									if type_wild_segments.element (segment_cursor_b).e then
										-- put_line(et_import.report_handle,"  --> D1"); -- CS: log ?
										side := start_point;
									end if;
									
 									if type_wild_segments.element (segment_cursor_b).s then
										-- put_line(et_import.report_handle,"  --> D2"); -- CS: log ?
 										side := end_point;	
 									end if;

									-- C
									--put_line(et_import.report_handle,"  --> C"); -- CS: log ?
									
									-- Search for a segment connected to the current side of the segment.
									-- If function search_for_same_coordinates discovers a suitable segment, 
									-- it adds the segment to current anonymous strand.
									-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to
									-- indicate which end point has been processed.
									-- If no connected segment found, the current anonymous net is considered
									-- as complete -> cancel loop, advance to next segment ...
									search_result := search_for_same_coordinates (
										segment_cursor	=> segment_cursor_b,
										seg_in			=> type_wild_segments.element (segment_cursor_b),
										side			=> side);
									
									if search_result.valid then
										--put_line(et_import.report_handle,"  --> F"); -- CS: log ?
										null;
									else
										--put_line(et_import.report_handle,"  done"); -- CS: log ?
										
										-- All collected segments belong to the same net.
										-- This net is to be added to the list of anonymous nets.
										add_strand_to_anonymous_strands; 	
																			
										exit;	-- no further segment search required.
									end if;
								end if;

								-- B
								--put_line(et_import.report_handle,"  --> B"); -- CS: log ?
								
								-- Update secondary segment_cursor with the cursor of the segment just found 
								-- by search_for_same_coordinates. So the secondary segment cursor now points to the next
								-- connected segment. 
								-- Same_coord_result contains the end point of the segment that has just been found.
								-- Depending on the end point of the matching segment, side must be set so that the
								-- search can continue on the opposide of the new segment.
								segment_cursor_b := search_result.cursor;
								
								case search_result.side is
									when end_point => 
										side := start_point;
									when start_point =>
										side := end_point;
								end case;
							end loop;

							log_indentation_down;
						end if;

						-- advance primary segment cursor
						next (segment_cursor_a);
					end loop;

					log_indentation_down;

				end if;

				log_indentation_down;

			end build_anonymous_strands;

		
			procedure check_header (line : in type_fields_of_line) is
			-- Tests the given line if it contains a valid schematic sheet header.
			-- Sets the flag schematic_headline_processed.
			-- Aborts program if schematic version invalid.
				use et_import;
			begin
				if et_string_processing.field (line,1) = schematic_header_keyword_sys_name and
					et_string_processing.field (line,2) = schematic_header_keyword_schematic and
					et_string_processing.field (line,3) = schematic_header_keyword_file and
					et_string_processing.field (line,4) = schematic_header_keyword_version then
						case cad_format is
							when KICAD_V4 =>
								if positive'value (et_string_processing.field (line,5)) = schematic_version_v4 then
									-- headline ok, version is supported
									schematic_version_valid := true;
								else
									log (ERROR, "schematic version" 
											& positive'image (schematic_version_v4) & " required.",
										console => true);
									raise constraint_error;
								end if;

							when KICAD_V5 =>
								if positive'value (et_string_processing.field (line,5)) = schematic_version_v5 then
									-- CS: currently the version number must exactly match. Range check ?
									-- headline ok, version is supported
									schematic_version_valid := true;
								else
									log (ERROR, "schematic version" 
											& positive'image(schematic_version_v5) & " required.",
										console => true);
									raise constraint_error;
								end if;

							when others => raise constraint_error;
								
						end case;
				end if;
			end check_header;
			
			procedure make_sheet_header (lines : in type_lines.list) is
			-- Builds the sheet header.
			-- The sheet header mainly contains the used libraries.

				sheet_header : type_sheet_header; -- the header being built
			
				--	LIBS:nucleo_core-rescue
				--	LIBS:power
				-- 	LIBS:bel_connectors_and_jumpers
				--	LIBS:bel_primitives
				--	LIBS:bel_stm32
				--	LIBS:nucleo_core-cache
				--	EELAYER 25 0
				--	EELAYER END

				-- This data goes into a the sheet_header. When the schematic file has been
				-- read completely, the sheet_header is appended to global list_of_sheet_headers. 
				-- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.
				-- NOTE: The library entries in the header are not used by kicad. However, they must be read
				-- and stored in sheet_header.libraries.
								
				use type_lines;
			
			begin -- make_sheet_header
				line_cursor := type_lines.first (lines);
				while line_cursor /= type_lines.no_element loop

					--log ("---> C " & to_string (line));
					
					-- Field #1 of the line must be broken down by its own ifs in order to get "LIBS" and "bel_stm32"
					if get_field_from_line (et_string_processing.field (et_kicad.line,1), 1, latin_1.colon) = schematic_library then

						-- for the log: write library name
						log (text => "uses library " & get_field_from_line (et_string_processing.field (et_kicad.line,1), 2, latin_1.colon),
							level => log_threshold + 1);

						-- Store bare library name in the list sheet_header.libraries:
						-- We use a doubly linked list because the order of the library names must be kept.
						type_library_names.append (
							container	=> sheet_header.libraries,
							new_item	=> et_kicad_general.to_library_name (
								get_field_from_line (et_string_processing.field (et_kicad.line,1), 2, latin_1.colon))
							);

					end if;

					-- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
					-- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
					if et_string_processing.field (et_kicad.line,1) = schematic_eelayer then
						if et_string_processing.field (et_kicad.line,2) = schematic_eelayer_end then
							null;
						else
							-- append layer numbers to the sheet header
							sheet_header.eelayer_a := positive'value(
								et_string_processing.field (et_kicad.line,2));

							sheet_header.eelayer_b := natural'value(
								et_string_processing.field (et_kicad.line,3));
						end if;
					end if;

					next (line_cursor);
				end loop;

				-- Add sheet_header to module.
                -- NOTE: The file name serves as key in order to map from file to header.
				add_sheet_header (
					header	=> sheet_header,
					sheet	=> current_schematic.sheet.file);

			end make_sheet_header;

			procedure make_drawing_frame (
			-- Builds the drawing frame.
			-- CS: Read lines and position of text placeholders from
			-- *.kicad_wks file (either the default file or the one specified
			-- in the project file by a line like "PageLayoutDescrFile=/home/user/tmp/sheet.kicad_wks".
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is

				frame : type_frame; -- a single drawing frame
			
				-- These are the components of the title block. At the end
				-- of this procedure they are assembled to a final title block:
				title_block_text 	: et_libraries.type_title_block_text; -- a single text within the title block
				title_block_texts 	: et_libraries.type_title_block_texts.list; -- a list of title block texts
				--title_block 		: et_libraries.type_title_block; -- a full title block
				
				-- If the description reveals there is more than one sheet, we have a hierarchic design. Means we
				-- need to read follwing sheet sections.
				-- The sheet_number_current obtained here serves as part of the coordinates of objects found on this sheet.
				-- The sheet description looks like this:

				-- $Descr A4 11693 8268
				-- encoding utf-8
				-- Sheet 5 8
				-- Title ""
				-- Date ""
				-- Rev ""
				-- Comp ""
				-- Comment1 ""
				-- Comment2 ""
				-- Comment3 ""
				-- Comment4 ""
				-- $EndDescr
				
				use type_lines;
			
			begin -- make_drawing_frame
				log (text => "making drawing frame ...", level => log_threshold);
				log_indentation_up;
			
				line_cursor := type_lines.first (lines);

				-- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
				-- CS test field count				
				frame.paper_size	:= et_general.to_paper_size (et_string_processing.field (et_kicad.line,2));

				-- The sheet size seems to be ignored by kicad. Only the paper_size matters.
				frame.size_x		:= mil_to_distance (et_string_processing.field (et_kicad.line,3)); 
				frame.size_y 		:= mil_to_distance (et_string_processing.field (et_kicad.line,4)); 
				
				--frame.coordinates.path := path_to_submodule;
				set_path (frame.coordinates, path_to_sheet);

				-- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
				-- kicad built-in things and remain unassigned here.

				next (line_cursor);

				-- read endcoding from a line like "encoding utf-8"
				-- CS: checks only for a non-default endcoding and outputs a warning.
				-- CS: we assume only one encoding. other encodings are ignored currently.
				-- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
				-- good idea.
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_encoding then
					-- CS test field count
					if et_string_processing.field (et_kicad.line,2) /= encoding_default then
						log (WARNING, "non-default endcoding '" 
							 & et_string_processing.field (et_kicad.line,2) & "' found !");
					end if;
				end if;

				next (line_cursor);

				-- Log sheet number on encountering a line like "Sheet 1 7"
				-- NOTE: The sheet number written here (field 2) has no meaning. The real sheet number is 
				-- obtained by reading the value of sheet_number. sheet_number is has been incremented
				-- before function read_schematic was called.
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_sheet then
					-- CS test field count

					-- The sheet number written here is meaningless:
					--sheet_number_current := to_sheet_number (field (et_kicad.line,2));
					-- Instead we log the global sheet_number:
					log (text => "sheet number" & to_sheet (sheet_number), level => log_threshold + 1);

					-- Get the total number of sheet of this design. 
					sheet_count_total := to_sheet (et_string_processing.field (et_kicad.line,3));
					
					-- CS: sheet_count_total must not change from sheet to sheet. Check required.
					if sheet_count_total > 1 then
						-- Set in the hierarchic_sheet_file_names (to be returned) the parent_sheet name. The schematic file 
						-- being processed (see input parameters of read_schematic) becomes the parent sheet
						-- of the sheet here.
						hierarchic_sheet_file_names.parent_sheet := to_submodule_name (
							to_string (current_schematic.sheet.file));
					end if;
					-- CS: make sure total sheet count is less or equal current sheet number.

					-- Our temporarily drawing frame gets the current sheet number assigned.
					set_sheet (frame.coordinates, sheet_number);
				end if;						

				next (line_cursor);

				-- read sheet title from a line like "Title "abc""
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_title then                        
					log (text => "sheet title", level => log_threshold + 1);
					
					title_block_text.meaning := et_libraries.TITLE;

					-- CS test field count										
					title_block_text.text := et_libraries.type_title_block_text_content.to_bounded_string (
												  (et_string_processing.field (et_kicad.line,2)));
					
					et_libraries.type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);
				
				-- read date from a line like "Date "1981-01-23""
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_date then
					log (text => "sheet date", level => log_threshold + 1);
					
					-- CS test field count					
					title_block_text.meaning := et_libraries.DRAWN_DATE;
					title_block_text.text := et_libraries.type_title_block_text_content.to_bounded_string (
												(et_string_processing.field (et_kicad.line,2)));

					et_libraries.type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);
				
				-- read revision from a line like "Rev "9.7.1"
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_revision then                        
					log (text => "sheet revision", level => log_threshold + 1);
					
					-- CS test field count					
					title_block_text.meaning := et_libraries.REVISION;
					title_block_text.text := et_libraries.type_title_block_text_content.to_bounded_string (
												(et_string_processing.field (et_kicad.line,2)));
					
					et_libraries.type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);

				-- read company name
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_company then
					log (text => "sheet company name", level => log_threshold + 1);
					
					-- CS test field count					
					title_block_text.meaning := et_libraries.COMPANY;
					title_block_text.text := et_libraries.type_title_block_text_content.to_bounded_string (
												(et_string_processing.field (et_kicad.line,2)));

					et_libraries.type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				next (line_cursor);

				-- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
				if  et_string_processing.field (et_kicad.line,1) = schematic_keyword_comment_1 or
					et_string_processing.field (et_kicad.line,1) = schematic_keyword_comment_2 or
					et_string_processing.field (et_kicad.line,1) = schematic_keyword_comment_3 or 
					et_string_processing.field (et_kicad.line,1) = schematic_keyword_comment_4 then

					log (text => "sheet comment", level => log_threshold + 1);
					
					-- CS test field count
					title_block_text.meaning := et_libraries.MISC;
					title_block_text.text := et_libraries.type_title_block_text_content.to_bounded_string (
												(et_string_processing.field (et_kicad.line,2)));

					et_libraries.type_title_block_texts.append (title_block_texts, title_block_text);
				end if;

				-- FINALIZE
				frame.title_block.texts := title_block_texts; -- assign collected texts list to temporarily title block
				-- CS: x/y coordinates and list of lines of a title block are kicad built-in things and 
				-- thus not available here. -> x/y assume default values (0/0).
				-- See comment above in header of this procedure.

				-- purge temporarily texts
				et_libraries.type_title_block_texts.clear (title_block_texts);

				-- append temporarily drawing frame to module
				add_frame (frame);

				log_indentation_down;

				exception
					when event:
						others =>
							log_indentation_reset;
							log (text => ada.exceptions.exception_message (event));
							raise;
				
			end make_drawing_frame;


			procedure make_gui_sheet (
			-- Builds the hierachic sheet.
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is

				sheet		: type_hierarchic_sheet; -- the hierarchical sheet being built
				sheet_name	: type_hierarchic_sheet_name; -- incl. file name and sheet name

				port_inserted	: boolean; -- used to detect multiple ports with the same name
				port_cursor		: type_hierarchic_sheet_ports.cursor; -- obligatory, but not read

				use type_lines;
				use type_submodule_name;
				use et_libraries;
				use conventions;

				text_size : pac_text.type_text_size; -- temporarily storage of a text size before being checked
			
				function to_direction (dir_in : in string) return type_port_direction is
				-- Converts a string to type_port_direction.
					result : type_port_direction;
					dir : type_sheet_port_direction; -- see et_kicad.ads
				begin
					dir := type_sheet_port_direction'value (dir_in);
					
					case dir is
						when I => result := INPUT;
						when O => result := OUTPUT;
						when B => result := BIDIR;
						when T => result := TRISTATE;
						when U => result := PASSIVE;
					end case;

					log_indentation_up;
					log (text => to_string (result), level => log_threshold + 2);
					log_indentation_down;
					
					return result;

					exception
						when constraint_error =>
							log (ERROR, "invalid port direction '" 
								 & dir_in & "' !");
							-- CS: provide more details
							raise;

				end to_direction;

				function to_orientation (or_in : in string) return et_coordinates.type_rotation is
				-- Converts a string to type_rotation
					result : et_coordinates.type_rotation;
					orientation : type_sheet_port_orientation; -- see et_kicad.ads
				begin
					orientation := type_sheet_port_orientation'value (or_in);

					case orientation is
						when R => result := et_coordinates.type_rotation (0.0);
						when L => result := et_coordinates.type_rotation (180.0);
					end case;
					
					return result;

					exception
						when constraint_error =>
							log (ERROR, "invalid port orientation '" 
								 & or_in & "' !");
							-- CS: provide more details
							raise;

				end to_orientation;
					
			begin -- make_gui_sheet
				log (text => "making gui sheet ...", level => log_threshold);
				log_indentation_up;
				
				line_cursor := type_lines.first (lines);
-- 				log (text => to_string (et_kicad.line), level => log_threshold + 1);

				-- read GUI sheet position and size from a line like "S 4050 5750 1050 650"
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_sheet_pos_and_size then
					-- CS test field count
					set_path (sheet.coordinates, path_to_sheet);
					--log (text => "path " & to_string (path (sheet.coordinates)));
					set_sheet (sheet.coordinates, sheet_number);
					
					--set_x (sheet.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,2)));
					set (X, mil_to_distance (et_string_processing.field (et_kicad.line,2)), sheet.coordinates);
					--set_y (sheet.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,3)));
					set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,3)), sheet.coordinates);

					sheet.size_x := mil_to_distance (et_string_processing.field (et_kicad.line,4));
					sheet.size_y := mil_to_distance (et_string_processing.field (et_kicad.line,5));                                
				end if;

				next (line_cursor);
-- 				log (text => to_string (et_kicad.line), level => log_threshold + 1);
				
				-- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_sheet_timestamp then 
					-- CS test field count					
					sheet.timestamp := type_timestamp (et_string_processing.field (et_kicad.line,2));
				end if;

				next (line_cursor);
				
				-- Read sheet name from a line like "F0 "mcu_stm32f030" 60"
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_sheet_name then
					-- CS test field count					
					sheet_name.name := to_submodule_name (et_string_processing.field (et_kicad.line,2));

					-- set text size of sheet name and test for excessive text size.
					sheet.text_size_of_name := to_text_size (mil_to_distance (et_string_processing.field (et_kicad.line,3)));

					-- Test text size by category.
					check_schematic_text_size (category => conventions.SHEET_NAME, size => sheet.text_size_of_name);
				end if;

				next (line_cursor);
				
				-- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
				if et_string_processing.field (et_kicad.line,1) = schematic_keyword_sheet_file then
					-- CS test field count					
					sheet_name.file := to_schematic_file_name (et_string_processing.field (et_kicad.line,2));
					
					-- set text size of file name and test for excessive text size
					sheet.text_size_of_file := to_text_size (mil_to_distance (et_string_processing.field (et_kicad.line,3)));

					-- Test text size by category.
					check_schematic_text_size (category => FILE_NAME, size => sheet.text_size_of_file);
					
					-- Append sheet file name to hierarchic_sheet_file_names. 
					-- This list will be returned by this function (we are in read_schematic) to the calling
					-- parent unit (import_design).
					type_hierarchic_sheet_file_names.append (
						container	=> hierarchic_sheet_file_names.sheets,
						new_item	=> (
										sheet		=> sheet_name, -- incl. file name and sheet name
										timestamp	=> sheet.timestamp)); -- B5D45A33
				end if;

				log (text => "hierarchic sheet " & to_string (submodule => sheet_name.name), level => log_threshold + 1);
				
				-- Read sheet ports from a line like "F2 "SENSOR_GND" I R 2250 3100 60".
				-- The index after the F is a successive number that increments on every port:
				-- So the next port would be "F3 "SENSOR_VCC" I R 2250 3300 60" ...
				next (line_cursor);

				-- Read ports of hierachic sheet if any. Otherwise output a warning.
				-- If no ports available, the line cursor points to a no_element.
				if line_cursor /= no_element then
					
					-- Test of excessive text size.
					text_size := to_text_size (mil_to_distance (et_string_processing.field (et_kicad.line, 7)));

					-- Test text size by category.
					check_schematic_text_size (category => PORT_NAME, size => text_size);
					
					while line_cursor /= no_element loop
						log_indentation_up;
						log (text => "port " & strip_quotes (et_string_processing.field (et_kicad.line, 2)), level => log_threshold + 2);

						-- add port
						type_hierarchic_sheet_ports.insert (
							container => sheet.ports,
							key => to_net_name (et_string_processing.field (et_kicad.line, 2)), -- port name
							new_item => (
								direction 	=> to_direction (et_string_processing.field (et_kicad.line, 3)),
								orientation	=> to_orientation (et_string_processing.field (et_kicad.line, 4)),
								coordinates	=> to_point (et_string_processing.field (et_kicad.line, 5), et_string_processing.field (et_kicad.line, 6)),
								text_size	=> text_size,
								processed	=> false),
							inserted => port_inserted,
							position => port_cursor
							);

						-- if port could not be inserted -> abort
						if not port_inserted then
							log (ERROR, "multiple usage of port " & et_string_processing.field (et_kicad.line, 2) & " !");
							raise constraint_error;
						end if;
						
						log_indentation_down;
						next (line_cursor);
					end loop;

				else -- sheet has no ports -> warning
					log (WARNING, "hierarchic sheet " & to_string (submodule => sheet_name.name) & " has no ports !");
				end if;

				-- insert the hierarchical sheet in module (see type_module)
				add_hierarchic_sheet (sheet_name, sheet);

				log_indentation_down;
				
				exception
					when event:
						others =>
							log_indentation_reset;
							--log (message_error , console => true);
							log (text => ada.exceptions.exception_message (event));
							raise;

			end make_gui_sheet;


			function net_segment_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a net segment header like "Wire Wire Line"
				result : boolean := false;
			begin
				-- CS test field count
				if et_string_processing.field (line,1) = schematic_keyword_wire then
					if et_string_processing.field (line,2) = schematic_keyword_wire then
						if et_string_processing.field (line,3) = schematic_keyword_line then
							result := true;
						end if;
					end if;
				end if;
				return result;
			end net_segment_header;
			
			procedure make_net_segment (
				lines			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a net segment and appends it to the collection of wild segments.

				-- After the segment heaser "Wire Wire Line" the next line like
				-- "2250 3100 2400 3100" is read here. It contains start and end points 
				-- of a net segment.
			
				segment : type_wild_net_segment; -- the segment being built
			begin
				--log (text => "making net segment ...", level => log_threshold);
				--log_indentation_up;

				line_cursor := type_lines.first (lines);
				
				-- Build a temporarily net segment with fully specified coordinates:
				set_path (segment.coordinates_start, path_to_sheet);
				set_path (segment.coordinates_end, path_to_sheet);
				
				-- The sheet number.
				set_sheet (segment.coordinates_start, sheet_number);
				set_sheet (segment.coordinates_end, sheet_number);

				-- the x/y position
				--set_x (segment.coordinates_start, mil_to_distance (et_string_processing.field (et_kicad.line,1)));
				set (X, mil_to_distance (et_string_processing.field (et_kicad.line,1)), segment.coordinates_start);
				--set_y (segment.coordinates_start, mil_to_distance (et_string_processing.field (et_kicad.line,2)));
				set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,2)), segment.coordinates_start);
				--set_x (segment.coordinates_end, mil_to_distance (et_string_processing.field (et_kicad.line,3)));
				set (X, mil_to_distance (et_string_processing.field (et_kicad.line,3)), segment.coordinates_end);
				--set_y (segment.coordinates_end, mil_to_distance (et_string_processing.field (et_kicad.line,4)));
				set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,4)), segment.coordinates_end);

				-- Ignore net segments with zero length (CS: for some reason they may exist. could be a kicad bug)
				-- If a net segment has zero length, issue a warning.
				if length (segment) > zero then 

					-- The net segments are to be collected in a wild list of segments for later sorting.
					log (text => "net segment" & to_string (segment => segment, scope => xy), level => log_threshold);
					
					type_wild_segments.append (wild_segments, segment);
				else -- segment has zero length
					log (WARNING, affected_line (et_kicad.line) & "Net segment with zero length found -> ignored !");
				end if; -- length

				--log_indentation_down;
			end make_net_segment;

			function junction_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a net junction "Connection ~ 4650 4600"
				result : boolean := false;
			begin
				if et_string_processing.field_count (line) = 4 then
					if et_string_processing.field (line,1) = schematic_keyword_connection then
						if et_string_processing.field (line,2) = schematic_tilde then
							result := true;
						end if;
					end if;
				end if;
				return result;
			end junction_header;

			procedure make_junction (
				line			: in type_fields_of_line;
				log_threshold	: in type_log_level) is
			-- Builds a net junction and stores it both in the 
			-- junction list of the module (for statistics, ERC, ...) 
			-- AND in the wild list junctions.
			-- The wild list is needed when the anonymous strands of
			-- the sheet are built (see procedure build_anonymous_strands).
			-- The wild list contains the junction of the current sheet exclusively.
				junction : type_net_junction;  -- the junction being built

				procedure append_junction (
				-- add junction to module.junctions
					module_name : in type_submodule_name.bounded_string;
					module		: in out type_module) is
				begin
					type_junctions.append (
						container	=> module.junctions,
						new_item	=> junction);
				end append_junction;
				
			begin -- make_junction
				--log (text => "making net junction ...", level => log_threshold);
				--log_indentation_up;
				
				set_path (junction.coordinates, path_to_sheet);
				set_sheet (junction.coordinates, sheet_number);
				
				--set_x (junction.coordinates, mil_to_distance (et_string_processing.field (line,3)));
				set (X, mil_to_distance (et_string_processing.field (line,3)), junction.coordinates);
				
				--set_y (junction.coordinates, mil_to_distance (et_string_processing.field (line,4)));
				set (Y, mil_to_distance (et_string_processing.field (line,4)), junction.coordinates);

				-- for the log
				log (text => "net junction" & to_string (junction => junction, scope => xy), level => log_threshold);

				-- add to wild list of junctions
				type_junctions.append (wild_junctions, junction);

				-- add to module.junctions
				type_modules.update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> append_junction'access);

				--log_indentation_down;
			end make_junction;

			function simple_label_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a simple label like 
			-- "Text Label 2350 3250 0 60 ~ 0"
				result : boolean := false;
			begin
				if et_string_processing.field_count (line) = 8 then
					if 	et_string_processing.field (line,1) = schematic_keyword_text and 
						et_string_processing.field (line,2) = schematic_keyword_label_simple then
							result := true;
					end if;
				end if;
				return result;
			end simple_label_header;

			procedure make_simple_label (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a simple net label and appends it to the collection of wild simple labels.

				-- The label header "Text Label 2350 3250 0 60 ~ 0" and the next line like
				-- "net_name_abc" is read here. It contains the supposed net name.

				use conventions;
				
				label : type_net_label_simple; -- the label being built
			begin
				--log (text => "simple label", level => log_threshold + 1);
				--log_indentation_up;
				
				line_cursor := type_lines.first (lines);

				-- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
				--set_path (label.coordinates, path_to_sheet);
				--set_sheet (label.coordinates, sheet_number);
				--set_x (label.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,3)));
				set (X, mil_to_distance (et_string_processing.field (et_kicad.line,3)), label.coordinates);
				--set_y (label.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,4)));
				set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,4)), label.coordinates);

				label.rotation := to_angle (et_string_processing.field (et_kicad.line,5));
				label.size := mil_to_distance (et_string_processing.field (et_kicad.line,6));
				label.style := to_text_style (style_in => et_string_processing.field (et_kicad.line,7), text => true);
				label.width := type_text_line_width'value (et_string_processing.field (et_kicad.line,8));

				next (line_cursor);

				-- Make sure the label text (later this will be a net name) is not longer
				-- than allowed.
				check_net_name_length (et_string_processing.field (et_kicad.line,1));
				
				-- get label text and put it to temporarily simple label
				label.text := to_net_name (et_string_processing.field (et_kicad.line,1));

				-- Make sure there are no forbidden characters in the net name.
				check_net_name_characters (label.text);
				
				-- for the log
				--log (text => "simple label" & to_string (label => type_net_label (label), scope => xy), level => log_threshold);
				log (text => "simple label" & to_string (label => type_net_label (label)), level => log_threshold);

				check_schematic_text_size (category => net_label, size => label.size);
				-- CS: check label style
				-- CS: check label line width
				
				-- The simple labels are to be collected in a wild list of simple labels.
				type_simple_labels.append (wild_simple_labels, label);

				--log_indentation_down;
			end make_simple_label;

			function tag_label_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a global or hierarchic label like 
			-- "Text HLabel 2700 2000 0 60 Input ~ 0" or
			-- "Text GLabel 4700 3200 1 60 UnSpc ~ 0"
				result : boolean := false;
			begin
				if et_string_processing.field_count (line) = 9 then
					if et_string_processing.field (line,1) = schematic_keyword_text and 
						(et_string_processing.field (line,2) = schematic_keyword_label_hierarchic or
						et_string_processing.field (line,2) = schematic_keyword_label_global) then
							result := true;
					end if;
				end if;
				return result;
			end tag_label_header;

			procedure make_tag_label (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a global or hierachical label and appends it to the collection of wild tag labels.

				-- The label header "Text GLabel 4700 3200 1 60 UnSpc ~ 0" and the next line like
				-- "net_name_abc" is read here. It contains the supposed net name.

				use conventions;
				
				label : type_net_label_tag; -- the label being built
			begin
				--log (text => "making tag label ...", level => log_threshold);
				--log_indentation_up;

				line_cursor := type_lines.first (lines);

				-- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
				-- The keyword in field 2 tells whether we have a hierarchic or global label:
				if et_string_processing.field (et_kicad.line,2) = schematic_keyword_label_hierarchic then
					label.hierarchic := true;
					label.global := false;
				else
					label.hierarchic := false;
					label.global := true;
				end if;

				--set_path (label.coordinates, path_to_sheet);
				--set_sheet (label.coordinates, sheet_number);
				--set_x (label.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,3)));
				set (X, mil_to_distance (et_string_processing.field (et_kicad.line,3)), label.coordinates);
				--set_y (label.coordinates, mil_to_distance (et_string_processing.field (et_kicad.line,4)));
				set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,4)), label.coordinates);

				label.rotation := to_angle (et_string_processing.field (et_kicad.line,5));
				label.direction := to_direction (et_string_processing.field (et_kicad.line,7));

				-- build text attributes from size, font and line width
				label.size := mil_to_distance (et_string_processing.field (et_kicad.line,6));
				label.style := to_text_style (style_in => et_string_processing.field (et_kicad.line,8), text => true);
				label.width := type_text_line_width'value (et_string_processing.field (et_kicad.line,9));

				next (line_cursor);

				-- Make sure the label text (later this will be a net name) is not longer
				-- than allowed.
				check_net_name_length (et_string_processing.field (et_kicad.line,1));
				
				-- get label text
				label.text := to_net_name (et_string_processing.field (et_kicad.line,1));
				
				-- Make sure there are no forbidden characters in the net name.
				check_net_name_characters (label.text);

				-- for the log
				--log (text => "tag label" & to_string (label => type_net_label (label), scope => xy), level => log_threshold);
				log (text => "tag label" & to_string (label => type_net_label (label)), level => log_threshold);

				check_schematic_text_size (category => net_label, size => label.size);
				-- CS: check style and line width
				
				-- The tag labels are to be collected in a wild list of tag labels for later sorting.
				type_tag_labels.append (wild_tag_labels, label);

				--log_indentation_down;
			end make_tag_label;

			function text_note_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a text note like
			-- Text Notes 7100 6700 0 67 Italic 13
			-- ET Test Circuit
				result : boolean := false;
			begin
				if et_string_processing.field_count (line) = 8 then
					if et_string_processing.field (line,1) = schematic_keyword_text and 
						et_string_processing.field (line,2) = schematic_keyword_note then
							result := true;
					end if;
				end if;
				return result;
			end text_note_header;

			procedure make_text_note (
				lines			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a text note and appends it to the collection of text notes.

				-- The label header "Text Notes 3400 2800 0 60 Italic 12" and the next line like
				-- "ERC32 Test Board" is read here. It contains the actual text.

				use et_symbols.pac_text;
				
				note : type_text; -- the text note being built
				rotation : et_coordinates.type_rotation;

				procedure warn is begin 
					log (WARNING, " text note at " 
						& kicad_coordinates.to_string (position => note.position, scope => SHEET) 
						& " might be misplaced !");
				end;
				
			begin -- make_text_note
				--log (text => "making text note ...", level => log_threshold);
				--log_indentation_up;
				
				line_cursor := type_lines.first (lines);

				-- set coordinates
				set_path (note.position, path_to_sheet);
				set_sheet (note.position, sheet_number);
				--set_x (note.position, mil_to_distance (et_string_processing.field (et_kicad.line,3)));
				set (X, mil_to_distance (et_string_processing.field (et_kicad.line,3)), note.position);
				--set_y (note.position, mil_to_distance (et_string_processing.field (et_kicad.line,4)));
				set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,4)), note.position);
				
				--note.rotation := to_angle (et_string_processing.field (et_kicad.line,5));
				rotation := to_angle (et_string_processing.field (et_kicad.line,5));

				-- Notes might be upside down or readable from the left. So we must fit the rotation
				-- into a range between 0 and 90 degree:
				if rotation < 0.0 then
					note.rotation := 90.0;
					warn;
				elsif rotation > 90.0 and rotation < 270.0 then
					note.rotation := 0.0;
					warn;					
				elsif rotation > 270.0 then
					note.rotation := 90.0;
					warn;
				end if;

				-- set text size and check for excessive size
				note.size := to_text_size (mil_to_distance (et_string_processing.field (et_kicad.line,6)));
				
				note.style := to_text_style (style_in => et_string_processing.field (et_kicad.line,7), text => true);

				-- If the line width is too small, assume default and issue warning:
				if mil_to_distance (et_string_processing.field (et_kicad.line,8)) < pac_text.type_text_line_width'first then
					log (WARNING, "Line width too small. Defaulting to minimal width !");
					note.line_width := pac_text.type_text_line_width'first;
				else
					note.line_width := mil_to_distance (et_string_processing.field (et_kicad.line,8));
				end if;

				next (line_cursor);

				-- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
				-- CS: store lines in a list of lines instead ?
				-- CS: Currently we store the line as it is in tmp_note.text
				note.content := et_text.to_content (to_string (line));

				write_note_properties (note, log_threshold);
				
				-- the notes are to be collected in the list of notes
				add_note (note);

				--log_indentation_down;
			end make_text_note;

			function component_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a header of a component.
			-- The header is "$Comp"	
			begin
				if et_string_processing.field_count (line) = 1 then
					if et_string_processing.field (line,1) = schematic_component_header then
						return true;
					else 
						return false;
					end if;
				else
					return false;
				end if;
			end component_header;

			function component_footer (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a footer of a component.
			-- The footer is "$EndComp"
			begin
				if et_string_processing.field_count (line) = 1 then
					if et_string_processing.field (line,1) = schematic_component_footer then
						return true;
					else 
						return false;
					end if;
				else
					return false;
				end if;
			end component_footer;

			
			procedure make_component (
				lines 			: in type_lines.list;
				log_threshold	: in type_log_level) is
			-- Builds a unit or a component and inserts it in the component list of 
			-- current module. The information required to make a component is provided
			-- in parameter "lines".

			-- Some entries of the component section are relevant for the whole component.
			-- Some entries are unit specific.
			-- The component section looks like this example:
			
			-- V4: L 74LS00 U1				-- component specific
			-- V5: L bel_logic:74LS00 U1	-- component specific
			-- U 4 1 5965E676	-- unit 1 of 4, link to package in board file
			-- P 4100 4000		-- unit position x/y
			-- AR Path="/59F17F77/5A991798" Ref="LED1"  Part="1" -- alternative reference
			-- AR Path="/5B7E59F3/5A991798" Ref="LED50"  Part="1" 
			-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
			-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
			-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
			-- F 3 "" H 4100 4000 50  0001 C CNN
			-- 	4    4100 4000		-- same as x/y pos

			--  1    0    0  -1  -- orientation 0,   mirror normal
			--  0   -1   -1   0  -- orientation 90,  mirror normal
			-- -1    0    0   1  -- orientation 180, mirror normal 
			-- 	0    1    1   0  -- orientation -90, mirror normal  

			-- 	1    0    0   1  -- orientation 0,   mirror --
			--  0   -1    1   0  -- orientation 90,  mirror -- 
			-- -1    0    0  -1  -- orientation 180, mirror -- 
			--  0    1   -1   0  -- orientation -90, mirror -- 

			-- -1    0    0  -1  -- orientation 0,   mirror |
			--  0    1   -1   0  -- orientation 90,  mirror |
			--  1    0    0   1  -- orientation 180, mirror |
			--  1    0    0   1  -- orientation -90, mirror |

				use et_devices;
				use et_libraries;
				use et_string_processing;

				reference					: type_device_name;	-- like IC5	
				appearance					: type_device_appearance := SCH; -- CS: why this default ?
				generic_name_in_lbr			: type_component_generic_name.bounded_string; -- like TRANSISTOR_PNP

				-- V5:
				component_library_name		: type_library_name.bounded_string; -- the name of the component library like bel_logic
				
				alternative_references		: type_alternative_references.list;
				unit_name					: type_unit_name.bounded_string; -- A, B, PWR, CT, IO-BANK1 ...
				position					: kicad_coordinates.type_position;
				orientation					: et_coordinates.type_rotation;
				mirror						: type_mirror;
				timestamp					: type_timestamp; -- 59F202F2
				alternative_representation	: type_de_morgan_representation;
			
				-- These are the "field found flags". They signal if a particular text field has been found.
				-- They are evaluated once the given lines are read completely.
				field_reference_found		: boolean := false;
				field_value_found			: boolean := false;
				field_package_found			: boolean := false;
				field_datasheet_found		: boolean := false;

				-- These are the actual fields that describe the component more detailled.
				-- They are contextual validated once the given lines are read completely.
				field_reference		: type_text_placeholder (meaning => NAME); -- like IC5 (redundant information with referenc, see above)
				field_value			: type_text_placeholder (meaning => VALUE);	-- like 74LS00
				field_package		: type_text_placeholder (meaning => PACKGE); -- like "bel_primiteves:S_SOT23"
				field_datasheet		: type_text_placeholder (meaning => DATASHEET); -- might be useful for some special components
			
				function to_field return type_text_placeholder is
				-- Converts a field like "F 1 "green" H 2700 2750 50  0000 C CNN" to a type_text_placeholder
					text_position : type_point;
					size : pac_text.type_text_size;

					use et_text;
				begin
					-- test if the field content is longer than allowed:
					check_text_content_length (et_string_processing.field (et_kicad.line,3));
					
					--set_x (text_position, mil_to_distance (et_string_processing.field (et_kicad.line,5)));
					set (X, mil_to_distance (et_string_processing.field (et_kicad.line,5)), text_position);
					--set_y (text_position, mil_to_distance (et_string_processing.field (et_kicad.line,6)));
					set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,6)), text_position);

					size := mil_to_distance (et_string_processing.field (et_kicad.line,7));

					return (
						-- read text field meaning
						meaning 	=> to_text_meaning (line => et_kicad.line, schematic => true),

						-- read content like "N701" or "NetChanger" from field position 3
						content		=> to_content (et_string_processing.field (et_kicad.line,3)),

						-- read rotation like "H"
						rotation	=> to_field_orientation (et_string_processing.field (et_kicad.line,4)),

						-- read coordinates
						position	=> text_position,
										
						size		=> size,
						style		=> to_text_style (style_in => et_string_processing.field (et_kicad.line,10), text => false),
						line_width	=> text_line_width_default,

						-- build text visibility
						--visible		=> to_field_visible (
						--					vis_in		=> et_string_processing.field (et_kicad.line,8),
						--					schematic	=> true),

						-- build text alignment
						alignment	=> (
										horizontal	=> to_alignment_horizontal (et_string_processing.field (et_kicad.line,9)),
										vertical	=> to_alignment_vertical   (et_string_processing.field (et_kicad.line,10)))
						);
				end to_field;

				procedure check_text_fields (log_threshold : in type_log_level) is 
				-- Tests if any "field found" flag is still cleared and raises an alarm in that case.
				-- Perfoms a CONTEXTUAL VALIDATION of the text fields before they are used to 
				-- assemble and insert the component into the component list of the module.

					use conventions;
				
					procedure missing_field (m : in type_placeholder_meaning) is begin
						log (ERROR,
								"component " & to_string (reference) 
								& latin_1.space
								& to_string (position => position)
								& latin_1.lf
								& "text field " & to_string (m) & " missing !",
							console => true);
						
						raise constraint_error;
					end missing_field;

					procedure process_alternative_references is
					-- Looks up alternative_references. The are provided in the schematic file in lines like:
					-- AR Path="/5B7CFC57/5A991D18" Ref="RPH19"  Part="1" 
					-- AR Path="/59F17FDE/5A991D18" Ref="RPH1"  Part="1" 
					-- The line with prenultimate timestamp (59F17FDE) that matches the current_schematic.timestamp
					-- dictates the new component reference (RPH1).
						use type_alternative_references;
						alt_ref_cursor : type_alternative_references.cursor := alternative_references.first;
						suitable_reference_found : boolean := false;

						procedure query_path (alt_ref : in type_alternative_reference) is
						-- queries paths like /59F17FDE/5A991D18 and compares the prenultimate timestamp
						-- with the current_schematic.timestamp. Sets the suitable_reference_found flag on match.
						-- Overwrites preliminary reference and content of field_reference.
							use type_alternative_reference_path;
							timestamp_cursor : type_alternative_reference_path.cursor := alt_ref.path.last;
						begin
							timestamp_cursor := previous (timestamp_cursor);
							if element (timestamp_cursor) = current_schematic.timestamp then
								
								reference := alt_ref.reference;
								field_reference.content := et_text.to_content (et_libraries.to_string (alt_ref.reference));
								suitable_reference_found := true;

								log (text => "update due to hierachic structure: " &
									 et_libraries.to_string (reference), 
									 level => make_component.log_threshold);
							end if;
						end query_path;
							
					begin -- process_alternative_references
						-- loop in list of alternative references and exit once a suitable one was found.
						while alt_ref_cursor /= type_alternative_references.no_element loop

							type_alternative_references.query_element (
								position	=> alt_ref_cursor,
								process		=> query_path'access);

							if suitable_reference_found then exit; end if;
							
							next (alt_ref_cursor);
						end loop;
					end process_alternative_references;
					
				begin -- check_text_fields
					log_indentation_up;

-- 					-- write precheck preamble
					log (text => "prechecking fields ...", level => log_threshold);
					log_indentation_up;
					
					-- reference
					-- NOTE: the reference prefix has been checked already in main of procedure make_component
					log (text => "reference", level => log_threshold + 1);
					if not field_reference_found then
						missing_field (NAME);
						-- CS: use missing_field (text_reference.meaning); -- apply this to other calls of missing_field too
					else
						-- If alternative references have been found, they must be looked up according to the
						-- timestamp of the current schematic (selector timestamp).
						-- Otherwise the reference must be verified aginst the content of the reference field.
						-- Reason: KiCad stores redundant information on the component reference as in this example;
							-- $Comp
							-- L 74LS00 IC1 <- reference
							-- U 1 1 59969711
							-- P 4100 4000
							-- F 0 "IC1" H 4100 4050 50  0000 C BIB <- text_reference

						if type_alternative_references.is_empty (alternative_references) then -- no alternative references
							log (text => "reference " & et_libraries.to_string (reference), level => log_threshold + 1);
							
							if et_libraries.to_string (reference) /= content (field_reference) then
								log (ERROR, "reference mismatch ! Header reads " 
									& et_libraries.to_string (reference) & " but field contains " 
									& content (field_reference),
									console => true);
								raise constraint_error;
							end if;

						else -- alternative references found
							process_alternative_references;
						end if;

						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
					end if;

					-- value
					log (text => "value", level => log_threshold + 1);
					if not field_value_found then
						missing_field (VALUE);
					else
						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_value.size);
					end if;

					-- If we are checking fields of a real component there are more 
					-- fields to be checked. If it is about a virtual component, those 
					-- fields are ignored and thus NOT checked:
					case appearance is
						when SCH_PCB =>
								
							-- package
							log (text => "package/footprint", level => log_threshold + 1);
							if not field_package_found then
								missing_field (PACKGE);
							else
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_package.size);
								-- CS: check content of field_package
								-- use library_name (content (field_package))
								-- check/validate library name (length, characters, ...)
								-- make sure the library exists. mind search order of footprint libraries

								-- check/validate package name (length, characters, ...)
								check_package_name_length (to_string (package_name (content (field_package))));
								check_package_name_characters (package_name (content (field_package)));
							end if;

							-- datasheet
							log (text => "datasheet", level => log_threshold + 1);
							if not field_datasheet_found then
								missing_field (DATASHEET);
							else
								check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_datasheet.size);
								-- CS: check content of field_datasheet
							end if;
							
						when others => null; -- CS ?
					end case;

					log_indentation_down;
					log_indentation_down;
					
					exception
						when event:
							others =>
								log (ERROR, 
									"invalid field in component " & et_libraries.to_string (reference)
									& to_string (position => position),
									console => true);
								log (text => ada.exceptions.exception_message (event), console => true);
								-- CS: evaluate prog position and provided more detailled output
								raise;

				end check_text_fields;

				function generic_name_to_library (
				-- Returns the full name of the library where given generic component is contained.
				-- The given reference serves to provide a helpful error message on the affected 
				-- component in the schematic.
					component 		: in type_component_generic_name.bounded_string; -- the generic name like "RESISTOR"
					reference 		: in type_device_name; -- the reference in the schematic like "R4"
					log_threshold	: in et_string_processing.type_log_level)
					return type_device_library_name.bounded_string is -- the full library name like "../libraries/resistors.lib"

					use type_libraries;
					use type_device_library_name;
				
					component_found : boolean := false; -- goes true once the given component was found in any library
					
					lib_cursor : type_libraries.cursor := tmp_component_libraries.first; -- points to the library being searched in
					library : type_device_library_name.bounded_string; -- the full library name to be returned

					procedure query_components (
					-- Queries the components in the current library. Exits prematurely once the 
					-- given generic component was found.
						lib_name 	: in type_device_library_name.bounded_string;
						components 	: in type_components_library.map) is
						use type_components_library;
						component_cursor : type_components_library.cursor := components.first;
						use type_component_generic_name;
					begin
						log_indentation_up;
						while component_cursor /= type_components_library.no_element loop
							
							log (text => to_string (key (component_cursor)), level => log_threshold + 2);

							-- Sometimes generic names in the library start with a tilde. it must
							-- be removed before testing the name.
							if strip_tilde (key (component_cursor)) = component then
								component_found := true;
								exit;
							end if;
							next (component_cursor);

						end loop;
						log_indentation_down;
					end query_components;
					
				begin -- generic_name_to_library
					log_indentation_up;
					log (text => "locating library containing generic component " & to_string (component) & " ...", level => log_threshold);
					
					-- loop in libraries and exit prematurely once a library with the given component was found
					while lib_cursor /= type_libraries.no_element loop
						log_indentation_up;
						log (text => "probing " 
							 & et_devices.to_string (key (lib_cursor)) 
							 & " ...", level => log_threshold + 1);

						query_element (
							position	=> lib_cursor,
							process		=> query_components'access);

						log_indentation_down;
						
						-- Exit BEFORE advancing the lib_cursor because lib_cursor position must be kept fore
						-- return value. See below. component_found MUST NOT be parameter of this loop.
						if component_found then
							exit;
						end if;
						
						next (lib_cursor);

					end loop;
					log_indentation_down;
					
					-- After a successful search return the name of the library where lib_cursor is pointing to.
					-- Otherwise send error messagen and abort.
					if component_found then
						return key (lib_cursor);
					else
						log (ERROR, "for component "  
							& et_libraries.to_string (reference)
							& " no generic model in any library found !",
							console => true);
						raise constraint_error;
					end if;
					
				end generic_name_to_library;

				function full_name_of_component_library (
				-- The given reference serves to provide a helpful error message on the affected 
				-- component in the schematic.
					component 		: in type_component_generic_name.bounded_string; -- the generic name like "RESISTOR"
					reference 		: in type_device_name; -- the reference in the schematic like "R4"
					log_threshold 	: in type_log_level) 
					return type_device_library_name.bounded_string is

					use type_lib_table;
					sym_lib_cursor : type_lib_table.cursor := sym_lib_tables.first;

					use type_libraries;
					lib_cursor : type_libraries.cursor;
					
					use type_library_name;
					
					full_name : type_device_library_name.bounded_string;
					component_found : boolean := false;

					procedure search_component (
					-- Seaches a component library for the given generic component.
						lib_name	: in type_device_library_name.bounded_string;
						lib			: in type_components_library.map) is
						use type_components_library;
					begin
						if contains (lib, component) then
							component_found := true;
						end if;
					end search_component;
					
				begin -- full_name_of_component_library
					log_indentation_up;
					log (text => "locating library '" & et_kicad_general.to_string (component_library_name) & "' containing generic component '" 
						 & to_string (component) & "' ...", level => log_threshold);

					-- Search in the sym-lib-table for the first an entry having the component_library_name (uri)
					while sym_lib_cursor /= type_lib_table.no_element loop
						if element (sym_lib_cursor).lib_name = component_library_name then
							full_name := element (sym_lib_cursor).lib_uri;

							-- locate component library by full_name
							lib_cursor := type_libraries.find (tmp_component_libraries, full_name);
							
							-- Test if library contains the given generic component.
							type_libraries.query_element (
								position	=> lib_cursor,
								process		=> search_component'access);
							
						end if;

						-- Cancel search once the given generic component has been found. Otherwise
						-- proceed with next same named library in sym-lib-table.
						if component_found then exit; end if;
						
						next (sym_lib_cursor);
					end loop;

					log_indentation_down;
					
					-- After a successful search return the name of the library where lib_cursor is pointing to.
					-- Otherwise send error messagen and abort.
					if component_found then
						return full_name;
					else
						log (ERROR, "for component "  
							& et_libraries.to_string (reference)
							& " no generic model in any library named '" & et_kicad_general.to_string (component_library_name) 
							& "' found !",
							console => true);
						raise constraint_error;
					end if;

				end full_name_of_component_library;


				function remove_leading_hash (reference : in et_libraries.type_device_name) return
				-- Removes from a reference like #PWR04 the leading hash character.
				-- CS: This function should be applied on virtual components (such as power flags or power symbols) only.
				-- The assumption is that their prefix always starts with a hash character.
					et_libraries.type_device_name is
					use et_libraries;
					use type_device_name_prefix;
					reference_out : et_libraries.type_device_name := reference; -- to be returned -- like PWR04
				begin
					--log (text => "renaming " & to_string (reference_out));
					--log (text => "length" & positive'image (length (reference_out.prefix)));
					reference_out.prefix := to_bounded_string (slice (reference_out.prefix, 2, length (reference_out.prefix)));
					--log (text => "prefix new '" & type_device_name_prefix.to_string (reference_out.prefix) & "'");
					--log (text => " to " & to_string (reference_out));
					return reference_out;
				end remove_leading_hash;
				
				procedure insert_component is
				-- Inserts the component in the component list of the module (indicated by module_cursor).
				-- Components may occur multiple times, which implies they are
				-- split into units (EAGLE refers to them as "gates").
				-- Only the first occurence of the component leads to appending it to the component list of the module.
				
				-- The component to be inserted gets assembled from the temporarily variables assigned until now.
				-- Tests if a footprint has been associated with the component.

					full_component_library_name : type_device_library_name.bounded_string;

					use et_import;
					
				begin -- insert_component

					case cad_format is
						when KICAD_V4 =>
							-- KiCad V4 does not provide an exact name of the library where the generic component
							-- model can be found. It only provides the generic name of the model.
							-- The library is determined by the order of the library names in the 
							-- project file. It is the first library in this list that contains the model.
							-- The function generic_name_to_library does the job and sets the full_component_library_name
							-- here right away:
							full_component_library_name := generic_name_to_library (
									component		=> generic_name_in_lbr, -- 7400
									reference		=> reference,			-- IC1
									log_threshold	=> log_threshold + 3);

						when KICAD_V5 =>
							-- KiCad V5 provides a simple name for the component library along with the generic 
							-- component name. From the library name we must deduce the full library name.
							full_component_library_name := full_name_of_component_library (
									component		=> generic_name_in_lbr,	-- 7400
									reference		=> reference,			-- IC1
									log_threshold	=> log_threshold + 3);

						when others => raise constraint_error;
					end case;
					
					log_indentation_up;
					
					-- The component is inserted into the components list of the module according to its appearance.
					-- If the component has already been inserted, it will not be inserted again.
					-- CS: Even if the component is not inserted again, all the operations that form its elements
					-- like power_flag, library_name, ... are executed which is a waste of computing time.
					
					case appearance is
						
						when sch => -- we have a line like "L P3V3 #PWR07"
					
							add_component (
								reference	=> remove_leading_hash (reference), -- #PWR03 becomes PWR03
								component	=> (
									appearance		=> sch,

									-- Whether the component is a "power flag" can be reasoned from its reference:
									power_flag		=> to_power_flag (reference),

									library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
									generic_name	=> generic_name_in_lbr,
									alt_references	=> alternative_references,
									
									value 			=> to_value (
														value => content (field_value),
														error_on_invalid_character => false),
										-- For the operators convenice no error is raised if invalid
										-- character found. This was the design gets imported but with
										-- (lots of) warnings.
									
									-- At this stage we do not know if and how many units there are. So the unit list is empty.
									units 			=> type_units_schematic.empty_map),
								log_threshold => log_threshold + 2);

						when sch_pcb => -- we have a line like "L 74LS00 U1"

							add_component ( 
								reference => reference,
								component => (
									appearance		=> sch_pcb,

									library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
									generic_name	=> generic_name_in_lbr,
									alt_references	=> alternative_references,
									
									value 			=> to_value (
														value => content (field_value),
														error_on_invalid_character => false),
										-- For the operators convenice no error is raised if invalid
										-- character found. This was the design gets imported but with
										-- (lots of) warnings.

									-- properties of a real component (appears in schematic and layout);
									datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),

									-- the package variant is determined by the package library and package name:
									variant			=> to_package_variant (
															component_library	=> full_component_library_name, -- ../lbr/bel_logic.lib
															generic_name		=> generic_name_in_lbr, -- 7400
															package_library		=> library_name (content (field_package)), -- bel_ic
															package_name		=> package_name (content (field_package)), -- S_SO14
															log_threshold		=> log_threshold + 2),

									-- This is layout related and will be filled on layout import later (much later):
									position			=> et_pcb_coordinates.package_position_default, -- the position of the package in the layout
									text_placeholders	=> (others => <>),  -- placeholders for reference, value, purpose in the layout
									
									-- At this stage we do not know if and how many units there are. So the unit list is empty for the moment.
									units => type_units_schematic.empty_map),

								log_threshold => log_threshold + 2);

								-- Test if footprint has been associated with the component.
								if content (field_package)'size = 0 then
									log (ERROR, "component " & to_string (reference) 
											& " footprint not specified !",
										console => true);
									raise constraint_error;
								end if;

						when others => -- CS: This should never happen. A subtype of type_device_appearance could be a solution.
							null;
							raise constraint_error;
							
					end case;

					log_indentation_down;
					
					exception
						when constraint_error =>
							log (ERROR, "component " & et_libraries.to_string (reference)
									& " " & kicad_coordinates.to_string (position => position),
								console => true);
							raise constraint_error;
					
				end insert_component;

				procedure insert_unit is 
				-- Inserts a unit into the unit list of a component. The text fields around a unit are placeholders.
				-- The properties of the placeholder texts are loaded with the properties of the text fields of the units
				-- found in the schematic. The idea behind is to store just basic text properties (type_text_basic) 
				-- for the texts around the unit, but not its content. The content is stored with the component as a kind
				-- of meta-data. See procedure insert_component.
				-- Raises constraint error if unit already in unit list of component.
				begin -- insert_unit
					log_indentation_up;
					
					case appearance is

						when SCH =>

							add_unit (
								reference	=> remove_leading_hash (reference), -- #PWR03 becomes PWR03
								unit_name	=> unit_name, -- "I/O Bank 3" or "PWR" or "A" or "B" ...	
								unit 		=> (
									appearance		=> sch,
									position		=> position,
									rotation		=> orientation,
									mirror			=> mirror,
									timestamp		=> timestamp,
									alt_repres		=> alternative_representation,

									-- placeholders:
									reference		=> (
											meaning		=> NAME,
											position	=> field_reference.position,
											style		=> field_reference.style,
											rotation	=> field_reference.rotation,
											size		=> field_reference.size,
											line_width	=> field_reference.line_width,
											alignment	=> field_reference.alignment),

									value			=> (
											meaning		=> VALUE,
											position	=> field_value.position,
											style		=> field_value.style,
											rotation	=> field_value.rotation,
											size		=> field_value.size,
											line_width	=> field_value.line_width,
											alignment	=> field_value.alignment)
											),
								
								log_threshold => log_threshold + 2);
												

						when SCH_PCB =>

							add_unit 
								(
								reference	=> reference,
								unit_name	=> unit_name, -- "I/O Bank 3" or "PWR" or "A" or "B" ...	
								unit 		=> 
									(
									appearance		=> sch_pcb,
									position		=> position,
									rotation		=> orientation,
									mirror			=> mirror,
									timestamp		=> timestamp,
									alt_repres		=> alternative_representation,

									-- The kicad placeholders are now converted to ET native placeholders:
								
									reference		=> (
											meaning		=> NAME,
											position	=> field_reference.position,
											style		=> field_reference.style,
											rotation	=> field_reference.rotation,
											size		=> field_reference.size,
											line_width	=> field_reference.line_width,
											alignment	=> field_reference.alignment),

									value			=> (
											meaning		=> VALUE,
											position	=> field_value.position,
											style		=> field_value.style,
											rotation	=> field_value.rotation,
											size		=> field_value.size,
											line_width	=> field_value.line_width,
											alignment	=> field_value.alignment)
									),
								log_threshold => log_threshold + 2
								);

						when others => null; -- CS
					end case;

					log_indentation_down;
				end insert_unit;

				procedure verify_unit_name_and_position (line : in type_fields_of_line) is
				-- Checks if the x/y position of the unit matches that provided in given line.
				-- It is about the strange repetition of the unit name and its x/y coordinates in a line like
				-- "2    6000 4000"
				begin -- verify_unit_name_and_position
					
					if et_devices.to_string (unit_name) /= et_string_processing.field (line,1) then
						log (ERROR, "invalid unit name '" & et_string_processing.field (line,1) & "'", console => true);
						raise constraint_error;
					end if;
					
					if x (position) /= mil_to_distance (et_string_processing.field (line,2)) then
	-- 					log (text => "position invalid. expected '" & to_string (position.x) 
	-- 						& "' found '" 
	-- 						& field (line,2)
	-- 						& "'");
						raise constraint_error; -- CS: write useful message
					end if;

					if y (position) /= mil_to_distance (et_string_processing.field (line,3)) then
						raise constraint_error; -- CS: write useful message
					end if;

				end verify_unit_name_and_position;


				procedure build_unit_orientation_and_mirror_style (line : in type_fields_of_line) is
				-- Builds from a line (see below) the component orientation and mirror style:

					-- Angles in Kicad are to be interpreted as: 
					-- positive angle -> counter clock wise
					-- negative angle -> clock wise

					-- The order of operations: FIRST rotate THEN mirror
					
					--  1    0    0  -1  -- orientation 0,   mirror normal
					--  0   -1   -1   0  -- orientation 90,  mirror normal
					-- -1    0    0   1  -- orientation 180, mirror normal 
					-- 	0    1    1   0  -- orientation -90, mirror normal  

					-- 	1    0    0   1  -- orientation 0,   mirror --
					--  0   -1    1   0  -- orientation 90,  mirror -- 
					-- -1    0    0  -1  -- orientation 180, mirror -- 
					--  0    1   -1   0  -- orientation -90, mirror -- 

					-- -1    0    0  -1  -- orientation 0,   mirror | 	-- not used
					--  0    1   -1   0  -- orientation 90,  mirror |	-- not used
					--  1    0    0   1  -- orientation 180, mirror |	-- not used
					--  1    0    0   1  -- orientation -90, mirror |	-- not used

					orient_1, orient_2 : type_schematic_unit_orientation;
					mirror_1, mirror_2 : type_schematic_unit_mirror_style;
				
				begin -- CS: provide useful log messages via exception handler

					-- compute unit orientation
					orient_1 := type_schematic_unit_orientation'value (et_string_processing.field (line, 1));
					orient_2 := type_schematic_unit_orientation'value (et_string_processing.field (line, 2));
					mirror_1 := type_schematic_unit_mirror_style'value (et_string_processing.field (line, 3));
					mirror_2 := type_schematic_unit_mirror_style'value (et_string_processing.field (line, 4));

					case orient_1 is
						when -1 =>
							if orient_2 = 0 then
								orientation := 180.0;

								-- compute unit mirror style
								if mirror_1 = 0 then
									case mirror_2 is
										when -1 =>
											mirror := X_AXIS;
										when  1 =>
											mirror := NO;
										when others =>
											-- invalid mirror style
											raise constraint_error;
									end case;
								else
									-- invalid mirror style
									raise constraint_error;
								end if;
								
							else
								-- invalid orientation
								raise constraint_error;
							end if;
								
						when  0 =>
							case orient_2 is
								when -1 => 
									orientation := 90.0;
									
									-- compute unit mirror style
									case mirror_1 is
										when -1 =>
											if mirror_2 = 0 then
												mirror := NO;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;

										when  0 =>
											-- invalid mirror style
											raise constraint_error;

										when  1 =>
											if mirror_2 = 0 then
												mirror := X_AXIS;
											else
												-- invaid mirror style
												raise constraint_error;
											end if;
									end case;

								when  1 =>
									orientation := -90.0;

									-- compute unit mirror style
									case mirror_1 is
										when -1 =>
											if mirror_2 = 0 then
												mirror := X_AXIS;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;

										when  0 =>
											-- invaid mirror style
											raise constraint_error;

										when  1 =>
											if mirror_2 = 0 then
												mirror := NO;
											else
												-- invalid mirror style
												raise constraint_error;
											end if;
									end case;

								when others => 
									-- invalid orientation
									raise constraint_error;
							end case;

						when  1 =>
							if orient_2 = 0 then
								orientation := 0.0;

								-- compute unit mirror style
								if mirror_1 = 0 then
									case mirror_2 is
										when -1 =>
											mirror := NO;
										when  1 =>
											mirror := X_AXIS;
										when others =>
											-- invalid mirror style
											raise constraint_error;
									end case;
								else
									-- invalid mirror style
									raise constraint_error;
								end if;
								
							else
								-- invalid orientation
								raise constraint_error;
							end if;
					end case;

				end build_unit_orientation_and_mirror_style;


				procedure add_alternative_reference (line : in type_fields_of_line) is
				-- Adds the alternative reference given in a line like 
				-- AR Path="/5B7E59F3/5B7E5817" Ref="#PWR03"  Part="1" 
				-- to the list alternative_references.
				
					path	: et_string_processing.type_fields_of_line; -- 59F17F77 5A991798
					ref		: et_libraries.type_device_name; -- #PWR03
					unit	: type_unit_name.bounded_string; -- 1 -- CS is this really about unit names ?

					path_segment : type_timestamp;
					alt_ref_path : type_alternative_reference_path.list;
				begin
					log (text => "alternative reference " & et_string_processing.to_string (line), level => log_threshold + 3); -- Path="/59F17F77/5A991798
					--log (text => field (line, 2) (8 .. field (line, 2)'last), level => log_threshold + 1);
					
					-- extract the path segments from field 2: example: Path="/59F17F77/5A991798					
					path := et_string_processing.read_line (
						line			=> trim (et_string_processing.field (line, 2) (8 .. et_string_processing.field (line, 2)'last), both), -- 59F17F77/5A991798
						-- NOTE: the trailing double quote is already gone.
						
						comment_mark	=> "", -- no comment marks
						ifs				=> hierarchy_separator (1)); -- hierarchy_separator is a string

					--log (text => et_string_processing.to_string (path), level => log_threshold + 1);
					
					-- Transfer the path segments to alt_ref_path.
					-- "path" contains a list of strings.
					-- alt_ref_path is a list of timestamps
					for place in 1 .. positive (et_string_processing.field_count (path)) loop

						-- convert the segment from string to timestamp
						path_segment := type_timestamp (et_string_processing.field (path, place));

						-- append the segment
						type_alternative_reference_path.append (	
							container	=> alt_ref_path,
							new_item	=> path_segment);
					end loop;

-- 					log (text => "new reference '" & field (line, 3) (6.. (field (line, 3)'last)) & "'", level => log_threshold + 1);  -- #PWR03
					
					-- extract the reference from field 3: example: Ref="#PWR03
					-- NOTE: the trailing double quote is already gone.
					ref := to_component_reference (
							text_in 		=> et_string_processing.field (line, 3) (6.. (et_string_processing.field (line, 3)'last)),  -- #PWR03
							leading_hash	=> true);

-- 					log (text => "test", level => log_threshold + 1);
-- 					log (text => "new reference " & et_libraries.to_string (ref), level => log_threshold + 1);  -- #PWR03
					
					-- extract the part name (CS unit name ?) from field 4: example Part="1
					-- NOTE: the trailing double quote is already gone.
					unit := to_unit_name (et_string_processing.field (line, 4) (7 .. (et_string_processing.field (line, 4)'last)));

					-- Now all components of the alternative reference are ready.
					-- Append the new alternative reference to list alternative_references:
					type_alternative_references.append (
						container	=> alternative_references,
						new_item	=> (
										path		=> alt_ref_path,
										reference	=> ref,
										part		=> unit
									   ));
					
				end add_alternative_reference;
				
				use type_lines;

				function generic_name (text : in string) return type_component_generic_name.bounded_string is
				-- Extracts from a given string like "bel_logic:7400" the generic component name "7400".
					ifs : constant string (1..1) := ":";

					-- The separator must NOT be at first position in text.
					-- CS: Text is limited to 200 characters which seems sufficient.
					subtype type_pos is positive range 2 .. 200;

					pos : type_pos := index (text, ifs); -- get position of ifs
				begin -- generic_name
					return type_component_generic_name.to_bounded_string (text (pos + 1 .. text'last)); -- 7400
				end generic_name;

				function extract_library_name (text : in string) return type_library_name.bounded_string is
				-- Extracts from a given string like "bel_logic:7400" the library name "bel_logic".
					ifs : constant string (1..1) := ":";

					-- The separator must NOT be at first position in text.
					-- CS: Text is limited to 200 characters which seems sufficient.
					subtype type_pos is positive range 2 .. 200;

					pos : type_pos := index (text, ifs); -- get position of ifs
				begin -- extract_library_name
					return type_library_name.to_bounded_string (text (text'first .. pos - 1)); -- bel_logic
				end extract_library_name;
				
			begin -- make_component (schematic)
				log (text => "making component ...", level => log_threshold);
				log_indentation_up;

				-- loop in lines provided by "lines"
				line_cursor := type_lines.first (lines);
				while line_cursor /= type_lines.no_element loop

					log (text => "component line: " & to_string (et_kicad.line), level => log_threshold + 6);

					-- V4: 
					--	- Read component generic name and annotation from a line like "L NetChanger N1".
					-- V5:
					--	- Read library name, component generic name and annotation from a line like "L bel_logic:7400 IC1". 
					
					-- From this entry we reason the component appearance. 
					-- The appearance is important for contextual validation of the fields.
					-- It is also required for validation of the reference (like R12 or C4).
					if et_string_processing.field (et_kicad.line,1) = schematic_component_identifier_name then -- "L"

						case et_import.cad_format is
							when et_import.KICAD_V4 =>
								generic_name_in_lbr := type_component_generic_name.to_bounded_string (
														et_string_processing.field (et_kicad.line,2)); -- "SN74LS00"

							when et_import.KICAD_V5 =>
								generic_name_in_lbr := generic_name (et_string_processing.field (et_kicad.line,2)); -- "bel_logic:SN74LS00"
								component_library_name := extract_library_name (et_string_processing.field (et_kicad.line,2)); -- "bel_logic:SN74LS00"

							when others => raise constraint_error;
						end case;
								
						log (text => "generic name " & to_string (generic_name_in_lbr), level => log_threshold + 1);
						
						check_generic_name_characters (
							name => generic_name_in_lbr, -- "SN74LS00"
							-- NOTE: We do not allow tilde characters here. they occur ONLY in the library:
							characters => component_generic_name_characters); 

						appearance := to_appearance (line => et_kicad.line, schematic => true);
						log (text => to_string (appearance, verbose => true), level => log_threshold + 3);

						-- Depending on the appearance of the component the reference is built and checked.
						-- IMPORTANT: The reference is preliminary. Due to possible hierarchic design, it
						-- might be overwritten once alternative references are found in this sheet.
						case appearance is
						
							when SCH => 
								-- We have a line like "L P3V3 #PWR07".
								-- Build a reference type from the given reference string.
								-- Afterward we validate the prefix of the reference. It must be
								-- a power symbol or a power flag (#PWR or #FLG).
								reference := to_component_reference (
									text_in			=> et_string_processing.field (et_kicad.line,3),
									leading_hash	=> true); 

								log (text => "reference " & to_string (reference) & " (preliminary)", level => log_threshold);
								validate_prefix (reference);

							when SCH_PCB =>
								-- we have a line like "L 74LS00 IC13"
								-- -- Build a reference type from the given reference string.
								-- Afterward we validate the prefix of the reference. 
								-- It is about a REAL component. Its prefix must be one 
								-- of those defined in the configuration file (see conventions).
								reference := to_component_reference ( -- character check included
									text_in			=> et_string_processing.field (et_kicad.line,3),
									leading_hash	=> false);

								log (text => "reference " & to_string (reference) & " (preliminary)", level => log_threshold);
								
							when others => -- CS: This should never happen. A subtype of type_device_appearance could be a solution.
								raise constraint_error;
								
						end case;
									
						-- CS: check proper annotation

					-- read line like "U 2 1 4543D4D3F" 
					-- U is the line indicator, 2 is the unit id, 1 is the demorgan flag.
					-- Last field is the link to the package in the board file.
					elsif et_string_processing.field (et_kicad.line,1) = schematic_component_identifier_unit then -- "U"

						-- KiCad uses positive numbers to identifiy units. But in general a unit name can
						-- be a string as well. Therefore we handle the unit id as string.
						unit_name := type_unit_name.to_bounded_string ( -- CS: check_unit_name_characters
							et_string_processing.field (et_kicad.line,2)); -- the unit id

						-- Read DeMorgan flag:
						alternative_representation := to_alternative_representation (line => et_kicad.line, schematic => true);

						-- Read and check the link to the board file:
						timestamp := type_timestamp (et_string_processing.field (et_kicad.line,4));

					-- Read unit coordinates from a line like "P 3200 4500".
					elsif et_string_processing.field (et_kicad.line,1) = schematic_component_identifier_coord then -- "P"
					
						--set_x (position, mil_to_distance (et_string_processing.field (et_kicad.line,2))); -- "3200"
						set (X, mil_to_distance (et_string_processing.field (et_kicad.line,2)), position); -- "3200"
						--set_y (position, mil_to_distance (et_string_processing.field (et_kicad.line,3))); -- "4500"
						set (Y, mil_to_distance (et_string_processing.field (et_kicad.line,3)), position); -- "4500"

						-- The unit coordinates is more than just x/y :
						set_path (position, path_to_sheet);
						set_sheet (position, sheet_number);

					-- Read alternative reference like "AR Path="/59EF082F" Ref="N23"  Part="1"
					elsif et_string_processing.field (et_kicad.line,1) = schematic_component_identifier_path then -- "AR"
						add_alternative_reference (et_kicad.line);

					-- read unit fields 0..2 from lines like:
					-- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
					--			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
					--			"F 2 "bel_netchanger:0.2MM" H 2600 2100 60  0001 C CNN"

					-- Set "field found flags" accordingly.
					-- Do some basic checks on the fields.
					elsif et_string_processing.field (et_kicad.line,1) = component_field_identifier then -- "F"

						--log (text => "unit field A: " & to_string (et_kicad.line));
						
						case type_component_field_id'value (et_string_processing.field (et_kicad.line,2)) is
							
							when component_field_reference =>
								field_reference_found := true;
								field_reference := to_field;
								-- NOTE: This is a redundant field. Its content must match the reference (see above).
								-- This test is performed in procedure check_text_fields.
								
							when component_field_value =>
								field_value_found := true;
								field_value := to_field;

								declare
									value : et_devices.type_value.bounded_string;
								begin
									value := et_devices.to_value (
											value 						=> content (field_value),
											error_on_invalid_character	=> false);
									-- For the operators convenice no error is raised if invalid
									-- character found. This was the design gets imported but with
									-- (lots of) warnings.
								end;

								
							when component_field_package =>
								field_package_found := true;
								field_package := to_field;
								check_package_name_length (content (field_package));
								check_package_name_characters (
									packge => type_component_package_name.to_bounded_string (content (field_package)),
									characters => et_kicad.component_package_name_characters);
								
							when component_field_datasheet =>
								field_datasheet_found := true;
								field_datasheet := to_field;
								check_datasheet_length (content (field_datasheet));
								check_datasheet_characters (
									datasheet => type_component_datasheet.to_bounded_string (content (field_datasheet)));
								
							when others => null; -- ignore other fields
						end case;

						--log (text => "unit field B: " & to_string (et_kicad.line));
						
					else
						-- What is left is a strange repetition of the unit name and its x/y coordinates in a line like
						-- "2    6000 4000"
						-- followed by the unit mirror style and the unit orientation in a line like
						-- "1    0    0    -1"

						case field_count (et_kicad.line) is
							when 3 => -- we have the unit name and its x/y position.
								-- We verify if unit name and position match the values read earlier:
								verify_unit_name_and_position (et_kicad.line);
							
							when 4 => null; -- we have the unit mirror style and orientation
								build_unit_orientation_and_mirror_style (et_kicad.line);
							
							when others => 
								raise constraint_error; -- CS: write useful message
						end case;

					end if;


					next (line_cursor);
				end loop;

				-- Check whether all required text fields have been found.
				-- Check content of text fields for syntax and plausibility.
				check_text_fields (log_threshold + 1);
				
				-- Insert component in component list of module. If a component is split
				-- in units, only the first occurence of it leads to inserting the component.
				-- Nevertheless there are some checks on the unit (see insert_component).
				insert_component;

				-- We update the component with the collected unit information.
				insert_unit;

				log_indentation_down;
				
				exception
					when event:
						others =>
							if line_cursor /= type_lines.no_element then
								error_in_schematic_file (et_kicad.line);
							end if;
								
							--log (text => ada.exceptions.exception_message (event), console => true);
							raise;
				
			end make_component;

			function no_connection_header (line : in type_fields_of_line) return boolean is
			-- Returns true if given line is a no-connection-flag "NoConn ~ 5000 3900"
				result : boolean := false;
			begin
				-- CS test field count
				if et_string_processing.field (line,1) = schematic_keyword_no_connection then
					if et_string_processing.field (line,2) = schematic_tilde then
						result := true;
					end if;
				end if;
				return result;
			end no_connection_header;
			
			procedure make_no_connection (line : in type_fields_of_line) is
			-- Builds a no-connect flag and stores it a wild list of no-connection-flags
			-- A line that specifies such a flag loops like "NoConn ~ 5000 3900"
				no_connection_flag : type_no_connection_flag;

				use type_modules;
			
				procedure append_no_connect_flag (
					module_name	: in type_submodule_name.bounded_string;
					module		: in out type_module) is
					use type_no_connection_flags;
				begin
					append (
						container => module.no_connections,
						new_item => no_connection_flag);
				end append_no_connect_flag;				
			
			begin -- make_no_connection
				set_path (no_connection_flag.coordinates, path_to_sheet);
				set_sheet (no_connection_flag.coordinates, sheet_number);
				
				--set_x (no_connection_flag.coordinates, mil_to_distance (et_string_processing.field (line,3)));
				set (X, mil_to_distance (et_string_processing.field (line,3)), no_connection_flag.coordinates);
				--set_y (no_connection_flag.coordinates, mil_to_distance (et_string_processing.field (line,4)));
				set (Y, mil_to_distance (et_string_processing.field (line,4)), no_connection_flag.coordinates);
				
				-- for the log
				log (text => "no-connection-flag" & to_string (no_connection_flag => no_connection_flag, scope => xy),
					 level => log_threshold + 1);

				-- append the no-connect-flag to the list of no_connections of the current module
				update_element (
					container	=> modules,
					position	=> module_cursor,
					process		=> append_no_connect_flag'access);
				
			end make_no_connection;
			
		begin -- read_schematic
			log_indentation_reset;
			log_indentation_up;
		
			if exists (to_string (current_schematic.sheet.file)) then
				log (text => "reading schematic file " & to_string (current_schematic.sheet.file) &
					" sheet name " & to_string (current_schematic.sheet.name) &
					" with timestamp " & string (current_schematic.timestamp) & " ...",
					 level => log_threshold,
					 console => true);

				-- log module path as recorded by parent unit
				log_indentation_up;
				log (text => "path " & to_string (path_to_sheet), level => log_threshold);
				
				open (file => schematic_handle, mode => in_file, name => to_string (current_schematic.sheet.file));
				set_input (schematic_handle);

				-- read schematic file line per line
				while not end_of_file loop

					-- Store line in variable "line" (see et_string_processing.ads)
					line := et_string_processing.read_line (
								line 			=> get_line,
								number 			=> ada.text_io.line (current_input),
								comment_mark 	=> "", -- there are no comment marks in the schematic file
								delimiter_wrap 	=> true, -- there are fields wrapped in delimiters
								ifs 			=> latin_1.space); -- fields are separated by space
					-- CS: If read_line exits with an exception, the exception handler of read_schematic
					-- outputs the line BEFORE the faulty line. Thus misleading the operator.
					
					case field_count (line) is
						when 0 => null; -- we skip empty lines
						when others =>

							-- At a certain log level we report the whole line as it is:
 							log (text => "line ->" & to_string (line) & "<-", level => log_threshold + 7);

							-- The first line should be the headline with the schematic version:
							-- READ SCHEMATIC HEADLINE:

							-- EESchema Schematic File Version x
							
							if not schematic_version_valid then
								check_header (line); 
								-- sets schematic_version_valid true.
								-- aborts program if version not supported
							end if;

							-- READ SHEET HEADER:

							--	EESchema Schematic File Version 2
							--	LIBS:nucleo_core-rescue
							--	LIBS:power
							--	LIBS:bel_connectors_and_jumpers
							--	LIBS:bel_primitives
							--	LIBS:bel_stm32
							--	LIBS:nucleo_core-cache
							--	EELAYER 25 0
							--	EELAYER END

							if not sheet_header_entered then
								if get_field_from_line (et_string_processing.field (line,1), 1, latin_1.colon) = schematic_library then
									sheet_header_entered := true;
									add (line);
								end if;
							else -- we are inside the sheet header and wait for the footer
								if field_count (line) = 2 then
									if et_string_processing.field (line,1) = schematic_eelayer 
										and et_string_processing.field (line,2) = schematic_eelayer_end then
											sheet_header_entered := false;
											add (line);
											make_sheet_header (lines);
											clear (lines); -- clean up line collector
									else
										add (line);
									end if;
								end if;
							end if;
							
							-- READ DESCRIPTION:

							-- $Descr A4 11693 8268
							-- encoding utf-8
							-- Sheet 5 8
							-- Title ""
							-- Date ""
							-- Rev ""
							-- Comp ""
							-- Comment1 ""
							-- Comment2 ""
							-- Comment3 ""
							-- Comment4 ""
							-- $EndDescr
							
							if not description_entered then
								if et_string_processing.field (line,1) = schematic_description_header then -- $Descr A4 11693 8268
									description_entered := true; -- we are entering the sheet description

									add (line);
								end if;
							else -- we are inside the description
								if et_string_processing.field (line,1) = schematic_description_footer then -- $EndDescr
									description_entered := false; -- we are leaving the description
									description_processed := true;

									make_drawing_frame (lines, log_threshold + 1);
									clear (lines); -- clean up line collector
								else
									add (line);
								end if;
							end if;

							-- Read hierarchical GUI sheets (if there has been a total sheet count greater 1 detected earlier).
							-- A hierachical GUI sheet displays a hierarchical sheet as a black box with its ports.
							-- It serves as link between a hierachical net and the parent module.
							-- Rightly said this is the black box representation of a submodule. 
							-- So in the following we refer to them as "submodule".
							-- A submodule (sheet) section example:
							
							-- $Sheet
							-- S 4050 5750 1050 650 
							-- U 58A73B5D
							-- F0 "Sheet58A73B5C" 58
							-- F1 "morpho_test.sch" 58
							-- F2 "SENSOR_GND" U R 2250 3100 60 
							-- F3 "SENSOR_VCC" I L 1350 3250 60 
							-- $EndSheet

							if sheet_count_total > 1 then -- read hierarchical GUI sheets
								if not sheet_description_entered then
									if et_string_processing.field (line,1) = schematic_sheet_header then -- $Sheet
										sheet_description_entered := true;
									end if;
								else -- we are inside a sheet description
									if et_string_processing.field (line,1) = schematic_sheet_footer then -- $EndSheet
										sheet_description_entered := false; -- we are leaving the sheet description

										make_gui_sheet (lines, log_threshold + 1);
										clear (lines);
									else
										add (line);
									end if;
								end if;
							end if;

							-- Further parts of the file can be read IF the description has been processed before (see above)
							if description_processed then
								
								-- READ NET SEGMENTS

								-- Wire Wire Line
								-- 2250 3100 2400 3100 

								if not net_segment_entered then
									if net_segment_header (line) then
										net_segment_entered := true;
									end if;
								else
									net_segment_entered := false; -- we are leaving a net segment
									add (line);
									make_net_segment (lines, log_threshold + 1);
									clear (lines);
								end if;

								-- READ NET JUNCTIONS 
								
								-- Connection ~ 4650 4600

								if junction_header (line) then
									make_junction (line, log_threshold + 1);
								end if;
									
								-- READ SIMPLE NET LABELS (they do not have a tag, but just a text) 

								-- Text Label 2350 3250 0 60 ~ 0
								-- TOP_VCC

								if not simple_label_entered then
									if simple_label_header (line) then
										simple_label_entered := true;
										add (line);
									end if;
								else
									simple_label_entered := false; -- we are leaving a simple label
									add (line);
									make_simple_label (lines, log_threshold + 1);
									clear (lines);
								end if;
								
								-- READ TAG NET-LABELS (global or hierarchical)

								-- Text GLabel 4700 3200 1    60   UnSpc ~ 0
								-- DRV_1

								if not tag_label_entered then
									if tag_label_header (line) then
										tag_label_entered := true;
										add (line);
									end if;
								else
									tag_label_entered := false; -- we are leaving a tag label
									add (line);
									make_tag_label (lines, log_threshold + 1);
									clear (lines);
								end if;

								-- READ NOTES 

								-- "Text Notes 3400 2800 0 60 Italic 12"
								-- "ERC32 Test Board"
								
								if not note_entered then
									if text_note_header (line) then
										note_entered := true; -- we are entering a note
										add (line);
									end if;
								else 
									note_entered := false; -- we are leaving a note
									add (line);
									make_text_note (lines, log_threshold + 1);
									clear (lines);
								end if;
								
								-- READ COMPONENTS
								-- Once a component header ($Comp) found, set component_entered flag. This indicates we are inside a component section.
								-- Inside the component section, we process its content until the component footer ($EndComp) is found.
								-- Some entries of the component section are relevant for the whole component. Some entries are unit specific.
								-- The component section looks like this example:
								
								-- $Comp
								-- L 74LS00 U1		-- component specific
								-- U 4 1 5965E676	-- unit specific
								-- P 4100 4000		-- unit specific
								-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
								-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
								-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
								-- F 3 "" H 4100 4000 50  0001 C CNN
								-- 	4    4100 4000		-- CS: same as x/y pos ?

								--  1    0    0  -1  -- orientation 0,   mirror normal
								--  0   -1   -1   0  -- orientation 90,  mirror normal
								-- -1    0    0   1  -- orientation 180, mirror normal 
								-- 	0    1    1   0  -- orientation -90, mirror normal  

								-- 	1    0    0   1  -- orientation 0,   mirror --
								--  0   -1    1   0  -- orientation 90,  mirror -- 
								-- -1    0    0  -1  -- orientation 180, mirror -- 
								--  0    1   -1   0  -- orientation -90, mirror -- 

								-- -1    0    0  -1  -- orientation 0,   mirror |
								--  0    1   -1   0  -- orientation 90,  mirror |
								--  1    0    0   1  -- orientation 180, mirror |
								--  1    0    0   1  -- orientation -90, mirror |
								-- $EndComp

								if not component_entered then
									if component_header (line) then
										component_entered := true;
									end if;
								else -- we are inside the component and wait for the component footer ($EndComp)
									if component_footer (line) then
										component_entered := false; -- we are leaving the component

										make_component (lines, log_threshold + 1);
										clear (lines);
									else -- read lines of unit/component
										add (line);
									end if;
								
								end if;

								-- READ NO-CONNECT-FLAGS

								-- NoConn ~ 5000 3900
								
								if no_connection_header (line) then
									make_no_connection (line);
								end if;
							end if;

					end case;

				end loop;

				close (schematic_handle);
				log_indentation_down;
				--log (text => "reading complete. closing schematic file " &
				--	 to_string (current_schematic.sheet.file) & " ...", log_threshold);

				-- From the wild list of net segments, assemble net segments to anonymous strands.
				-- A strand is: all net segments connected with each other by their start or end points.
				build_anonymous_strands (log_threshold + 1);
	
				-- All anonymous strands must be given a name. The name is enforced by the a net label.
				-- (The fact that power-put ports enforce a name also, is cared for later on netlist generation.)
				-- The first label found on the strand sets the strand name and scope. 
				-- Other labels on the strand are checked for their name only. 
				-- If the name differs from the net name set earlier, a warning is output. 
				-- Strands without label remain anonymous. Their name is assigned by using the notation "N$".
				-- The strands are finally appended to the strands of the current module (see spec. of type_module.strands).
				associate_net_labels_with_anonymous_strands (log_threshold + 1);

			else
				log (ERROR, "schematic file '" & to_string (current_schematic.sheet.file) & "' not found !",
					console => true);
				raise constraint_error;
			end if;

			return hierarchic_sheet_file_names;

			exception
				when others =>
					error_in_schematic_file (line);
					raise;					

		end read_schematic;

		module_name : type_submodule_name.bounded_string; -- the name of the module to be created
		module_inserted : boolean := false; -- goes true if module already created. should never happen


		procedure save_libraries is
			use et_import;

			procedure save_components (
			-- Saves the current tmp_component_libraries in the current module.
				module_name	: in type_submodule_name.bounded_string;
				module		: in out type_module) is
			begin
				module.component_libraries := tmp_component_libraries;
			end save_components;

			procedure save_packages (
			-- Saves the package_libraries in the current module.
				module_name	: in type_submodule_name.bounded_string;
				module		: in out type_module) is
			begin
				module.footprints := et_kicad_pcb.package_libraries;
			end save_packages;
			
		begin -- save_libraries
			-- tmp_component_libraries is a tempoarily storage place.
			-- It must be saved in module.component_libraries.
			-- tmp_component_libraries is furhter-on requried for other operations (like read_schematic) within the current module.
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
-- 				et_coordinates.Y_axis_positive := et_coordinates.downwards;
				
				-- Derive top level schematic file name from project name.
				-- Clears tmp_component_libraries (which is a temparily storage).
				-- Creates empty component/symbol libraries in tmp_component_libraries.
				-- For V4:	This action creates new directory and component library search lists
				-- 			in search_list_component_libraries and search_list_project_lib_dirs.
				-- For V5:	Reads sym-lib-tables and stores them in sym_lib_tables.
				--			Creates empty package libraries in et_kicad_pcb.package_libraries.
				top_level_schematic	:= read_project_file (log_threshold + 1);
				
				module_name := to_submodule_name (
					base_name (kicad_coordinates.to_string (top_level_schematic)));

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
						component_libraries => type_libraries.empty_map,

						-- V5
						-- package/footprint libraries
						footprints			=> et_kicad_pcb.type_libraries.empty_map,
						
						strands				=> type_strands.empty_list,
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
				et_kicad_pcb.read_libraries (log_threshold); -- fills et_kicad_pcb.package_libraries
				
				-- read component libraries
				read_components_libraries (log_threshold); -- fills tmp_component_libraries

				-- copy temparily containers in module
				save_libraries;

				current_schematic.sheet.file := top_level_schematic;
				check_submodule_name_characters (to_submodule_name (current_schematic.sheet.file));
				
                -- The top level schematic file is the first entry in the module path.
				-- The top level schematic file is the root in the module path.
				-- Starting from the top level module, we read its schematic file. The result can be a list 
				-- of sheets which means that the design is hierarchic.
				
				-- The function read_schematic requires the name of the current schematic,
				-- It returns a list of hierachic sheets.
				hierarchic_sheet_file_names := read_schematic (current_schematic, log_threshold);

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
							hierarchic_sheet_file_names := read_schematic (current_schematic, log_threshold);

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
				et_kicad_pcb.read_boards (log_threshold);
				log_indentation_down;

			when others =>
				null; -- CS: add import of other CAD kicad formats (v6, v7, ..) here

				
		end case;

-- 		exception
-- 			-- CS: log exception message
-- 			when event:
-- 				constraint_error =>
-- 					log (ERROR, "in schematic file '" 
-- 						& to_string (current_schematic) & "' " 
-- 						console => true);
-- 						et_import.close_report;
-- 						put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
-- 					raise;
		
	end import_design;
	
	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment_base'class) 
		return boolean is

		sits_on_segment : boolean := false;

		use et_coordinates;
		use geometry;
		use et_schematic.shapes;
		d : type_distance_point_line;

		-- CS this is a workaround in order to provide a line for function distance_point_line:
		type type_line_scratch is new et_schematic.shapes.type_line with null record;
		line : type_line_scratch := (
			start_point	=> geometry.type_point (segment.coordinates_start), 
			end_point	=> geometry.type_point (segment.coordinates_end));
		
	begin
		-- calculate the shortes distance of point from line.
		d := distance_point_line (
			point 		=> type_point (junction.coordinates),
			line		=> line,
			line_range	=> BETWEEN_END_POINTS);

		if (not d.out_of_range) and d.distance = zero then
			sits_on_segment := true;
		end if;

		return sits_on_segment;
	end junction_sits_on_segment;

	
	function component_power_flag (cursor : in type_components_library.cursor)
	-- Returns the component power flag status.
		return type_power_flag is
		use et_string_processing;
	begin
		-- Only vitual components have the power flag property. 
		-- For real components the return is always false;
-- 		if et_libraries."=" (type_components_library.element (cursor).appearance, et_libraries.SCH) then
		if type_components_library.element (cursor).appearance = SCH then
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
-- 		reference		: in et_libraries.type_device_name; -- X701
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
		et_kicad.modules.update_element (
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
			name 	: in et_libraries.type_device_name;
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
			module		: in out type_module) is

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
					log (text => to_string (position => strand.position, scope => kicad_coordinates.MODULE),
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

		use et_coordinates.geometry;
		use et_schematic.shapes;
		distance : type_distance_point_line;

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
					& to_string (port.coordinates, kicad_coordinates.MODULE),
						console => true);
				raise constraint_error;
			end if;
		end test_junction;

		use et_coordinates;

		-- CS this is a workaround in order to provide a line for function distance_point_line:
		type type_line_scratch is new et_schematic.shapes.type_line with null record;
		line : type_line_scratch := (
			start_point	=> geometry.type_point (segment.coordinates_start), 
			end_point	=> geometry.type_point (segment.coordinates_end));
		
	begin -- port_connected_with_segment
		-- First make sure the port is to be connected at all. Ports intended to be open
		-- are regarded as "not connected with the segment".
		--if not port.intended_open then
		if not (port.intended_open) then
	
			-- Make sure port and segment share the same module path and sheet.
			-- It is sufficient to check against the segment start coordinates.
			if same_path_and_sheet (port.coordinates, segment.coordinates_start) then

				-- calculate the shortes distance of point from line.
				distance := distance_point_line (
					point 		=> type_point (port.coordinates),
					line		=> line,
					line_range	=> with_end_points);

				if (not distance.out_of_range) and distance.distance = zero then

					-- If point sits on either start or end point of given line
					if distance.sits_on_start or distance.sits_on_end then

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

		use et_coordinates;
		use et_libraries;
		
		strand		: type_strands.cursor := first_strand;
		segment		: type_net_segments.cursor;
		component	: type_portlists.cursor;
		port		: type_ports.cursor;
		
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
					log (text => "probing component " & et_libraries.to_string (key (component)), level => log_threshold + 4);

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
									log (text => "component " & et_libraries.to_string (key (component)) 
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
									log (WARNING, "component " & et_libraries.to_string (key (component)) 
										& " POWER IN port " & to_string (element (port).name) 
										& " at" & to_string (element (port).coordinates, module)
										& " conflicts with net " & et_general.to_string (element (strand).name) & " !");
									--raise constraint_error;

								-- If strand has a name and is local or hierarchic -> error and abort
								--elsif et_schematic."/=" (element (strand).scope, et_schematic.global) then
								elsif element (strand).scope /= GLOBAL then
									log (ERROR, "component " & et_libraries.to_string (key (component)) 
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
			use et_coordinates.geometry;
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

		lib_cursor	: type_libraries.cursor;
		use type_components_library;
		comp_cursor	: type_components_library.cursor := no_element;
	
		use type_libraries;
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
		if lib_cursor /= type_libraries.no_element then
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
	
		use et_libraries;
		use type_full_library_names;
		use et_string_processing;

		-- This component cursor points to the schematic component being processed.
		use type_components_schematic;
		component_cursor_sch: type_components_schematic.cursor;

		-- The component reference in the schematic (like R44 or IC34)
		-- is tempoarily held here:
		component_reference	: et_libraries.type_device_name;
	
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
		
			-- The unit cursor of the component advances through the units stored in the library.
			unit_cursor : type_units_library.cursor;

			-- The port cursor of the unit indicates the port of a unit.
			port_cursor : type_ports_library.cursor; 

			unit_name_lib : type_unit_name.bounded_string; -- the unit name in the library. like "A", "B" or "PWR"
			unit_position : kicad_coordinates.type_position; -- the coordinates of the current unit
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
					
					port_coordinates : kicad_coordinates.type_position;
					use et_coordinates.geometry;

					function left_open return type_port_open is
					-- Returns true if a no-connect-flag sits at the port_coordinates.

						port_open : type_port_open := false;
					
						procedure query_no_connect_flags (
							module_name	: in type_submodule_name.bounded_string;
							module 		: in type_module) is
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
					rotate (
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
					move (point => port_coordinates, offset => unit_position);

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
						when sch =>
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

						when sch_pcb =>
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

		procedure check_appearance_sch_vs_lib is
		-- Verifies appearance of schematic component against library component.
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
			component_reference := et_kicad.component_reference (component_cursor_sch);
			log (text => "reference " & et_libraries.to_string (component_reference), level => log_threshold + 1);
			
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
					& to_string (element (component_cursor_sch).generic_name) 
					& " in " & et_devices.to_string (element (component_cursor_sch).library_name),
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
	
		use type_modules;
		use et_string_processing;

		procedure query_portlists (
			module_name	: in type_submodule_name.bounded_string;
			module 		: in type_module) is
			use type_portlists;
			portlist_cursor : type_portlists.cursor := module.portlists.first;

			procedure query_ports (
				component	: in et_libraries.type_device_name;
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

				use et_coordinates;
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
										& to_string (element (port_cursor).coordinates, kicad_coordinates.MODULE)
										& " nor via other units of this component !");
									raise constraint_error;
								end if;
							else
								log (WARNING, "port not connected at" 
									& to_string (element (port_cursor).coordinates, kicad_coordinates.MODULE));
							end if;

						else
							log (WARNING, "port not connected at" 
								& to_string (element (port_cursor).coordinates, kicad_coordinates.MODULE));
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
			library_cursor : type_libraries.cursor;

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
						component_name	: in et_libraries.type_device_name;
						component 		: in type_component_schematic) is
						use type_units_schematic;
						unit_cursor : type_units_schematic.cursor := component.units.first;
						unit_deployed : boolean := false;
						use et_devices.type_unit_name;
						use et_import;

						function unit_not_deployed return string is
						begin
							return et_libraries.to_string (key (component_sch)) 
								& " unit " & to_string (key (unit))
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

				log (text => et_libraries.to_string (key (component_sch)) & " in " 
					& et_devices.to_string (element (component_sch).library_name), level => log_threshold + 1);

				-- Set library cursor so that it points to the library of the generic model.
				library_cursor := type_libraries.find (
					container	=> tmp_component_libraries, -- the collection of project libraries with generic models
					key 		=> element (component_sch).library_name); -- lib name provided by schematic component
				
				-- Query the library components.
				type_libraries.query_element (
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
		use et_coordinates;
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
		reference		: in et_libraries.type_device_name;
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
		reference		: in et_libraries.type_device_name;
		unit_name		: in et_devices.type_unit_name.bounded_string;
		unit 			: in type_unit_schematic;
		log_threshold	: in et_string_processing.type_log_level) is

		procedure add (
			reference	: in et_libraries.type_device_name;
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
				log (ERROR, "component " & et_libraries.to_string (reference) &
					 " unit " & et_devices.to_string (unit_name) & " used multiple times !" &
					 " Make sure " & et_libraries.to_string (reference) & " exists ONLY ONCE !",
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
					position : kicad_coordinates.type_position;
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
						use et_coordinates;
						use geometry;
						use et_schematic.shapes;
						distance : type_distance_point_line;
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
									type type_line_scratch is new et_schematic.shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> geometry.type_point (element (segment_cursor_sec).coordinates_start), 
										end_point	=> geometry.type_point (element (segment_cursor_sec).coordinates_end));
								begin
									-- If START point of primary segment sits BETWEEN start and end point of secondary segment,
									-- exit prematurely and return the coordinates of the expected junction.
									distance := distance_point_line (
										point 		=> type_point (element (segment_cursor_prim).coordinates_start),
										line		=> line,
										line_range	=> BETWEEN_END_POINTS);
								
									if (not distance.out_of_range) and distance.distance = zero then
										junction_position.expected := true;
										junction_position.position := element (segment_cursor_prim).coordinates_start;
										exit;
									end if;

									-- If END point of primary segment sits BETWEEN start and end point of secondary segment,
									-- exit prematurely and return the coordinates of the expected junction.

									distance := distance_point_line (
										point 		=> type_point (element (segment_cursor_prim).coordinates_end),
										line		=> line,
										line_range	=> BETWEEN_END_POINTS);
								
									if (not distance.out_of_range) and distance.distance = zero then
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
							& to_string (element (strand_cursor_sec).position, scope => kicad_coordinates.MODULE),
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
						use et_coordinates;
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

				use et_coordinates;
				
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
							 & to_string (junction.position, kicad_coordinates.MODULE));
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
					& to_string (element (strand_cursor_prim).position, scope => kicad_coordinates.MODULE),
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
						use et_coordinates;
						use geometry;
					begin
						while segment_cursor /= type_net_segments.no_element loop

							-- Make sure junction and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (element (segment_cursor).coordinates_start, element (junction_cursor).coordinates) then

								declare
									type type_line_scratch is new et_schematic.shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> geometry.type_point (element (segment_cursor).coordinates_start), 
										end_point	=> geometry.type_point (element (segment_cursor).coordinates_end));
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
						 & to_string (element (junction_cursor).coordinates, kicad_coordinates.MODULE));
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
						use et_coordinates;
						use geometry;
					begin
						while segment_cursor /= type_net_segments.no_element loop

							-- Make sure junction and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (element (segment_cursor).coordinates_start, element (junction_cursor).coordinates) then

								declare
									type type_line_scratch is new et_schematic.shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> geometry.type_point (element (segment_cursor).coordinates_start), 
										end_point	=> geometry.type_point (element (segment_cursor).coordinates_end));
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
						component	: in et_libraries.type_device_name;
						ports 		: in type_ports.list) is
						port_cursor : type_ports.cursor := ports.first;
						use type_ports;
						use et_coordinates;
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
					& to_string (element (junction_cursor).coordinates, kicad_coordinates.MODULE));
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
						use et_coordinates;
						use geometry;
						no_connection_flag_cursor : type_no_connection_flags.cursor := module.no_connections.first;
					begin -- query_no_connect_flags
						log (text => "quering no_connection_flags ...", level => log_threshold + 4);
						log_indentation_up;
						
						while no_connection_flag_cursor /= type_no_connection_flags.no_element loop

							log (text => to_string (element (no_connection_flag_cursor).coordinates, 
													scope => kicad_coordinates.MODULE),
								level => log_threshold + 4);
						
							-- now we have element (segment_cursor) 
							-- and element (no_connection_flag_cursor) to work with

							-- Make sure no_connection_flag and segment share the same module path and sheet.
							-- It is sufficient to check against the segment start coordinates.
							if same_path_and_sheet (
								element (no_connection_flag_cursor).coordinates,
								element (segment_cursor).coordinates_start) then

								declare
									type type_line_scratch is new et_schematic.shapes.type_line with null record;
									line : type_line_scratch := (
										start_point	=> geometry.type_point (element (segment_cursor).coordinates_start), 
										end_point	=> geometry.type_point (element (segment_cursor).coordinates_end));
								begin
									if on_line (type_point (element (no_connection_flag_cursor).coordinates), line) then
										log (WARNING, "no-connection-flag misplaced on a net at " 
											& to_string (element (no_connection_flag_cursor).coordinates, kicad_coordinates.MODULE));
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
					& to_string (element (strand_cursor).position, scope => kicad_coordinates.MODULE),
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
					component 	: in et_libraries.type_device_name;
					ports 		: in type_ports.list) is
					port_cursor : type_ports.cursor := ports.first;
					use type_ports;
					use et_coordinates;
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
						 & to_string (element (no_connection_flag_cursor).coordinates, kicad_coordinates.MODULE));
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
		use et_coordinates;
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
		use et_coordinates.geometry;
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
		use et_coordinates;
	begin
		--return (to_string (position => label.coordinates, scope => scope));
		return geometry.to_string (point => label.coordinates);
	end to_string;

	function to_string (
		junction	: in type_net_junction;
		scope		: in kicad_coordinates.type_scope) 
		return string is
		-- Returns the position of the given junction as string.
	begin	
		return (to_string (position => junction.coordinates, scope => scope));
	end to_string;

	function length (segment : in type_net_segment_base) 
		return et_coordinates.type_distance is
	-- Returns the length of the given net segment.
		use et_coordinates;
		len : type_distance;
		use et_string_processing;
	begin
		len := distance (segment.coordinates_start, segment.coordinates_end);
		--log (text => "segment length " & et_coordinates.to_string (len) & "mm", level => 3);
		return len;
	end length;
	
	function to_string (
		segment	: in type_net_segment_base'class;
		scope 	: in kicad_coordinates.type_scope := kicad_coordinates.SHEET)
		return string is
	-- Returns the start and end coordinates of the given net segment.
		use kicad_coordinates;
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
				use conventions;
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

				use et_coordinates;
			begin -- query_ports

				while port_cursor /= type_ports_with_reference.no_element loop
			
					-- log reference, port and direction (all in one line)
					log (text => "reference " & et_libraries.to_string (element (port_cursor).reference)
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
								& to_string (element (port_cursor).coordinates, scope => kicad_coordinates.MODULE)
								& et_schematic.show_danger (et_schematic.not_predictable),
								console => true
								);
							raise constraint_error;

							when others		=> null; -- CS: TRISTATE, PASSIVE, POWER_IN
					end case;

					-- Count ports by component category (address real components only)
					--if element (port_cursor).appearance = et_libraries.sch_pcb then
					--if et_libraries."=" (element (port_cursor).appearance, et_libraries.sch_pcb) then
					if element (port_cursor).appearance = SCH_PCB then
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
							& to_string (element (ports.first).coordinates, scope => kicad_coordinates.MODULE));

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
		use et_coordinates;

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

						--if element (port_cursor).reference = port.reference then
						if et_libraries."=" (element (port_cursor).reference, port.reference) then
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
			& et_libraries.to_string (port.reference) & " port " & to_string (port.name) & " ...",
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
		use type_modules;
		use et_coordinates;

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
							component	: in et_libraries.type_device_name;
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
										component	: in et_libraries.type_device_name;
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
										log (text => "probing " & et_libraries.to_string (component) 
											& " port " & to_string (element (port_cursor).name)
											& latin_1.space
											& to_string (position => element (port_cursor).coordinates, scope => kicad_coordinates.MODULE),
											level => log_threshold + 5);

										-- test if port sits on segment
										if port_connected_with_segment (element (port_cursor), element (segment)) then
											log_indentation_up;
										
											log (text => "connected with " & et_libraries.to_string (component) 
												& " port " & to_string (element (port_cursor).name)
												& latin_1.space
												& to_string (position => element (port_cursor).coordinates, scope => kicad_coordinates.MODULE),
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
									scope => kicad_coordinates.MODULE),
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
		reference		: in et_libraries.type_device_name;
		log_threshold	: in et_string_processing.type_log_level)
		return et_devices.type_terminal_count is
	-- Returns the number of terminals of the given component reference.
	-- Requires module_cursor (global variable) to point to the current module.

		use type_modules;
		use et_string_processing;
		use et_coordinates;
		terminals : et_devices.type_terminal_count; -- to be returned

		procedure locate_component_in_schematic (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
			use type_components_schematic;
		
			component_cursor: type_components_schematic.cursor;
			
			library_name	: et_kicad_general.type_device_library_name.bounded_string;
			generic_name	: type_component_generic_name.bounded_string;
			package_variant	: et_devices.type_component_variant_name.bounded_string;

			library_cursor	: type_libraries.cursor;

			procedure locate_component_in_library (
				library_name 	: in et_kicad_general.type_device_library_name.bounded_string;
				components 		: in type_components_library.map) is
				use type_components_library;

				component_cursor : type_components_library.cursor;

				procedure query_variants (
				-- Looks up the list of variants of the component.
					name 		: in type_component_generic_name.bounded_string;
					component 	: in type_component_library) is
					use et_devices.type_component_variants;
					use et_import;

					variant_cursor : et_devices.type_component_variants.cursor;
				begin -- query_variants
					log (text => "locating variant " & et_devices.type_component_variant_name.to_string (package_variant)
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
						terminals := et_kicad_pcb.terminal_count (
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

			type_libraries.query_element (
				position	=> library_cursor,
				process		=> locate_component_in_library'access);

			log_indentation_down;
		end locate_component_in_schematic;
		
	begin -- terminal_count
		log (text => "fetching terminal count of " & et_libraries.to_string (reference) & " ...", level => log_threshold);
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

		use type_modules;
		use et_string_processing;
		use et_coordinates;
		terminal : et_devices.type_terminal; -- to be returned

		procedure locate_component_in_schematic (
			module_name : in type_submodule_name.bounded_string;
			module		: in type_module) is
		
			use type_components_schematic;
			component_cursor: type_components_schematic.cursor;
			
			library_name	: et_kicad_general.type_device_library_name.bounded_string;
			generic_name	: type_component_generic_name.bounded_string;
			package_variant	: et_devices.type_component_variant_name.bounded_string;

			--use type_libraries;
			library_cursor	: type_libraries.cursor;

			procedure locate_component_in_library (
				library_name 	: in et_kicad_general.type_device_library_name.bounded_string;
				components 		: in type_components_library.map) is
				use type_components_library;

				component_cursor : type_components_library.cursor;

				procedure query_variants (
				-- Looks up the list of variants of the component.
					name 		: in type_component_generic_name.bounded_string;
					component 	: in type_component_library) is
					use et_devices.type_component_variants;

					variant_cursor : et_devices.type_component_variants.cursor;

					procedure locate_terminal (
						variant_name 	: in et_devices.type_component_variant_name.bounded_string;
						variant 		: in et_devices.type_component_variant) is
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

					et_devices.type_component_variants.query_element (
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

			type_libraries.query_element (
				position	=> library_cursor,
				process		=> locate_component_in_library'access);

			log_indentation_down;
		end locate_component_in_schematic;
	
	begin -- to_terminal
		log (text => "locating in module " & to_string (module) & " terminal (according to package variant) for " 
			& et_libraries.to_string (port.reference) 
			& " port " & to_string (port.name) & " ...", level => log_threshold);
		log_indentation_up;

		-- Abort if given port is not a real component.
		--if et_libraries."=" (port.appearance, et_libraries.sch_pcb) then -- real component
		if port.appearance = SCH_PCB then -- real component			

			query_element (
				position	=> find (modules, module), -- sets indirectly the cursor to the module
				process		=> locate_component_in_schematic'access);
			
		else -- abort
			log (ERROR, et_libraries.to_string (port.reference) 
				& " is a virtual component and thus has no package !");
			raise constraint_error;
		end if;
			
		log_indentation_down;
		return terminal; 
	end to_terminal;

	function connected_net (
	-- Returns the name of the net connected with the given component and terminal.
		module			: in type_submodule_name.bounded_string; -- nucleo_core
		reference		: in et_libraries.type_device_name;	-- IC45
		terminal		: in et_packages.type_terminal_name.bounded_string; -- E14
		log_threshold	: in et_string_processing.type_log_level)		
		return type_net_name.bounded_string is

		net : type_net_name.bounded_string; -- to be returned

		-- As an intermediate storage place here the module name, the component reference and the port name are stored.
		-- Selector port contains the port name associated with the given terminal name (acc. to. package variant).
		-- Once the port name has been found, this variable is set (see procedure locate_terminal):
		port : type_port_of_module; 

		use et_string_processing;
		use type_modules;
		use et_coordinates;

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
			package_variant	: et_devices.type_component_variant_name.bounded_string;

			library_cursor	: type_libraries.cursor;

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
				
					use et_devices.type_component_variants;
					variant_cursor : et_devices.type_component_variants.cursor;

					procedure locate_terminal (
					-- Locates the given terminal in the package variant.
						variant_name 	: in et_devices.type_component_variant_name.bounded_string;
						variant 		: in et_devices.type_component_variant) is
						use et_devices.type_terminal_port_map;
						use type_port_name;
						terminal_cursor : et_devices.type_terminal_port_map.cursor;
					begin -- locate_terminal
						terminal_cursor := variant.terminal_port_map.find (terminal);
						if terminal_cursor /= et_devices.type_terminal_port_map.no_element then -- given terminal found

							-- set the intermediate variable "port". see declarations above.
							port.module := connected_net.module; -- the name of the given module
							port.reference := reference; -- the given component reference
							port.name := element (terminal_cursor).name; -- the port name
							
							log (text => "port name " & et_symbols.to_string (port.name), level => log_threshold + 4);
						else
							log (ERROR, "terminal " & et_packages.to_string (terminal) & " not found !",
								 console => true);
							raise constraint_error;
						end if;
					end locate_terminal;
					
				begin -- query_variants
					log (text => "locating variant " & et_devices.to_string (package_variant) & " ...", level => log_threshold + 3);
					log_indentation_up;

					variant_cursor := component.variants.find (package_variant);

					-- Locate the given terminal in the variant.
					-- The variant should be found (because the component has been inserted in the library earlier).
					if variant_cursor /= et_devices.type_component_variants.no_element then

						-- locate the given terminal in the package variant
						query_element (
							position 	=> variant_cursor,
							process 	=> locate_terminal'access);

					else
						log (ERROR, "package variant " & et_devices.to_string (key (variant_cursor)) &
							" not found !", console => true);
						raise constraint_error;
					end if;
					log_indentation_down;	
				end query_variants;
				
			begin -- locate_component_in_library
				log (text => "locating generic component " & to_string (generic_name) 
						& " in library " & et_devices.to_string (library_name) & " ...", level => log_threshold + 2);
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
					log (ERROR, "generic model for " & et_libraries.to_string (reference) & " not found !", console => true);
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

				if type_libraries."/=" (library_cursor, type_libraries.no_element) then
					type_libraries.query_element (
						position	=> library_cursor,
						process		=> locate_component_in_library'access);
				else -- library not found -> abort
					log (ERROR, "library " & et_devices.to_string (library_name) & " not found !", console => true);
					raise constraint_error;
				end if;

			else -- component nof found in schematic -> abort
				log (ERROR, "component " & et_libraries.to_string (reference) & " not found !", console => true);
				raise constraint_error;
			end if;
				
			log_indentation_down;
		end query_components;

	begin -- connected_net
		log (text => "locating in module " & to_string (module) & " net connected with " 
			& et_libraries.to_string (reference) & " terminal " & et_packages.to_string (terminal) & " ...", level => log_threshold);
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
		use et_coordinates;
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
								when SCH_PCB =>
									terminal := to_terminal (port, module_name, log_threshold + 3); -- fetch the terminal
									log (text => to_string (port => port) 
										& et_devices.to_string (terminal, show_unit => true, preamble => true));

								when SCH =>
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
		use et_coordinates;
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
					
						--if et_libraries."=" (port.appearance, et_libraries.sch_pcb) then
						if port.appearance = SCH_PCB then
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
-- 		category		: in conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose 		: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold 	: in et_string_processing.type_log_level) is
-- 		
-- 		use et_string_processing;
-- 		use et_coordinates;
-- 		use et_libraries;
-- 		use et_kicad.type_modules;
-- 		use conventions;
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
-- 					if conventions.category (key (component)) = category then -- category must match
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
-- 		category 		: in conventions.type_component_category; -- CONNECTOR, LIGHT_EMMITTING_DIODE, ...
-- 		purpose 		: in et_libraries.type_component_purpose.bounded_string; -- PWR_IN, SYS_FAIL, ...
-- 		log_threshold 	: in et_string_processing.type_log_level)
-- 		return natural is
-- 
-- 		occurences : natural := 0; -- to be returned
-- 
-- 		use et_coordinates;
-- 		use et_kicad.type_modules;
-- 		use et_libraries;
-- 		use conventions;
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
-- 					if conventions.category (key (component)) = category then -- category must match
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
-- 		use et_coordinates;
-- 		
-- 		procedure count_components (
-- 			name	: in type_submodule_name.bounded_string;
-- 			module	: in type_module) is
-- 
-- 			use type_components_schematic;		
-- 			component : type_components_schematic.cursor := module.components.first;
-- 			
-- 			use conventions;
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
-- 				component	: in et_libraries.type_device_name;
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
		use et_coordinates;
		use et_libraries;
		use et_text;
	begin
		log (text => "text note" & to_string (
			position => note.position, scope => kicad_coordinates.XY), level => log_threshold);

		log_indentation_up;

		-- content
		if type_text_content.length (note.content) > 0 then
			log (text => "content '" & to_string (note.content) & "'", level => log_threshold);
		else
			log (text => et_string_processing.message_warning & "no content !", level => log_threshold); 
		end if;
	
		if log_level >= log_threshold + 1 then
			
			-- size
			log (text => "size" & pac_text.to_string (note.size));

			-- style
			log (text => "style " & to_lower (to_string (note.style)));

			-- line width
			log (text => "line width" & pac_text.to_string (note.line_width));

			-- rotation
			log (text => "rotation" & geometry.to_string (note.rotation));

			-- visible
			--log (text => "visible " & to_lower (et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log (text => et_text.to_string (note.alignment));
		end if;
		
		log_indentation_down;
	end write_note_properties;

	
end et_kicad;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
