------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD SCHEMATIC COORDINATES                         --
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

with ada.strings;				use ada.strings;
with ada.strings.unbounded;

with ada.exceptions;

with ada.numerics.generic_elementary_functions;
with et_string_processing;

package body et_kicad_coordinates is
	
	function to_string (schematic : in type_schematic_file_name.bounded_string) return string is begin
		return type_schematic_file_name.to_string (schematic);
	end;

	function to_schematic_file_name (file : in string) return type_schematic_file_name.bounded_string is begin
		return type_schematic_file_name.to_bounded_string (file);
	end;

	procedure check_submodule_name_characters (
	-- Checks for forbidden characters in submodule name.
		name		: in type_submodule_name.bounded_string;
		characters	: in character_set := submodule_name_characters) is
		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		-- Test given submodule name and get position of possible invalid characters.
		invalid_character_position := index (
			source	=> name,
			set		=> characters,
			test	=> outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (WARNING, "invalid character in submodule name '" 
				& to_string (name) & "' at position" & natural'image (invalid_character_position) & " !");
		end if;
	end check_submodule_name_characters;

	
	function to_string (
		path 		: in type_path_to_submodule.list;
		top_module 	: in boolean := true) return string is
	-- Returns the given path as string with hierarchy_separator.
	-- If top_module = false, the name of the top module is omitted.
	
		use type_path_to_submodule;
		use ada.strings.unbounded;
	
		submodule : type_path_to_submodule.cursor := path.first;
		result : unbounded_string;
	begin
		-- If top_module is false, advance cursor right to next module.
		if not top_module then
			next (submodule);
		end if;

		if is_empty (path) then
			result := to_unbounded_string (hierarchy_separator);
		else
			-- Loop through list of submodules and collect their names in "result".
			while submodule /= type_path_to_submodule.no_element loop
				result := result & hierarchy_separator 
					& to_unbounded_string (to_string (element (submodule)));
-- 					& hierarchy_separator;
				next (submodule);
			end loop;
		end if;
-- 		if result = hierarchy_separator then
-- 			result := result & " (top module)";
-- 		end if;
		
		--return to_string ("location " & result);
		return to_string (result);
	end to_string;

	function path (position : in type_position) return type_path_to_submodule.list is begin
		return position.path;
	end;

	procedure set_path (
		position	: in out type_position;
		path		: in type_path_to_submodule.list) is begin
	-- Sets the path in given position.
		position.path := path;
	end;
	
-- 	procedure check_submodule_name_length (name : in string) is
-- 	-- Checks if the given submodule name is not longer than allowed.
-- 		use et_string_processing;
-- 	begin
-- 		if name'length > submodule_name_length_max then
-- 			log_indentation_reset;
-- 			log (message_error & "max. number of characters for module name is" 
-- 				 & positive'image (submodule_name_length_max) & " !",
-- 				console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end check_submodule_name_length;
-- 
-- 	procedure check_submodule_name_characters (
-- 	-- Checks for forbidden characters in submodule name.
-- 		name : in type_submodule_name.bounded_string;
-- 		characters : in character_set := submodule_name_characters) is
-- 		use et_string_processing;
-- 		invalid_character_position : natural := 0;
-- 	begin
-- 		-- Test given submodule name and get position of possible invalid characters.
-- 		invalid_character_position := index (
-- 			source => name,
-- 			set => characters,
-- 			test => outside);
-- 
-- 		-- Evaluate position of invalid character.
-- 		if invalid_character_position > 0 then
-- 			log_indentation_reset;
-- 			log (message_error & "invalid character in submodule name '" 
-- 				& to_string (name) & "' at position" & natural'image (invalid_character_position),
-- 				console => true);
-- 			raise constraint_error;
-- 		end if;
-- 	end check_submodule_name_characters;
	
	function to_string (submodule : in type_submodule_name.bounded_string) return string is 
	-- Returns the given submodule name as string.
	begin
		return type_submodule_name.to_string (submodule);
	end;

	function to_submodule_name (submodule : in string) return type_submodule_name.bounded_string is
	-- Converts a string to type_submodule_name.
	begin
		return type_submodule_name.to_bounded_string (submodule);
	end;
	
	
-- 	function to_string (
-- 		path : in type_path_to_submodule.list;
-- 		top_module : in boolean := true) return string is
-- 	-- Returns the given path as string with hierarchy_separator.
-- 	-- If top_module = false, the name of the top module is omitted.
-- 	
-- 		use type_path_to_submodule;
-- 		use ada.strings.unbounded;
-- 	
-- 		submodule : type_path_to_submodule.cursor := path.first;
-- 		result : unbounded_string;
-- 	begin
-- 		-- If top_module is false, advance cursor right to next module.
-- 		if not top_module then
-- 			next (submodule);
-- 		end if;
-- 
-- 		if is_empty (path) then
-- 			result := to_unbounded_string (hierarchy_separator);
-- 		else
-- 			-- Loop through list of submodules and collect their names in "result".
-- 			while submodule /= type_path_to_submodule.no_element loop
-- 				result := result & hierarchy_separator 
-- 					& to_unbounded_string (to_string (element (submodule)));
-- -- 					& hierarchy_separator;
-- 				next (submodule);
-- 			end loop;
-- 		end if;
-- -- 		if result = hierarchy_separator then
-- -- 			result := result & " (top module)";
-- -- 		end if;
-- 		
-- 		--return to_string ("location " & result);
-- 		return to_string (result);
-- 	end to_string;

	function to_string (
	-- Returns the given position as string. Scope specifies how much position is to
	-- be displayed. See specification of type_scope.
		position	: in type_position;
		scope		: in type_scope := SHEET)
		return string is

		coordinates_preamble_xy : constant string := " pos "
			& "(x"
			& axis_separator
			& "y) ";
		
		coordinates_preamble_sheet : constant string := " pos "
			& "(sheet"
			& axis_separator
			& "x"
			& axis_separator
			& "y) ";

		coordinates_preamble_module : constant string := " pos "
			& "(path"
			& axis_separator
			& "sheet"
			& axis_separator
			& "x"
			& axis_separator
			& "y) ";
		
		use et_string_processing;
	begin
		case scope is
			when MODULE =>
				return coordinates_preamble_module
					& to_string (position.path) & latin_1.space & hierarchy_separator & latin_1.space
					& to_sheet (position.sheet_number) 
					& latin_1.space & axis_separator & latin_1.space
					& to_string (x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (y (position));
				
			when SHEET =>
				return coordinates_preamble_sheet
					& to_sheet (position.sheet_number) 
					& latin_1.space & axis_separator & latin_1.space
					& to_string (x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (y (position));

			when XY =>
				return coordinates_preamble_xy
					& to_string (x (position))
					& latin_1.space & axis_separator & latin_1.space
					& to_string (y (position));

		end case;
	end to_string;

	function sheet (position : in type_position) return type_sheet is begin
		return position.sheet_number;
	end sheet;

	function same_path_and_sheet (left, right : in type_position) return boolean is
	-- Returns true if the given coordinates have same path and sheet.
		same : boolean := false;
		use type_path_to_submodule;
	begin 
		-- We compare path and sheet. x/y are ignored
		if path (left) = path (right) then
			if sheet (left) = sheet (right) then
				same := true;
			end if;
		end if;

		return same;
	end same_path_and_sheet;
	
	procedure set_sheet (position : in out type_position; sheet : in type_sheet) is
	-- Sets the sheet number in given position.
	begin
		position.sheet_number := sheet;
	end set_sheet;

	
end et_kicad_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
